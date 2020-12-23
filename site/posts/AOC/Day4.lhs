-----------
title: 'Overengineering Day 4 with type-level programming'
subtitle: 'Day 4: Passport Processing - Advent of Code 2020'
published: 'Wednesday 23 Dec 2020 05:31:10 GMT'
sort: 4
-----------

> {-# LANGUAGE UndecidableInstances #-}
> module Day4 (run1, run2) where
> --------------------------------------------------------------------------------
> import Control.Monad.Except
> --------------------------------------------------------------------------------
> --import Relude as Prelude hiding (show)
> --import Prelude hiding (undefined, head, span)
> import Relude.Extra.Type
> import Relude.Extra.Foldable1
> import Relude.Extra.Newtype
> import qualified Data.Text as Tx
> import Data.Text.Read
> --------------------------------------------------------------------------------
> import Data.Char
> import Data.Bifunctor
> import Data.Kind
> import Data.Functor.Identity
> import GHC.TypeLits as TypeLits
> import GHC.Records
> import Control.Applicative
> import qualified Control.Arrow as Ar
> import Control.Arrow hiding (first, second, (+++))
> import Data.List.NonEmpty (some1)
> import qualified Data.List.NonEmpty as NE
> import Data.Ix (inRange)
> import Text.Show
> import Text.Read
> --------------------------------------------------------------------------------
> import Parser
> import RegexInTypes
> --------------------------------------------------------------------------------

 <!--imports-->

Day4
====

Challenge 1
-----------

We have to parse some passport data, and check if it's valid, we can reuse the parser from Day 2.
For fun, I decided to try and build a parser using type-level programming to generate a parser from a type-level regex...
this was a *lot* tricker than expected.
I've gained a new appreciation for how much utility Haskell is lacking in the type-level department
(atleast out of the box, I'm sure there's some abstract nonsense library out there that would've come in handy).

The jist of how it works is that a type-level regular expression can be 'compiled' using a typeclass to build a parser combinator expression,
which returns a Heterogeneous List of named capture groups from the expression.

The implementation of the type-level stuff lives in the [RegexInTypes](/haskell/RegexInTypes.html) module.
Be warned, it's very ugly! And I have yet to write it up properly!

Here are the passport field types, which will be used as tags to construct the parser.

> data PassportField
>   = BirthYearField
>   | IssueYearField
>   | ExpirationYearField
>   | HeightField
>   | HairColourField
>   | EyeColourField
>   | PassportIdField
>   | CountryIdField

Each field type is given an underlying representation type, these aren't defined until we need them in part 2.

> type family PassportFieldRepr (field :: PassportField) :: Type where
>   PassportFieldRepr 'BirthYearField      = BirthYear
>   PassportFieldRepr 'IssueYearField      = IssueYear
>   PassportFieldRepr 'ExpirationYearField = ExpirationYear
>   PassportFieldRepr 'HeightField         = Height
>   PassportFieldRepr 'HairColourField     = HairColour
>   PassportFieldRepr 'EyeColourField      = EyeColour
>   PassportFieldRepr 'PassportIdField     = PassportId
>   PassportFieldRepr 'CountryIdField      = CountryId

A field is specified by a name, a type, and whether it is required.

> data Field ty = Field IsRequired Symbol ty
> data IsRequired = Required | Optional

Optional fields can be represented with a Maybe.

> type family MaybeRepr (req :: IsRequired) (ty :: Type) :: Type where
>   MaybeRepr 'Required ty = ty
>   MaybeRepr 'Optional ty = Maybe ty

The layout of a fully parsed Passport:

> type PassportFields =
>   [ 'Field 'Required "BirthYear"      'BirthYearField
>   , 'Field 'Required "IssueYear"      'IssueYearField
>   , 'Field 'Required "ExpirationYear" 'ExpirationYearField
>   , 'Field 'Required "Height"         'HeightField
>   , 'Field 'Required "HairColour"     'HairColourField
>   , 'Field 'Required "EyeColour"      'EyeColourField
>   , 'Field 'Required "PassportId"     'PassportIdField
>   , 'Field 'Optional "CountryId"      'CountryIdField ]

Using a type family to map field representations given a list of fields.

> type family PassportRepr (xs :: [Field PassportField]) :: [Type] where
>   PassportRepr (('Field req name ty) ': xs) = (name := (MaybeRepr req (PassportFieldRepr ty))) ': (PassportRepr xs)
>   PassportRepr '[] = '[]


Now building the actual regex, most of the work for that is in another module (it got really long!).

There may be malformed fields, so let's add an extra capture for each field to handle them.

> type family Malformed name where
>   Malformed name = ((AppendSymbol "malformed_" name) ':? (PMany (Not WSpace))) ':++ ((P WSpace) ':|| (End))

Each Field has a Regex to parse it and capture out the necessary parts.

> type family ParseField (req :: IsRequired) (name :: Symbol) (ty :: k) :: ParseExp


> type instance ParseField 'Optional name ty= (ParseField 'Required name ty)

> type instance ParseField 'Required name 'BirthYearField =
>   ('S "byr:") :++
>     (((name ':? (PMany ("0" ':- "9"))) ':++ ((P WSpace) ':|| (End))) ':|| Malformed name)

> type instance ParseField 'Required name 'IssueYearField =
>   ('S "iyr:") ':++
>     (((name ':? (PMany ("0" ':- "9"))) ':++ ((P WSpace) ':|| (End))) :|| Malformed name)

> type instance ParseField 'Required name 'ExpirationYearField =
>   ('S "eyr:") ':++
>     (((name ':? (PMany ("0" ':- "9"))) ':++ ((P WSpace) ':|| (End))) ':|| Malformed name)

> type instance ParseField 'Required name 'HeightField =
>   ('S "hgt:") ':++
>       ( ((name ':? ( (PMany ("0" ':- "9")) ':++ ((S "cm") ':|| (S "in"))
>             )) ':++ ((P WSpace) ':|| (End)))
>         ':||
>         (Malformed name)
>       )

> type HexDigit = (P (("0" ':- "9") `Or` ("a" ':- "f")))
> type family NTimes (n :: Nat) (a :: ParseExp) :: ParseExp where
>   NTimes 0 a = Empty
>   NTimes 1 a = a
>   NTimes n a = (NTimes ((n `Div` 2) + (n `Mod` 2)) a) ':++ (NTimes (n `Div` 2) a)
>
> type instance ParseField 'Required name 'HairColourField =
>   ('S "hcl:")
>     ':++ (((name ':? ((S "#") ':++ (
>      (NTimes 6 HexDigit)
>     ))) ':++ ((P WSpace) ':|| (End)))':|| Malformed name)

> type family AnyOf (xs :: [ParseExp]) :: ParseExp where
>   AnyOf (x ': '[]) = x
>   AnyOf xs = (AnyOf (Drop ((Length xs) `Div` 2) xs)) ':|| (AnyOf (Take ((Length xs) `Div` 2) xs))

> type instance ParseField 'Required name 'EyeColourField  =
>   ('S "ecl:") ':++
>     (((name ':? (
>      AnyOf [(S "amb") ,(S "blu") , (S "brn") , (S "gry") , (S "grn") , (S "hzl") , (S "oth")]
>     )) ':++ ((P WSpace) ':|| (End)))  ':|| Malformed name)

> type instance ParseField 'Required name 'PassportIdField =
>   ('S "pid:")  ':++
>     (((name ':? (
>      NTimes 9 (P ("0" ':- "9"))
>     )) ':++ ((P WSpace) ':|| (End))) ':|| Malformed name)

> type instance ParseField 'Required name 'CountryIdField
>   = ('S "cid:")  ':++ (((name ':? (PMany ("0" ':- "9"))) ':++ ((P WSpace) ':|| (End))) ':|| Malformed name)

Gluing it all together with a type family.

> type family ParseFields (fields :: [Field PassportField]) :: [ParseExp] where
>   ParseFields (('Field req name ty) ': xs)  = ((ParseField req name ty) ': (ParseFields xs))
>   ParseFields '[] = '[]

The compilation times were getting ridiculous at this stage! mostly in generating the actual parser from the regular expressions, however
the large type-family generated type dont help either.
Let's just paste the fully evaluated type-family results (from ghci's `:kind!` command to view the kind and type of a type expression).


> type PassportExp' = Many (AnyOf(ParseFields PassportFields))
> type PassportExp =
>   'Many
>   (((('S "cid:"
>       ':++ ((("CountryId" ':? 'PMany ("0" ':- "9"))
>              ':++ ('P 'WSpace ':|| 'End))
>             ':|| (("malformed_CountryId" ':? 'PMany ('Not 'WSpace))
>                   ':++ ('P 'WSpace ':|| 'End))))
>      ':|| ('S "pid:"
>            ':++ ((("PassportId"
>                    ':? (((('P ("0" ':- "9") ':++ 'P ("0" ':- "9"))
>                           ':++ 'P ("0" ':- "9"))
>                          ':++ ('P ("0" ':- "9") ':++ 'P ("0" ':- "9")))
>                         ':++ (('P ("0" ':- "9") ':++ 'P ("0" ':- "9"))
>                               ':++ ('P ("0" ':- "9") ':++ 'P ("0" ':- "9")))))
>                   ':++ ('P 'WSpace ':|| 'End))
>                  ':|| (("malformed_PassportId" ':? 'PMany ('Not 'WSpace))
>                        ':++ ('P 'WSpace ':|| 'End)))))
>     ':|| (('S "ecl:"
>            ':++ ((("EyeColour"
>                    ':? ((('S "oth" ':|| 'S "hzl") ':|| ('S "grn" ':|| 'S "gry"))
>                         ':|| (('S "brn" ':|| 'S "blu") ':|| 'S "amb")))
>                   ':++ ('P 'WSpace ':|| 'End))
>                  ':|| (("malformed_EyeColour" ':? 'PMany ('Not 'WSpace))
>                        ':++ ('P 'WSpace ':|| 'End))))
>           ':|| ('S "hcl:"
>                 ':++ ((("HairColour"
>                         ':? ('S "#"
>                              ':++ ((('P ('Or ("0" ':- "9") ("a" ':- "f"))
>                                      ':++ 'P ('Or ("0" ':- "9") ("a" ':- "f")))
>                                     ':++ 'P ('Or ("0" ':- "9") ("a" ':- "f")))
>                                    ':++ (('P ('Or ("0" ':- "9") ("a" ':- "f"))
>                                           ':++ 'P ('Or ("0" ':- "9") ("a" ':- "f")))
>                                          ':++ 'P ('Or ("0" ':- "9") ("a" ':- "f"))))))
>                        ':++ ('P 'WSpace ':|| 'End))
>                       ':|| (("malformed_HairColour" ':? 'PMany ('Not 'WSpace))
>                             ':++ ('P 'WSpace ':|| 'End))))))
>    ':|| ((('S "hgt:"
>            ':++ ((("Height"
>                    ':? ('PMany ("0" ':- "9") ':++ ('S "cm" ':|| 'S "in")))
>                   ':++ ('P 'WSpace ':|| 'End))
>                  ':|| (("malformed_Height" ':? 'PMany ('Not 'WSpace))
>                        ':++ ('P 'WSpace ':|| 'End))))
>           ':|| ('S "eyr:"
>                 ':++ ((("ExpirationYear" ':? 'PMany ("0" ':- "9"))
>                        ':++ ('P 'WSpace ':|| 'End))
>                       ':|| (("malformed_ExpirationYear" ':? 'PMany ('Not 'WSpace))
>                             ':++ ('P 'WSpace ':|| 'End)))))
>          ':|| (('S "iyr:"
>                 ':++ ((("IssueYear" ':? 'PMany ("0" ':- "9"))
>                        ':++ ('P 'WSpace ':|| 'End))
>                       ':|| (("malformed_IssueYear" ':? 'PMany ('Not 'WSpace))
>                             ':++ ('P 'WSpace ':|| 'End))))
>                ':|| ('S "byr:"
>                      ':++ ((("BirthYear" ':? 'PMany ("0" ':- "9"))
>                             ':++ ('P 'WSpace ':|| 'End))
>                            ':|| (("malformed_BirthYear" ':? 'PMany ('Not 'WSpace))
>                                  ':++ ('P 'WSpace ':|| 'End)))))))

And the resulting captures from the expression (the Caps type-family is from the [RegexInTypes](/haskell/RegexInTypes.html) module):

> type PassportCaps' = Caps PassportExp'
> type PassportCaps =
>   '[ "CountryId" := [Text], "malformed_CountryId" := [Text],
>      "PassportId" := [Text], "malformed_PassportId" := [Text],
>      "EyeColour" := [Text], "malformed_EyeColour" := [Text],
>      "HairColour" := [Text], "malformed_HairColour" := [Text],
>      "Height" := [Text], "malformed_Height" := [Text],
>      "ExpirationYear" := [Text], "malformed_ExpirationYear" := [Text],
>      "IssueYear" := [Text], "malformed_IssueYear" := [Text],
>      "BirthYear" := [Text], "malformed_BirthYear" := [Text]]

The validation for part 1 is simple, we just leave malformed Text as is:

> type family ValidPassportRepr (xs :: [Field PassportField]) :: [Type] where
>   ValidPassportRepr (('Field req name ty)': xs) = (MaybeRepr req Text) ': (ValidPassportRepr xs)
>   ValidPassportRepr '[] = '[]

> type ValidPassport1' = ValidPassportRepr PassportFields
> type ValidPassport1 = '[Text, Text, Text, Text, Text, Text, Text, Maybe(Text)]

The `HasField` typeclass is implemented on our HList type (in the [RegexInTypes](/haskell/RegexInTypes.html) module) to provide virtual field accesors.
We can build a Passport by extracting fields from the regex captures.

> getFieldOrMalformed :: forall sym mal xs.
>   ( HasField sym (HList xs) [Text]
>   , HasField mal (HList xs) [Text]
>   , (AppendSymbol "malformed_" sym) ~ mal
>   ) => (HList xs) -> Maybe Text
> getFieldOrMalformed xs = (viaNonEmpty head (getField @(sym) xs)) <|> (viaNonEmpty head (getField @(mal) xs))

> validatePassport :: HList (PassportCaps') -> Maybe (HList ValidPassport1)
> validatePassport xs = do
>   a <- (getFieldOrMalformed @"BirthYear" xs)
>   b <- (getFieldOrMalformed @"IssueYear" xs)
>   c <- (getFieldOrMalformed @"ExpirationYear" xs)
>   d <- (getFieldOrMalformed @"Height" xs)
>   e <- (getFieldOrMalformed @"HairColour" xs)
>   f <- (getFieldOrMalformed @"EyeColour" xs)
>   g <- (getFieldOrMalformed @"PassportId" xs)
>   return (a ::: b ::: c ::: d ::: e ::: f ::: g ::: (getFieldOrMalformed @"CountryId" xs) ::: HNil)
>

Now *finally* gluing everything together!

> run1 :: Text -> Int
> run1 =
>   (Tx.splitOn "\n\n")
>      >>> (fmap $ execParser (runExp @(PassportExp')))
>      >>> catMaybes
>      >>> (fmap $ snd)
>      >>> (fmap validatePassport)
>      >>> catMaybes
>      >>> length


Challenge 2
---------

This time there are additional rules for passport fields, and malformed fields aren't accepted.
We can validate each field with an additional parsing step after the regex captures.

> newtype BirthYear = BirthYear (Int)
>   deriving stock (Show)
>   deriving (ValidateItem) via (ValidateRange 1920 2002 Int)

> newtype IssueYear = IssueYear (Int)
>   deriving stock (Show)
>   deriving (ValidateItem) via (ValidateRange 2010 2020 Int)

> newtype ExpirationYear = ExpirationYear (Int)
>   deriving stock (Show)
>   deriving (ValidateItem) via (ValidateRange 2020 2030 Int)

> newtype HeightCm  = HeightCm Int
>   deriving stock Show
>   deriving (ValidateItem) via (ValidateRange 150 193 Int)

> newtype HeightInch = HeightInch Int
>   deriving stock Show
>   deriving (ValidateItem) via (ValidateRange 59 76 Int)

> newtype Height = Height (Either HeightCm HeightInch)
>   deriving stock Show
>   deriving (ValidateItem) via (ValidateEitherSuffix "cm" "in" HeightCm HeightInch)

> newtype HairColour = HairColour Text
>   deriving stock Show
>   deriving ValidateItem via ValidText

> data EyeColour = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
>   deriving stock (Show, Read)
>   deriving ValidateItem via (ValidateRead (ReadCase EyeColour))

> newtype PassportId = PassportId Int
>   deriving stock Show
>   deriving ValidateItem via (ValidateRead Int)
>
> newtype CountryId = CountryId Int
>   deriving stock Show
>   deriving ValidateItem via (ValidateRead Int)

> type ValidPassport2' = PassportRepr PassportFields
> type ValidPassport2 =
>   '["BirthYear" := BirthYear, "IssueYear" := IssueYear,
>   "ExpirationYear" := ExpirationYear, "Height" := Height,
>   "HairColour" := HairColour, "EyeColour" := EyeColour,
>   "PassportId" := PassportId, "CountryId" := Maybe CountryId]

> class ValidateItem (a :: Type) where
>   validateItem :: Parser a

Validating using Read to parse from text, then Enum to check a range:

> newtype ValidateRange (min :: Nat) (max :: Nat) (a :: Type) = ValidateRange a
>   deriving (Read, Enum, Eq, Ord) via a

> instance
>   ( Read (ValidateRange from to a), Enum (ValidateRange from to a), Ord (ValidateRange from to a)
>   , KnownNat from, KnownNat to
>   ) => ValidateItem (ValidateRange from to a) where
>   validateItem = do
>      val <- parseRead
>      guard ((val >= fromval) && (val <= toval))
>      return val
>     where
>       fromval :: (ValidateRange from to a)
>       fromval = toEnum $ fromInteger @(Int) $ TypeLits.natVal (Proxy @(from))
>
>       toval :: (ValidateRange from to a)
>       toval = toEnum $ fromInteger @(Int) $ TypeLits.natVal (Proxy @(to))


And via Text (in the case that the regex alone is enough to validate a field):

> newtype ValidText = ValidText Text
> instance ValidateItem ValidText where
>   validateItem = ValidText <$> consumeAll

Validating using a Read instance alone:

> newtype ValidateRead a = ValidateRead a
>   deriving (Read) via a

> instance (Read (ValidateRead a)) => ValidateItem (ValidateRead a) where
>   validateItem = parseRead

Make a read instance, fixing up the default casing rules for Haskell data Constructors.

> newtype ReadCase a = ReadCase a
> instance (Read a) => Read (ReadCase a) where
>   readsPrec n "" = []
>   readsPrec n (ch : rest) = fmap (first ReadCase) (readsPrec @(a) n ((toUpper ch) : rest))

Validate Either based on suffix of string.

> newtype ValidateEitherSuffix (asuff :: Symbol) (bsuff :: Symbol) a b = ValidateEitherSuffix (Either a b)
> instance
>   ((ValidateItem a), (ValidateItem b), KnownSymbol asuff, KnownSymbol bsuff)
>   => ValidateItem (ValidateEitherSuffix asuff bsuff a b) where
>   validateItem =
>     ((matchSuffix aSuffix) *> (fmap (ValidateEitherSuffix . Left) (validateItem @(a))))
>     <|>
>     ((matchSuffix bSuffix) *> (fmap (ValidateEitherSuffix . Right) (validateItem @(b))))
>     where
>       aSuffix :: Text
>       aSuffix = toText $ symbolVal (Proxy @(asuff))
>
>       bSuffix :: Text
>       bSuffix = toText $ symbolVal (Proxy @(bsuff))

HLists are a pain! I tried to write a Traversable like thing for this, but couldn't :(

> validatePassport2 :: HList (PassportCaps) -> Maybe (HList ValidPassport2')
> validatePassport2 xs = do
>   birthYear <- getValidatedField @("BirthYear") @(BirthYear) xs
>   issueYear <- getValidatedField @("IssueYear") @(IssueYear) xs
>   expirationYear <- getValidatedField @("ExpirationYear") @(ExpirationYear) xs
>   height <- getValidatedField @("Height") @(Height) xs
>   hairColour <- getValidatedField @("HairColour") @(HairColour) xs
>   eyeColour <- getValidatedField @("EyeColour") @(EyeColour) xs
>   passportId <- getValidatedField @("PassportId") @(PassportId) xs
>   countryId <- (fmap (fmap Just) (getValidatedField @("CountryId") @(CountryId) xs)) <|> (Just (Named @"CountryId" Nothing))
>   return (birthYear ::: issueYear ::: expirationYear ::: height ::: hairColour ::: eyeColour ::: passportId ::: countryId ::: HNil)
>
> getValidatedField :: forall sym field xs.
>   ( HasField sym (HList xs) [Text], ValidateItem field) => (HList xs) -> Maybe (sym := field)
> getValidatedField xs = fmap (Named @sym) ((viaNonEmpty head (getField @(sym) xs)) >>= (execParser (validateItem @(field))))

> run2 :: Text -> Int
> run2 =
>   (Tx.splitOn "\n\n")
>      >>> (fmap $ execParser (runExp @(PassportExp)))
>      >>> catMaybes
>      >>> (fmap $ snd)
>      >>> (fmap validatePassport2)
>      >>> catMaybes
>      >>> length

> sample = Tx.intercalate "\n"
>   ["eyr:2029 iyr:2013"
>   ,"hcl:#ceb3a1 byr:1939 ecl:blu"
>   ,"hgt:163cm"
>   ,"pid:660456119\n\n"]



