--------
title: 'Parser Combinators'
subtitle: 'Day 2: Passport Philosophy - Advent of Code 2020'
--------

> module Day2 where
> --------------------------------------------------------------------------------
> import Control.Monad.State.Strict
> import Control.Monad.Except
> import Data.Text hiding (length, filter, null, empty, index)
> import qualified Data.Text as Tx
> --------------------------------------------------------------------------------
> import Control.Applicative
> import Control.Monad
> import Data.Kind
> import Data.Char
> import Data.Ix
> --------------------------------------------------------------------------------
> import Parser

 <!--imports-->

Day 2
===

Challenge 1
-------

This challenge involves parsing a list of password rules and passwords, and validating the passwords
according to the given rules, so `3-6 s: somepassword` means somepassword is valid if it contains 3-6 of the character s.

I feel like building a parser combinator interface myself,
this lives in the [Parser](haskell/Parser.html) module.

As a regular expression, the language we need to parse looks something like [0-9]*-[0-9]* [a-z]: [a-z]*,
then we can have an extra pass over the parser result to actually validate the password.

First, the datatype we're parsing the first part into.

> data Validation = Validation Int Int Char
>   deriving (Show)
> data ValidatedPassword = ValidPassword {validationScheme :: Validation, validPassword :: Either Text Text}
>   deriving (Show)

And a smart constructor that runs the validation.

> validatePassword :: Validation -> Text -> ValidatedPassword
> validatePassword vl@(Validation from to ch) ps
>   | (inRange (from, to) (Tx.length $ Tx.filter (==ch) ps)) = ValidPassword vl $ Right ps
>   | otherwise = ValidPassword vl $ Left ps

The parsers themselves are pretty simple.

> validation :: Parser Validation
> validation =
>   Validation
>     <$> (parseNum @(Int))
>     <*> ((skipUntilP isDigit) *> (parseNum @(Int)))
>     <*> ((skipUntilP isLower) *> (anyChar))

> validatedPassword :: Parser ValidatedPassword
> validatedPassword =
>   validatePassword
>     <$> validation
>     <*> ((skipUntilP isAlpha) *> (whileP isAlpha))


> run1 :: Text -> Int
> run1 =
>   (execParser (many $ (validatedPassword <* skipWhileP isSpace)))
>     >>> (fromMaybe [])
>     >>> (foldMap (validPassword >>> either (const $ Sum 0) (const $ Sum 1)))
>     >>> getSum
>

Challenge 2
------

The second challenge is a variation on the first, with a different meaning of the password validation system.
`4-9 b` now means that the character b must appear precisely once between positions 4 and 9 (1-indexed) in the string.

We can reuse the same parser definition, changing up the validatePassword function.


> ixOne :: Int -> Text -> Maybe (Char)
> ixOne ix tx = (unpack tx) !!? (index (1, Tx.length tx) ix)

> validatePassword2 :: Validation -> Text -> ValidatedPassword
> validatePassword2 vl@(Validation ixa ixb ch) ps
>   | (maybe (False) (==ch) (ixOne ixa ps)) `xor` (maybe (False) (==ch) (ixOne ixb ps)) = ValidPassword vl $ Right ps
>   | otherwise = ValidPassword vl $ Left ps
>
> validatedPassword2 :: Parser ValidatedPassword
> validatedPassword2 =
>   validatePassword2
>     <$> validation
>     <*> ((skipUntilP isAlpha) *> (whileP isAlpha))

> run2 :: Text -> Int
> run2 =
>   (execParser (many $ (validatedPassword2 <* skipWhileP isSpace)))
>     >>> (fromMaybe [])
>     >>> (foldMap (validPassword >>> either (const $ Sum 0) (const $ Sum 1)))
>     >>> getSum


