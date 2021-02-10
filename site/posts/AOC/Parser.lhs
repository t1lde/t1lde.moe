------------
published: 'Wednesday 23 Dec 2020 05:31:10 GMT'
title: Parser.lhs
subtitle: A Parser Combinator Library
------------

> module Parser where
> --------------------------------------------------------------------------------
> import Control.Monad.State.Strict
> import Control.Monad.Except
> import Data.Text hiding (length, filter, null, empty)
> import qualified Data.Text as Tx
> --------------------------------------------------------------------------------
> -- import Relude as Prelude
> --------------------------------------------------------------------------------
> import Data.Kind
> import Data.Char
> --------------------------------------------------------------------------------

 <!--imports-->


This module is a lil' parser combinator I whipped up for Advent of Code 2020 challenges.
The I didn't try very hard to make it complete, correct or fast, I just made it do what I needed.

ParserT
-------

The ParserT type is equivalent to (and derives instances from) StateT Text.

> newtype ParserT (m :: Type -> Type) a
>   = ParserT { runParser :: Text -> m (a, Text) }
>   deriving (Monad, Functor, Applicative,  MonadState Text)
>     via (StateT Text m)
>   deriving MonadTrans
>     via (StateT Text)
>
> deriving via (StateT Text m) instance (Alternative m, Monad m, MonadPlus m) => Alternative (ParserT m)
> deriving via (StateT Text m) instance (MonadPlus m, Alternative m) => MonadPlus (ParserT m)

Runs a parser over a Text input, ignoring the State result.

> execParser :: (Functor m) => (ParserT m a) -> Text -> m a
> execParser p = (runParser p) >>> (fmap fst)
>


Basic Parser with underlying Maybe monad

> type Parser = ParserT Maybe


Base Parsers
-----------------

Runs a parser over the output of another parser, prepending the output to the input first
to not advance the state

> overOutput :: (MonadPlus m) => Text -> ParserT m b -> ParserT m b
> overOutput tx m = do
>   modify (tx<>)
>   m

Consumes and returns the whole text

> consumeAll :: (MonadPlus m) => ParserT m Text
> consumeAll = get <* (put "")

Lifts a Read parser to ParserT

> parseTo :: (Read a, MonadPlus m) => ParserT m Text -> ParserT m a
> parseTo parser = parser >>= (unpack >>> readMaybe >>> (maybe empty return))

Lifts a Read parser over the whole string

> parseRead :: (Read a, MonadPlus m) => ParserT m a
> parseRead =
>   ((gets toString) <* (put ""))
>     >>= (readMaybe >>> (maybe empty return))

Gets a single char at the front

> anyChar :: (MonadPlus m) => ParserT m Char
> anyChar =
>   (gets Tx.uncons)
>     >>= (maybe empty return)
>     >>= (\(ch, next) -> (put next) *> (return ch))
>

Matches a single char at the front on a given predicate

> charP :: (MonadPlus m) => (Char -> Bool) -> ParserT m Char
> charP p =
>   (gets Tx.uncons)
>     >>= (maybe empty return)
>     >>= (\(ch, next) -> (guard (p ch)) *> (put next) *> (return ch) )

Matches Text at front

> matchText :: (MonadPlus m) => Text -> ParserT m Text
> matchText tx =
>   (gets $ Tx.stripPrefix tx)
>     >>= (maybe empty return)
>     >>= (put >>> (*> (return tx)))

Matches Text at back

> matchSuffix :: (MonadPlus m) => Text -> ParserT m Text
> matchSuffix tx =
>   (gets $ Tx.stripSuffix tx)
>     >>= (maybe empty return)
>     >>= (put >>> (*> (return tx)))

Fails if remaining text is not empty (matches end of string)

> matchEnd :: (MonadPlus m) => ParserT m ()
> matchEnd = do
>   tx <- get
>   guard (Tx.null tx)


Matches and returns many characters satisfying a predicate, or fails if empty.

> whileP :: (MonadPlus m) => (Char -> Bool) -> ParserT m Text
> whileP predicate =
>   (gets $ span predicate)
>     >>= (\(pfix, next) -> (guard $ not $ Tx.null pfix) *> (put next) *> return pfix )

Skips over a string while a predicate holds (doesn't fail).

> skipWhileP :: (Monad m) => (Char -> Bool) -> ParserT m ()
> skipWhileP predicate =
>   (gets $ span predicate) >>= (snd >>> put)

The inverses of whileP and skipWhileP

> untilP :: (MonadPlus m) => (Char -> Bool) -> ParserT m Text
> untilP = (fmap not) >>> whileP
>
> skipUntilP :: (MonadPlus m) => (Char -> Bool) -> ParserT m ()
> skipUntilP = (fmap not) >>> skipWhileP

> parseNum :: (Num a) => Parser a
> parseNum = fromInteger <$> (parseTo @(Integer) $ whileP isDigit)
