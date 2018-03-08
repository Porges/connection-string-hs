{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module: Data.ConnectionString

== Introduction

This module is intended for parsing connection strings in a manner
that is consistent with .NET's <https://docs.microsoft.com/en/dotnet/api/system.data.common.dbconnectionstringbuilder DbConnectionStringBuilder>
class.

The syntax of a connection string appears quite simple at first glance,
and consists of a list of key-value pairs separated by semicolons:

>>> parse "key=value; key2 = value2"
Right "key=value;key2=value2"

However, the format can be more complicated than expected.

== Examples

A value may be single-quoted (single quotes can be escaped
inside a single-quoted string by doubling them):

>>> parse "squote='value with '' quotes'"
Right "squote=\"value with ' quotes\""

Or double-quoted (double quotes can also be escaped inside 
a double-quoted string by doubling them):

>>> parse "dquote=\"value with \"\" quotes\""
Right "dquote='value with \" quotes'"

Quotes of both kinds may be present in keys:

>>> parse "'quote\"=value"
Right "'quote\"=value"

Whitespace is ignored everywhere except in quoted strings and inside keys or unquoted values:

>>> parse "; a key = v v\t\n;\t key 2 = \"v v\"\n;\t key 3 = 'v v'; "
Right "a key=\"v v\";key 2=\"v v\";key 3=\"v v\""

Equals signs may be escaped in keys by doubling them:

>>> parse "1==2=false"
Right "1==2=false"

Keys are case-insensitive (and converted to lower-case on output):

>>> parse "BIG=small"
Right "big=small"

Later values override earlier ones:

>>> parse "key=value;key=value2"
Right "key=value2"

Assigning a key no value will remove it:

>>> parse "key=value;key="
Right ""

However, you can assign an empty value by giving it a quoted value:

>>> parse "key=value;key=''"
Right "key=''"

-- TODO ^ there appears to be a bug here in .NET

On the other hand, not providing a key doesn't make any sense:

>>> parse "key=value;=value"
Left "1:11:\nunexpected '='\nexpecting ';', end of input, or white space\n"

>>> parse "=value"
Left "1:1:\nunexpected '='\nexpecting ';', end of input, or white space\n"

Another quirk is that keys can contain semicolons:

>>> parse "key=value;key2;extended=value"
Right "key=value;key2;extended=value"

This module implements all of these quirks for you!

-}
module Data.ConnectionString
    ( ConnectionString
    , Data.ConnectionString.toList
    , keys
    , values
    , (!)
    , toString
    , parse
    , Parseable
    , parser
    )
where

import Data.Char (isSpace)
import qualified Data.CaseInsensitive as CI
import Data.Function ((&))
import Data.List (dropWhileEnd, intersperse)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Void (Void)
import GHC.Exts (IsList(..))

import Control.Applicative.Combinators (many, sepEndBy, skipMany, skipSome, some, (<|>))

import qualified Text.Megaparsec as P
import Text.Megaparsec (Parsec, try, Stream, Token, (<?>))
import Text.Megaparsec.Char (char, notChar, space)

-- | A connection string is a set of keys that map to values.
newtype ConnectionString s = ConnectionString (Map.Map (Key s) s)
    deriving (Eq)

instance (Show s, IsString s, IsList s, Item s ~ Char) => Show (ConnectionString s) where
    show = show . toString

toList :: ConnectionString s -> [(s, s)]
toList (ConnectionString cs) =
    Map.toList cs
    & map (\(Key k, v) -> (CI.foldedCase k, v))

keys :: ConnectionString s -> [s]
keys (ConnectionString cs) =
    Map.keys cs
    & map (\(Key k) -> CI.foldedCase k)

values :: ConnectionString s -> [s]
values (ConnectionString cs) = Map.elems cs

-- | Tries to find the given key in the connection string,
-- and returns either the value or 'Nothing'.
(!) :: (CI.FoldCase s, Ord s, IsList s, Item s ~ Char) => ConnectionString s -> s -> Maybe s
ConnectionString cs ! key = cs Map.!? toKey key

-- | A key is case-insensitive and does not start
-- or end with whitespace characters.
newtype Key s = Key (CI.CI s)
    deriving (Eq, Ord)

-- | Converts the given string-like type to a key. (Does not fail.)
toKey :: (CI.FoldCase s, IsList s, Item s ~ Char) => s -> Key s
toKey s =
    GHC.Exts.toList s
    & dropWhile isSpace
    & dropWhileEnd isSpace
    & fromList
    & CI.mk
    & Key

-- | Render a 'ConnectionString' as the string type.
toString :: forall s. (IsString s, IsList s, Item s ~ Char) => ConnectionString s -> s
toString (ConnectionString cs) =
    Map.toList cs
    & map (\(k, v) -> encodeKey k <> "=" <> encodeValue v)
    & intersperse ";" 
    & mconcat
    & fromList

    where
    encodeKey :: Key s -> String
    encodeKey (Key (CI.foldedCase -> k)) =
        replace '=' "==" (GHC.Exts.toList k)

    encodeValue :: s -> String
    encodeValue (GHC.Exts.toList -> v)
        | v == "" = "''"
        | hasDquote && not hasSquote = "'" <> v <> "'"
        | hasSemiColon || hasSpace || hasDquote || hasSquote
            = "\"" <> replace '"' "\"\"" v <> "\""
        | otherwise = v
        where
        hasSquote = '\'' `elem` v
        hasDquote = '"' `elem` v
        hasSemiColon = ';' `elem` v
        hasSpace = any isSpace v

    replace :: Char -> String -> String -> String
    replace _ _ [] = []
    replace from to (x:xs)
        | from == x = to ++ replace from to xs
        | otherwise = x : replace from to xs


-- | Parses a connection string, or fails with an error.
--
-- You can parse 'String' inputs:
--
-- >>> parse ("key=value;key2=value2")
-- Right "key=value;key2=value2"
--
-- Or you can parse 'Data.Text.Text' inputs:
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text
-- >>> parse ("key=value;key2=value2" :: Text)
-- Right "key=value;key2=value2"
--
-- In either case, 'parse' will produce a 'ConnectionString' that
-- has values of the same type as the input.
parse :: Parseable s => s -> Either String (ConnectionString s)
parse cs =
    case (P.parse (parser <* P.eof) "" cs) of
        Left err -> Left (P.parseErrorPretty err)
        Right kvs -> Right (ConnectionString (Map.mapMaybe id (Map.fromList kvs)))

-- | The constraints on things that 'parse' can handle.
-- (Essentially this means "either 'String' or 'Data.Text.Text'".)
type Parseable s = (Stream s, Token s ~ Char, IsString s, Ord s, CI.FoldCase s)

-- | A reusable ("Text.Megaparsec") parser for connection strings.
--
-- A 'Nothing' in the output list indicates that the corresponding
-- key should be deleted from the set.
parser :: Parseable s => Parsec Void s [(Key s, Maybe s)]
parser = skipMany semiColon *> keyValue `sepEndBy` (skipSome semiColon)
    where
    semiColon = P.between space space (char ';')

keyValue :: Parseable s => Parsec Void s (Key s, Maybe s)
keyValue = (,) <$> (key <?> "key") <* equals <*> (value <?> "value")
    where
    equals = P.between space space (char '=')

    key =
        Key . CI.mk . fromString . dropWhileEnd isSpace <$>
        some (notChar '=' <|> try (char '=' *> char '='))
            
    value = fmap fromString <$> (sQuoted <|> dQuoted <|> unQuoted)

    sQuoted =
        Just <$>
        P.between (char '\'') (char '\'') (many (notChar '\'' <|> try (char '\'' *> char '\'')))

    dQuoted =
        Just <$>
        P.between (char '"') (char '"') (many (notChar '"' <|> try (char '"' *> char '"')))

    unQuoted =
        -- a completely empty value indicates we should remove it (see docs
        -- above), so we return Nothing in this case
        (\s -> if null s then Nothing else Just s) . dropWhileEnd isSpace <$>
        (space *> many (notChar ';') <* space)

