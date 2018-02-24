{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

{-|
Module: Data.ConnectionString

== Introduction

This module is intended for parsing connection strings in a manner
that is consistent with .NET's <https://docs.microsoft.com/en/dotnet/api/system.data.common.dbconnectionstringbuilder DbConnectionStringBuilder>
class.

The syntax of a connection string appears quite simple at first glance,
and consists of a list of key-value pairs separated by semicolons:

>>> parse "key=value; key2 = value2"
Right (fromList [("key","value"),("key2","value2")])

However, the format can be more complicated than expected.

== Examples

A value may be single-quoted (single quotes can be escaped
inside a single-quoted string by doubling them):

>>> parse "squote='value with '' quotes'"
Right (fromList [("squote","value with ' quotes")])

Or double-quoted (double quotes can also be escaped inside 
a double-quoted string by doubling them):

>>> parse "dquote=\"value with \"\" quotes\""
Right (fromList [("dquote","value with \" quotes")])

Quotes of both kinds may be present in keys:

>>> parse "'quote\"=value"
Right (fromList [("'quote\"","value")])

Whitespace is ignored everywhere except in quoted strings and inside keys or unquoted values:

>>> parse "; a key = v v\t\n;\t key 2 = \"v v\"\n;\t key 3 = 'v v'; "
Right (fromList [("a key","v v"),("key 2","v v"),("key 3","v v")])

Equals signs may be escaped in keys by doubling them:

>>> parse "1==2=false"
Right (fromList [("1=2","false")])

Later values override earlier ones:

>>> parse "key=value;key=value2"
Right (fromList [("key","value2")])

Assigning a key no value will remove it:

>>> parse "key=value;key="
Right (fromList [])

However, you can assign an empty value by giving it a quoted value:

>>> parse "key=value;key=''"
Right (fromList [("key","")])

On the other hand, not providing a key doesn't make any sense:

>>> parse "key=value;=value"
Left "1:11:\nunexpected '='\nexpecting ';', end of input, or white space\n"

>>> parse "=value"
Left "1:1:\nunexpected '='\nexpecting ';', end of input, or white space\n"

This module implements all of these quirks for you!

-}
module Data.ConnectionString
    ( ConnectionString
    , parse
    , Parseable
    , parser
    )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Map as Map
import Data.String (IsString(..))
import Data.Void (Void)

import Control.Applicative.Combinators (many, sepEndBy, skipMany, skipSome, some, (<|>))

import qualified Text.Megaparsec as P
import Text.Megaparsec (Parsec, try, Stream, Token, (<?>))
import Text.Megaparsec.Char (char, notChar, space)

-- | A connection string is a set of keys and values.
type ConnectionString s = Map.Map s s

-- | Parses a connection string, or fails with an error.
--
-- You can parse 'String' inputs:
--
-- >>> parse ("key=value;key2=value2")
-- Right (fromList [("key","value"),("key2","value2")])
--
-- Or you can parse 'Data.Text.Text' inputs:
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text
-- >>> parse ("key=value;key2=value2" :: Text)
-- Right (fromList [("key","value"),("key2","value2")])
--
-- In either case, 'parse' will produce a 'ConnectionString' that
-- has values of the same type as the input.
parse :: Parseable s => s -> Either String (ConnectionString s)
parse cs =
    case (P.parse (parser <* P.eof) "" cs) of
        Left err -> Left (P.parseErrorPretty err)
        Right kvs -> Right (Map.mapMaybe id (Map.fromList kvs))

-- | The constraints on things that 'parse' can handle.
-- (Essentially this means "either 'String' or 'Data.Text.Text'".)
type Parseable s = (Stream s, Token s ~ Char, IsString s, Ord s)

-- | A reusable ("Text.Megaparsec") parser for connection strings.
--
-- A 'Nothing' in the output list indicates that the corresponding
-- key should be deleted from the set.
parser :: Parseable s => Parsec Void s [(s, Maybe s)]
parser = skipMany semiColon *> keyValue `sepEndBy` (skipSome semiColon)
    where
    semiColon = P.between space space (char ';')

keyValue :: Parseable s => Parsec Void s (s, Maybe s)
keyValue = (,) <$> (key <?> "key") <* equals <*> (value <?> "value")
    where
    equals = P.between space space (char '=')

    key =
        fromString . dropWhileEnd isSpace <$>
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

