
{-# LANGUAGE LambdaCase #-}

module ParseHelpers where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Word
import Text.Read

import HexStuff
import ParseMonad

nameParser :: Parse String String
nameParser = liftA2 (:) (spot isAlpha) (list $ spot isAlphaNum)

readParser :: Read a => Parse String String -> Parse String a
readParser = (=<<) $ flip (.) readMaybe $ \case
  Nothing -> empty
  Just res -> return res

numberStringParser = (list1 $ spot isDigit) <|> (liftA2 (++) (tokens "0x") (list1 $ spot isHexDigit))

numberParser :: Read a => Parse String a
numberParser = readParser $ numberStringParser

decNumberParser :: Read a => Parse String a
decNumberParser = readParser $ list1 $ spot isDigit

signedNumberParser :: Read a => Parse String a
signedNumberParser = readParser $ numberStringParser <|> liftA2 (:) (token '-') numberStringParser

word8Parser :: Parse String Word8
word8Parser = fmap unHex $ readParser $ replicateM 2 $ spot isHexDigit

hexStringParser = ws >> list1 (word8Parser <* ws)
