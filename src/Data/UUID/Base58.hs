module Data.UUID.Base58

  ( randomBase58
  , base58encode
  , base58decode
  , base10encode
  , base10decode
  , uuidToInteger
  ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (ord)
import           Data.List (elemIndex)
import           Data.UUID (UUID, toByteString)
import           Data.UUID.V4 (nextRandom)

randomBase58 :: IO LBS.ByteString
randomBase58 = do
  uid <- nextRandom
  let integerValue = uuidToInteger uid
  return (base58encode integerValue)

uuidToInteger :: UUID -> Integer
uuidToInteger uid = summed where
  bytes  = toByteString uid
  ints   = LBS.foldl (\acc x -> acc ++ [toInteger(ord x)]) [] bytes
  summed = foldl (\acc (exp, amt) -> acc + amt ^ exp) 0 (zip [0..15] ints)

base58alphabet = "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"
base58encode   = baseEncode base58alphabet
base58decode   = baseDecode base58alphabet

base10alphabet = "0123456789"
base10encode   = baseEncode base10alphabet
base10decode   = baseDecode base10alphabet

baseEncode :: [Char] -> Integer -> LBS.ByteString
baseEncode alphabet value = LBS.pack encoded where
  encoded  = expand (value `divMod` base) []
  base     = toInteger $ length alphabet
  lookup n = alphabet !! (fromIntegral n)
  expand (dividend, remainder) xs
    | (dividend >  0) = expand (dividend `divMod` base) result
    | (dividend == 0 && remainder >  0) = result
    | (dividend == 0 && remainder == 0) = xs
    where result = [lookup remainder] ++ xs

baseDecode :: [Char] -> LBS.ByteString -> Maybe Integer
baseDecode alphabet value = decoded where
  (_, decoded)   = foldr reducer accumulator chars
  chars          = (LBS.unpack value)
  base           = toInteger $ length alphabet
  shift exponent = (* (base ^ exponent))
  lookup char    = toInteger <$> elemIndex char alphabet
  accumulator    = (0, Just 0)
  reducer (char) (exponent, amount) =
    (exponent + 1, (+) <$> amount <*> (shift exponent <$> lookup char))

