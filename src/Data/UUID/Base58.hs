module Data.UUID.Base58

  ( randomBase58
  , base58ToUUID
  , uuidToBase58
  , encodeBase58
  , decodeBase58
  , encodeBase10
  , decodeBase10
  , encodeBase16
  , decodeBase16
  , encodeBase256
  , decodeBase256
  , integerToUUID
  , uuidToInteger
  ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char (ord)
import           Data.List (elemIndex)
import           Data.UUID (UUID, fromByteString, toByteString)
import           Data.UUID.V4 (nextRandom)

randomBase58 :: IO LBS.ByteString
randomBase58 = do
  uuid <- nextRandom
  return (uuidToBase58 uuid)

base58ToUUID :: LBS.ByteString -> Maybe UUID
base58ToUUID x = case decodeBase58 x of
  Nothing  -> Nothing
  Just int -> case integerToUUID int of
    Nothing   -> Nothing
    Just uuid -> Just uuid

uuidToBase58 :: UUID -> LBS.ByteString
uuidToBase58 = encodeBase58 . uuidToInteger

alphabetBase58 = "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"
encodeBase58   = encodeBase alphabetBase58
decodeBase58   = decodeBase alphabetBase58

alphabetBase10 = "0123456789"
encodeBase10   = encodeBase alphabetBase10
decodeBase10   = decodeBase alphabetBase10

alphabetBase16 = "0123456789abcdef"
encodeBase16   = encodeBase alphabetBase16
decodeBase16   = decodeBase alphabetBase16

alphabetBase256 = ['\x0'..'\xff']
encodeBase256   = encodeBase alphabetBase256
decodeBase256   = decodeBase alphabetBase256

encodeBase :: [Char] -> Integer -> LBS.ByteString
encodeBase alphabet value = LBS.pack encoded where
  encoded  = expand (value `divMod` base) []
  base     = toInteger $ length alphabet
  lookup n = alphabet !! (fromIntegral n)
  expand (dividend, remainder) xs
    | (dividend >  0) = expand (dividend `divMod` base) result
    | (dividend == 0 && remainder >  0) = result
    | (dividend == 0 && remainder == 0) = xs
    where result = [lookup remainder] ++ xs

decodeBase :: [Char] -> LBS.ByteString -> Maybe Integer
decodeBase alphabet value = decoded where
  (_, decoded)   = foldr reducer accumulator chars
  chars          = (LBS.unpack value)
  base           = toInteger $ length alphabet
  shift exponent = (* (base ^ exponent))
  lookup char    = toInteger <$> elemIndex char alphabet
  accumulator    = (0, Just 0)
  reducer (char) (exponent, amount) =
    (exponent + 1, (+) <$> amount <*> (shift exponent <$> lookup char))

uuidToInteger :: UUID -> Integer
uuidToInteger uid = summed where
  (summed, _) = foldr reducer (0, 0) ints
  bytes  = (LBS.unpack . toByteString) uid
  ints   = map (toInteger . ord) bytes
  reducer x (acc, exp) = (256 ^ exp * x + acc, exp + 1)

integerToUUID :: Integer -> Maybe UUID
integerToUUID = fromByteString . encodeBase256 

