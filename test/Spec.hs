import Test.Hspec
import Prelude hiding (length)
import Data.UUID (fromByteString, toByteString)
import Data.UUID.V4 (nextRandom)
import Data.UUID.Base58
import Data.ByteString.Lazy.Char8 (length, pack)

main :: IO ()
main = hspec $ do

  describe "Data.UUID.Base58" $ do

    it "Properly decodes and encodes Base58 UUIDs" $ do
      uuid <- randomBase58
      length uuid `shouldBe` 22
      encodeBase58 <$> decodeBase58 uuid `shouldBe` Just uuid
      uuidToBase58 <$> base58ToUUID uuid `shouldBe` Just uuid

    it "Sanity checks big-endian base10" $ do
      encodeBase10 919100 `shouldBe` (pack "919100")

    it "Works with byte arrays" $ do
      uuid <- nextRandom
      let encodedActual   = (encodeBase256 . uuidToInteger) uuid
      let decodedActual   = integerToUUID <$> (decodeBase256 encodedActual)
      let encodedExpected = toByteString uuid
      let decodedExpected = fromByteString encodedExpected
      encodedActual `shouldBe` encodedExpected
      decodedActual `shouldBe` Just decodedExpected
