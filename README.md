# base58-uuid

Base58 codec for UUIDs

## Usage

```haskell
$ stack ghci
*Data.UUID.Base58> :t randomBase58 
randomBase58 :: IO LBS.ByteString

*Data.UUID.Base58> uuid <- randomBase58
*Data.UUID.Base58> uuid
"chAiWHbBSNpNJMV59dqJVZ"

*Data.UUID.Base58> base58ToUUID uuid
Just 5b6591c9-56fa-4fa9-9e90-48e901e49823

*Data.UUID.Base58> uuidToBase58 <$> base58ToUUID uuid
Just "chAiWHbBSNpNJMV59dqJVZ"

*Data.UUID.Base58> uuidToInteger <$> base58ToUUID uuid
Just 121487126500310875379488292051662182435

*Data.UUID.Base58> integerToUUID <$> uuidToInteger <$> base58ToUUID uuid
Just (Just 5b6591c9-56fa-4fa9-9e90-48e901e49823)
```

## Development

```bash
$ stack test
```

