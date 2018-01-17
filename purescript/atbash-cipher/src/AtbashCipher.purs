module AtbashCipher
  ( decode
  , encode
  ) where

import Prelude

import Data.Array (catMaybes, find, mapWithIndex, zip)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray, joinWith, toCharArray, toLower, trim)
import Data.Tuple (fst, snd)

decode :: String → Maybe String
decode ciphertext =
  Just $ fromCharArray $ catMaybes $ map decodeChar (toCharArray ciphertext)

encode :: String → Maybe String
encode plaintext =
  Just $ trim $ joinWith "" $ groupInFives $ catMaybes $ map encodeChar (toCharArray plaintext')
  where
    plaintext' = toLower plaintext
    groupInFives chars = mapWithIndex (\i c -> if i `mod` 5 == 4
                                                  then fromCharArray [c, ' ']
                                                  else fromCharArray [c])
                                      chars

decodeChar encoded = fst <$> find (\pair -> snd pair == encoded) key

encodeChar decoded = snd <$> find (\pair -> fst pair == decoded) key

key = zip (toCharArray "abcdefghijklmnopqrstuvwxyz0123456789")
          (toCharArray "zyxwvutsrqponmlkjihgfedcba0123456789")
