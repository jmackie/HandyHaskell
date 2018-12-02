{-# OPTIONS -Weverything -fno-warn-implicit-prelude #-}

import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Test.Hspec as Test

import Data.Either (isLeft)
import Data.ByteString (ByteString)
import Data.Text.Encoding.Error (UnicodeException)
import Control.Exception (evaluate)


-- "Hello world"
helloAscii :: ByteString
helloAscii =
    Data.ByteString.pack [72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100]

-- "你好世界" (Chinese)
helloUtf8 :: ByteString
helloUtf8 = Data.ByteString.pack
    [228, 189, 160, 229, 165, 189, 228, 184, 150, 231, 149, 140]

-- Dodgy unicode (invalid 2 octet sequence)
helloInvalidUtf8 :: ByteString
helloInvalidUtf8 = Data.ByteString.pack [195, 40] <> helloUtf8


main :: IO ()
main = Test.hspec $ do
    Test.describe "Data.ByteString.Char8" $ do
        let decode :: ByteString -> String
            decode = Data.ByteString.Char8.unpack

        Test.it "works fine for ascii text" $ do
            decode helloAscii `Test.shouldBe` "Hello world"
        Test.it "garbles unicode" $ do
            decode helloUtf8 `Test.shouldNotBe` "你好世界"
        Test.it "garbles bad unicode" $ do
            decode helloInvalidUtf8 `Test.shouldNotBe` "你好世界"

    Test.describe "Data.Text.Encoding.decodeUtf8" $ do
        let decode :: ByteString -> String
            decode = Data.Text.unpack . Data.Text.Encoding.decodeUtf8

            unicodeException :: Test.Selector UnicodeException
            unicodeException _ = True

        Test.it "works fine for ascii text" $ do
            decode helloAscii `Test.shouldBe` "Hello world"
        Test.it "works fine for unicode" $ do
            decode helloUtf8 `Test.shouldBe` "你好世界"
        Test.it "throws an exception for bad unicode" $ do
            evaluate (decode helloInvalidUtf8)
                `Test.shouldThrow` unicodeException

    Test.describe "Data.Text.Encoding.decodeUtf8'" $ do
        let decode :: ByteString -> Either UnicodeException String
            decode = fmap Data.Text.unpack . Data.Text.Encoding.decodeUtf8'

        Test.it "works fine for ascii text" $ do
            decode helloAscii `Test.shouldBe` Right "Hello world"
        Test.it "works fine for unicode" $ do
            decode helloUtf8 `Test.shouldBe` Right "你好世界"
        Test.it "indicates bad unicode via Left value" $ do
            decode helloInvalidUtf8 `Test.shouldSatisfy` isLeft
