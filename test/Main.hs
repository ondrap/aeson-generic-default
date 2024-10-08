module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Aeson as AE
import Data.Aeson.DefaultField
import GHC.Generics (Generic)
import Data.Default (Default(..))
import qualified Data.Text as T
import Data.Aeson (FromJSON(..))
import Data.Either (isLeft)

data Color = Red | Green | Blue
  deriving (Show, Eq, Generic, FromJSON)
instance Default Color where
  def = Red

newtype BlueDefault = BlueDefault Color deriving Generic
instance DefaultConstant BlueDefault where
  defValue _ = Blue

data ConfigFileT d = ConfigFile {
  defaultEnabled :: DefaultField d (DefBool True)
, defaultDisabled :: DefaultField d (DefBool False)
, defaultText :: DefaultField d (DefText "default text")
, defaultInt :: DefaultField d (DefInt 42)
, defaultNegativeInt :: DefaultField d (DefNegativeInt 42)
, defaultRed :: DefaultField d (DefDefault Color)
, defaultBlue :: DefaultField d (DefDefaultConstant BlueDefault)
, normalField :: T.Text
, normalOptional :: Maybe Int
} deriving (Generic)

type ConfigFile = ConfigFileT Final
deriving instance Show ConfigFile
deriving instance Eq ConfigFile
instance FromJSON ConfigFile where
  parseJSON = parseWithDefaults AE.defaultOptions


main :: IO ()
main = defaultMain $ testGroup "Tests" [
    testCase "Basic empty input" $ do
      let decoded = AE.eitherDecode "{\"normalField\":\"input\"}"
          expectedResult1 :: ConfigFile
          expectedResult1 = ConfigFile {
            defaultEnabled = True
          , defaultDisabled = False
          , defaultText = "default text"
          , defaultInt = 42
          , defaultNegativeInt = -42
          , defaultRed = Red
          , defaultBlue = Blue
          , normalField = "input"
          , normalOptional = Nothing
          }
      decoded @?= Right expectedResult1
  , testCase "Basic partial input" $ do
      let decoded = AE.eitherDecode "{\"normalField\":\"input\", \"defaultEnabled\":false,\"defaultRed\":\"Green\", \"defaultNegativeInt\":2}"
          expectedResult1 :: ConfigFile
          expectedResult1 = ConfigFile {
            defaultEnabled = False
          , defaultDisabled = False
          , defaultText = "default text"
          , defaultInt = 42
          , defaultNegativeInt = 2
          , defaultRed = Green
          , defaultBlue = Blue
          , normalField = "input"
          , normalOptional = Nothing
          }
      decoded @?= Right expectedResult1
  , testCase "Basic null input" $ do
      let decoded = AE.eitherDecode "{\"normalField\":\"input\", \"defaultEnabled\":null}" :: Either String ConfigFile
      assertBool "Should fail on null value in default field" (isLeft decoded)
  ]
