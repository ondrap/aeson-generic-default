module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Aeson as AE

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing default parsing" [ unitTests ]


data ConfigFileT d = ConfigFile {
  defaultEnabled :: DefaultField d (DefBool True)
, defaultDisabled :: DefaultField d (DefBool False)
, defaultText :: DefaultField d (DefText "default text")
, defaultInt :: DefaultField d (DefInt 42)
, defaultNegativeInt :: DefaultField d (DefNegativeInt 42)
-- >   , defaultRed :: DefaultField d (DefDefault Color)
-- >   , defaultBlue :: DefaultField d (DefDefaultCoerce BlueDefault Color)
-- >   , normalField :: T.Text
-- >   , normalOptional :: Maybe Int
-- > } deriving (Generic, Show)


unitTests = testGroup ""
    [ testCase "" $ 
       AE.decode "" @?= 
    ]