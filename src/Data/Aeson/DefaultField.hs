{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module : Data.Aeson.DefaultField
-- License     : BSD-style
--
-- Maintainer  : palkovsky.ondrej@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Type-level default fields for aeson Generic FromJSON parser 
--

module Data.Aeson.DefaultField (
  -- * How to use this library
  -- $use

  -- * How to extend the library
  -- $extend

  -- * Caveats
  -- $caveats

  -- * Main field types and functions
    DefaultField
  , ParseStage(Final)
  , parseWithDefaults
  -- * Different basic types
  , DefInt(..)
  , DefNegativeInt(..)
  , DefText(..)
  , DefString(..)
  , DefBool(..)
  , DefDefault(..)
  -- * Support for newtype default constants
  , DefaultConstant(..)
  , DefDefaultConstant(..)
) where

import GHC.TypeLits (Nat, KnownNat, Symbol, KnownSymbol, natVal, symbolVal)
import Data.Aeson (FromJSON(..), genericParseJSON)
import qualified Data.Aeson as AE
import qualified Data.Text as T
import GHC.Generics
import Data.Kind (Type)
import Data.Coerce (Coercible, coerce)
import Data.Aeson.Types (Parser)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Default (Default (def))


-- | Boolean default field
newtype DefBool (a :: Bool) = DefBool Bool
  deriving (Generic)
instance FromJSON (DefBool True) where
  omittedField = return (DefBool True)
  parseJSON AE.Null = return (DefBool True)
  parseJSON v = DefBool <$> parseJSON v
instance FromJSON (DefBool False) where
  omittedField = return (DefBool False)
  parseJSON AE.Null = return (DefBool False)
  parseJSON v = DefBool <$> parseJSON v

-- | Positive Int default field (only positive numbers are supported as type parameters)
newtype DefInt (num :: Nat) = DefInt Int
  deriving (Generic)
instance KnownNat num => FromJSON (DefInt num) where
  omittedField = return (DefInt (fromIntegral $ natVal (Proxy @num)))
  parseJSON AE.Null = return (DefInt (fromIntegral $ natVal (Proxy @num)))
  parseJSON v = DefInt <$> parseJSON v

-- | Negative Int default field
newtype DefNegativeInt (num :: Nat) = DefNegativeInt Int
  deriving (Generic)
instance KnownNat num => FromJSON (DefNegativeInt num) where
  omittedField = return (DefNegativeInt (negate $ fromIntegral $ natVal (Proxy @num)))
  parseJSON AE.Null = return (DefNegativeInt (negate $ fromIntegral $ natVal (Proxy @num)))
  parseJSON v = DefNegativeInt <$> parseJSON v

-- | Text default field
newtype DefText (x :: Symbol) = DefText T.Text
  deriving (Generic)
instance KnownSymbol sym => FromJSON (DefText sym) where
  omittedField = return (DefText (fromString $ symbolVal (Proxy @sym)))
  parseJSON AE.Null = return (DefText (fromString $ symbolVal (Proxy @sym)))
  parseJSON v = DefText <$> parseJSON v

-- | String default field
newtype DefString (x :: Symbol) = DefString String
  deriving (Generic)
instance KnownSymbol sym => FromJSON (DefString sym) where
  omittedField = return (DefString (symbolVal (Proxy @sym)))
  parseJSON AE.Null = return (DefString (symbolVal (Proxy @sym)))
  parseJSON v = DefString <$> parseJSON v

-- | Default field using the "Default" class
newtype DefDefault a = DefDefault a
  deriving (Generic)
instance (FromJSON a, Default a) => FromJSON (DefDefault a) where
  omittedField = return (DefDefault def)
  parseJSON AE.Null = return (DefDefault def)
  parseJSON v = DefDefault <$> parseJSON v


-- | Default class for 'DefDefaultConstant' field configuration
class (Generic a, GNewtype (Rep a)) => DefaultConstant a where
  defValue :: Proxy a -> EmbeddedType (Rep a)

-- | Use 'DefaultConstant' type as a default value for a field.
--
-- E.g. you cannot create a direct settings for real numbers; however, you can do this:
--
-- > newtype Pi = Pi Double deriving Generic
-- > instance DefaultConstant Pi where
-- >   defValue _ = 3.141592654
-- >
-- > data MyObjectT d = MyObject {
-- >   phaseAngle :: DefaultField d (DefDefaultConstant Pi)
-- > } deriving Generic
newtype DefDefaultConstant a = DefDefaultConstant (EmbeddedType (Rep a))
  deriving (Generic)
instance (DefaultConstant a, FromJSON (EmbeddedType (Rep a))) => FromJSON (DefDefaultConstant a) where
  omittedField = return (DefDefaultConstant $ defValue (Proxy @a))
  parseJSON AE.Null = return (DefDefaultConstant (defValue (Proxy @a)))
  parseJSON v = DefDefaultConstant <$> parseJSON v

-- | Kind for separating parsing with defaults and the final type
data ParseStage = 
    InsertDefaults  -- ^ Use this type to instantiate the intermediate data type for parsing defaults
  | Final -- ^ Use this type to instantiate the final data type (e.g. using the type alias)

-- | Higher-kinded-type that either instantiates the field to a newtype that
-- decodes the default value if not present; or instantiates to the type
-- embedded in the newtype parameter
type family DefaultField (m :: ParseStage) a where
  DefaultField InsertDefaults a = a
  DefaultField Final a = EmbeddedType (Rep a)

-- | Copied from newtype-generics package; this way we get the type inside the newtype
class GNewtype n where
  type EmbeddedType n :: Type -- ^ Type function to retrieve the type embedded in the newtype
instance GNewtype (D1 d (C1 c (S1 s (K1 i a)))) where
  type EmbeddedType (D1 d (C1 c (S1 s (K1 i a)))) = a

-- | 'genericParseJSON' drop-in replacement
parseWithDefaults :: forall (o :: ParseStage -> Type) x. 
    (AE.GFromJSON AE.Zero (Rep (o InsertDefaults))
    , Generic (o InsertDefaults), Generic (o Final)
    , Coercible (Rep (o InsertDefaults) x) (Rep (o Final) x)
    ) 
    => AE.Options -> AE.Value -> Parser (o Final)
parseWithDefaults opts v = do
  (dx :: o InsertDefaults) <- genericParseJSON opts v
  return $ to @_ @x (coerce (from @_ @x dx))

-- $use
--
-- @
-- import Data.Aeson ( FromJSON(parseJSON), decode )
-- import Data.Default ( Default(..) )
-- import GHC.Generics (Generic)
-- import qualified Data.Text as T
-- import qualified Data.Aeson as AE
-- import Data.Aeson.DefaultField
-- 
-- -- Define some custom datatype for json data
-- data Color = Red | Green | Blue deriving (Generic, 'FromJSON', Show)
-- 
-- -- We may want a default instance for later use with 'DefDefault'
-- instance 'Default' Color where
--   def = Red
-- 
-- -- Alternatively, we may create a separate default type that would be coerced back
-- -- to Color with 'DefDefaultConstant'. This allows for different default values
-- -- with the same type for different fields.
-- newtype BlueDefault = BlueDefault Color deriving Generic
-- instance 'DefaultConstant' BlueDefault where
--   defValue _ = Blue
-- 
-- -- Simply create the data object we want to parse from the file; add a type parameter
-- -- so that we can use the type family magic to create 2 different type representations.
-- -- Normal fields act normally. Use 'DefaultField' with the appropriate settings
-- -- to configure the parsing of the missing fields.
-- data ConfigFileT d = ConfigFile {
--     defaultEnabled :: 'DefaultField' d ('DefBool' True)
--   , defaultDisabled :: 'DefaultField' d ('DefBool' False)
--   , defaultText :: 'DefaultField' d ('DefText' "default text")
--   , defaultInt :: 'DefaultField' d ('DefInt' 42)
--   , defaultNegativeInt :: 'DefaultField' d ('DefNegativeInt' 42)
--   , defaultRed :: 'DefaultField' d ('DefDefault' Color)
--   , defaultBlue :: 'DefaultField' d ('DefDefaultConstant' BlueDefault)
--   , normalField :: T.Text
--   , normalOptional :: Maybe Int
-- } deriving (Generic)
-- 
-- -- Create a type alias so that we can (mostly) handle the type as if nothing special
-- -- was happening under the hood.
-- type ConfigFile = ConfigFileT 'Final'
-- deriving instance Show ConfigFile
-- 
-- -- Create a custom parsing instance for the data object. 
-- instance FromJSON ConfigFile where
--   parseJSON = 'parseWithDefaults' AE.defaultOptions{AE.rejectUnknownFields=True}
--
-- @
-- 
-- >>> AE.decode "{\"defaultDisabled\":true,\"normalField\":\"text\"}" :: Maybe ConfigFile
-- >>> Just (ConfigFile {
--           defaultEnabled = True
--         , defaultDisabled = True
--         , defaultText = "default text"
--         , defaultInt = 42
--         , defaultNegativeInt = -42
--         , defaultRed = Red
--         , defaultBlue = Blue
--         , normalField = "text"
--         , normalOptional = Nothing}
--         )

-- $extend
--
-- The provided 'DefText', 'DefInt', etc. newtypes should provide enough flexibility to configure the missing
-- fields for the objects. If a special type of configuration is needed, a /newtype/
-- based on a final type must be created with 'Generic' and 'FromJSON' instances.
-- See the source code for examples.

-- $caveats
--
-- The final step in the parsing is coercing the structure with newtypes (e.g. 'DefBool') to 
-- a structure with the final types (e.g. Bool). Unfortunately, the type families in Haskell 
-- cause the type not to be directly coercible between the intermediate and the 'Final' stage.
-- However, it is possible through the Generic instances.
--
-- This solution probably brings some performance degradation in the sense that the structure
-- must be recreated. Benchmark before use in performance-sensitive situations.
