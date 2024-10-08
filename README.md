## aeson-generic-default

Type-level configuration of missing values for aeson FromJSON Generic parser.

Using higher-kinded-types, Coercible and Generic instances it is possible to define
a class that can be configured by a type parameter to use custom field
parsers during parsing and only when the parsing is done, it can be coerced
to use normal types.

```haskell
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

instance FromJSON ConfigFile where
  parseJSON = parseWithDefaults defaultOptions
```

The resulting `ConfigFile` type alias has a form of:

```haskell
{
  defaultEnabled :: Bool
, defaultDisabled :: Bool
, defaultText :: Text
, defaultInt :: Int
, defaultNegativeInt :: Int
, defaultRed :: Color
, defaultBlue :: Color
, normalField :: T.Text
, normalOptional :: Maybe Int
}
```

The type-level configuration can be easily extended with newtypes, e.g. to create a full compatibility
layer with `singletons`:

```haskell
newtype DefKind (a :: k) = DefKind (Demote k) deriving Generic
instance (SingI a, SingKind k, FromJSON (Demote k)) => FromJSON (DefKind (a :: k)) where
  omittedField = Just $ DefKind $ fromSing (sing @a)
  parseJSON v = DefKind <$> parseJSON v
```

And then the definition of the field can be for any singleton (e.g. Bool):

```haskell
flag :: DefaultField d (DefKind True)
```
