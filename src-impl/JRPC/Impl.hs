{-# LANGUAGE MonoLocalBinds #-}

module JRPC.Impl where

import JRPC.Types
import Data.Vector
import qualified Data.Vector as V
import Data.Aeson hiding (Array)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Text
import Data.Scientific
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Coerce
import Data.Hashable
import Data.String

type M = IO

newtype Array a = Array (Vector a)
  deriving newtype Functor

newtype Json = Json Value

newtype JsonObject = JsonObject Object

newtype JsonArray = JsonArray (Vector Value)

newtype JsonString = JsonString Text
  deriving newtype (Eq, Hashable, IsString)

newtype JsonNumber = JsonNumber Scientific

newtype JsonBool = JsonBool Bool

consumeJson 
  :: Json
  -> (JsonObject -> a)
  -> (JsonArray -> a)
  -> (JsonString -> a)
  -> (JsonNumber -> a)
  -> (JsonBool -> a)
  -> a
  -> a
consumeJson (Json v) ifObj ifArray ifStr ifNum ifBool ifNull =
  case v of
    Object obj -> ifObj (coerce obj)
    A.Array array -> ifArray (coerce array)
    String str -> ifStr (coerce str)
    Number num -> ifNum (coerce num)
    Bool bool -> ifBool (coerce bool)
    Null -> ifNull

jsonArrayToArrayOfJson :: JsonArray -> Array Json
jsonArrayToArrayOfJson = coerce

arrayOfJsonToJsonArray :: Array Json -> JsonArray
arrayOfJsonToJsonArray = coerce

getId :: JsonObject -> Maybe JsonNumber
getId (JsonObject x) = do
  KM.lookup "id" x >>= \case
    Number n -> pure (coerce n)
    _ -> Nothing

getMethod :: JsonObject -> Maybe JsonString
getMethod (JsonObject x) = do
  KM.lookup "method" x >>= \case
    String s -> pure (coerce s)
    _ -> Nothing

getParams :: JsonObject -> Maybe (Either JsonArray JsonObject)
getParams (JsonObject x) = do
  KM.lookup "params" x >>= \case
    A.Array a -> pure (Left $ coerce a)
    Object ob -> pure (Right $ coerce ob)
    _ -> Nothing

jsonArrayToJson :: JsonArray -> Json
jsonArrayToJson = Json . A.Array . coerce


mkJsonRpcError :: Maybe JsonNumber -> JsonString -> Maybe JsonObject -> Json
mkJsonRpcError mbId message mbData = Json $ object
  [ "id" .= fromMaybe Null (fmap Number $ coerce mbId),
    "jsonrpc" .= String "2.0",
    "error" .= object
      [ "message" .= coerce @_ @Text message,
        "data" .= fromMaybe Null (fmap Object $ coerce mbData)
      ]
  ]

mkJsonRpcResult :: JsonNumber -> Json -> Json
mkJsonRpcResult id_ res = Json $ object
  [ "id" .= Number (coerce id_),
    "jsonrpc" .= String "2.0",
    "result" .= coerce @_ @Value res
  ]

newtype MethodMap = MethodMap
  (HM.HashMap JsonString (Either JsonArray JsonObject -> M (Either (CustomError JsonString JsonObject) Json)))

buildMethodMap
  :: [(JsonString, Either JsonArray JsonObject -> M (Either (CustomError JsonString JsonObject) Json))]
  -> MethodMap
buildMethodMap = coerce . HM.fromList

lookupMethodMap
  :: JsonString
  -> MethodMap
  -> Maybe (Either JsonArray JsonObject -> M (Either (CustomError JsonString JsonObject) Json))
lookupMethodMap s m = HM.lookup s (coerce m)

sequenceStrategy :: forall a . Array (M a) -> M (Array a)
sequenceStrategy = coerce @(M (Vector a)) . V.sequence . coerce

valueToJson :: Value -> Json
valueToJson = coerce

jsonToValue :: Json -> Value
jsonToValue = coerce

jsonObjectToObject :: JsonObject -> Object
jsonObjectToObject = coerce

arrayToVector :: Array a -> Vector a
arrayToVector = coerce

vectorToArray :: Vector a -> Array a
vectorToArray = coerce

makeMethod
  :: ToMethod f JsonArray JsonObject JsonString Json M
  => f
  -> Either JsonArray JsonObject
  -> M (Either (CustomError JsonString JsonObject) Json)
makeMethod = mkMethod
  do JsonString . pack
  do \(JsonString s) (JsonObject o) -> coerce (KM.lookup (K.fromText s) o)
  do \(JsonArray array) -> coerce (V.uncons array)
