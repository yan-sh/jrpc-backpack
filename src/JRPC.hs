{-# LANGUAGE MonoLocalBinds #-}

module JRPC where

import qualified JRPC.Internal as I
import qualified JRPC.Types as T
import Data.Aeson hiding (Array)
import Data.Vector
import Data.Coerce

type CustomError = T.CustomError

type JsonRpcError = T.JsonRpcError

type ToMethod = T.ToMethod

type ToMethodObject = T.ToMethodObject

type ToMethodArray = T.ToMethodArray

type Named = T.Named

type Json = I.Json

type JsonObject = I.JsonObject

type JsonArray = I.JsonArray

type JsonString = I.JsonString

type JsonNumber = I.JsonNumber

type JsonBool = I.JsonBool

type MethodMap = I.MethodMap

type Array = I.Array

run :: MethodMap
    -> Maybe (forall a . Array (IO a) -> IO (Array a))
    -> Json
    -> IO Json
run = I.run

fromList
  :: [(JsonString, Either JsonArray JsonObject -> IO (Either (CustomError JsonString JsonObject) Json))]
  -> MethodMap
fromList = I.fromList

valueToJson :: Value -> Json
valueToJson = I.valueToJson

jsonToValue :: Json -> Value
jsonToValue = I.jsonToValue

jsonObjectToObject :: JsonObject -> Object
jsonObjectToObject = I.jsonObjectToObject

jsonArrayToArrayOfJson :: JsonArray -> Array Json
jsonArrayToArrayOfJson = I.jsonArrayToArrayOfJson

arrayToVector :: Array a -> Vector a
arrayToVector = I.arrayToVector

vectorToArray :: Vector a -> Array a
vectorToArray = I.vectorToArray

makeMethod
  :: ToMethod f JsonArray JsonObject JsonString Json IO
  => f
  -> Either JsonArray JsonObject
  -> IO (Either (CustomError JsonString JsonObject) Json)
makeMethod = I.makeMethod

unnamed :: Named n a -> a
unnamed = coerce

customError :: JsonString -> Maybe JsonObject -> CustomError JsonString JsonObject
customError = T.CustomError
