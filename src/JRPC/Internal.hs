{-# LANGUAGE MonoLocalBinds #-}

module JRPC.Internal where

import qualified JRPC.Def as D
import JRPC.Types
import qualified JRPC.Impl as I
import Data.Aeson hiding (Array)
import Data.Vector

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
run = D.run

fromList
  :: [(JsonString, Either JsonArray JsonObject -> IO (Either (CustomError JsonString JsonObject) Json))]
  -> MethodMap
fromList = I.buildMethodMap

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
