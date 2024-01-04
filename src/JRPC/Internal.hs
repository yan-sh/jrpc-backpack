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

type MethodMap = I.MethodMap

type Array = I.Array

type Params = I.Params

type Response = I.Response

run :: MethodMap
    -> Maybe (forall a . Array (IO a) -> IO (Array a))
    -> Json
    -> IO Json
run = D.run

fromList :: [(JsonString, Params -> IO Response)] -> MethodMap
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

sequenceStrategy :: Array (IO a) -> IO (Array a)
sequenceStrategy = I.sequenceStrategy

makeMethod :: ToMethod f JsonArray JsonObject JsonString Json IO => f -> Params -> IO Response
makeMethod = I.makeMethod
