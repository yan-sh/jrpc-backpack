module JRPC.Indef where

import Prelude hiding (id)
import JRPC.Sig
import JRPC.Types
import Data.Maybe (fromMaybe)
import Data.Bool (bool)

run :: MethodMap
    -> Maybe (forall a . Array (M a) -> M (Array a))
    -> Json
    -> M Json
run methodMap mbStrategy = go True

  where

    go arrayIsAllowed json_ = consumeJson json_
        (fmap responseToJSON . runOnObject)
        (bool (const invalidReq) (fmap jsonArrayToJson . runOnArray) arrayIsAllowed)
        (const invalidReq)
        (const invalidReq)
        (const invalidReq)
        invalidReq
      where
        invalidReq = pure $ jsonFromError InvalidRequest

    runOnArray =
      fmap arrayOfJsonToJsonArray
        . strategy
        . fmap (go False) 
        . jsonArrayToArrayOfJson
 
    responseToJSON = either jsonFromError jsonFromResult
      where
        jsonFromResult (id, res) = case res of
          Left (CustomError m d) -> mkJsonRpcError (Just id) m d
          Right result -> mkJsonRpcResult id result

    runOnObject jsonObject = do
      fromMaybe (pure $ Left InvalidRequest) do
        id <- getId jsonObject
        method <- getMethod jsonObject
        params <- getParams jsonObject
        pure $ case lookupMethodMap method methodMap of
          Nothing -> pure $ Left $ MethodNotFound id
          Just f -> fmap (Right . (id,)) (f params)

    jsonFromError = \case
      ParseError        -> mkError "Parse error"
      InvalidRequest    -> mkError "Invalid request"
      MethodNotFound id -> mkErrorId id "Method not found"
      InvalidParams id  -> mkErrorId id "Invalid params"
      InternalError id  -> mkErrorId id "Internal error"
      where
        mkErrorId id message = mkJsonRpcError (Just id) message Nothing
        mkError message = mkJsonRpcError Nothing message Nothing

    strategy = fromMaybe sequenceStrategy mbStrategy
