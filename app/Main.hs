{-# LANGUAGE DataKinds #-}

import JRPC
import Data.Aeson
import qualified Data.Text as T

methodMap :: MethodMap
methodMap = fromList
  [ ( "reverse"
    , makeMethod reverseMethod
    )
  ]
    where
      reverseMethod
        :: Named "string" (Maybe Json)
        -> IO (Either (CustomError JsonString JsonObject) Json)
      reverseMethod (unnamed -> mbJson) = do
        case mbJson of
          Nothing -> pure (Left $ customError "empty" Nothing)
          Just js ->
            case jsonToValue js of
              String s -> pure $ Right $ valueToJson $ String (T.reverse s)
              _ -> pure $ Left $ customError "wrong json" Nothing

main :: IO ()
main = do
  r <- run methodMap Nothing $ valueToJson $ object
    [ "jsonrpc" .= String "2.0"
    , "id" .= Number 1
    , "method" .= String "reverse"
    , "params" .= object ["string" .= String "123"]
    ]
  print $ jsonToValue r
