{-# LANGUAGE DataKinds #-}

import JRPC
import JRPC.Types
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V

methodMap :: MethodMap
methodMap = fromList
  [ ( "reverse"
    , makeMethod reverseMethod
    )
  ]
    where
      reverseMethod :: Named "string" (Maybe Json) -> IO Response
      reverseMethod (Named mbJson) = do
        case mbJson of
          Nothing -> pure (Left $ CustomError "empty" Nothing)
          Just js ->
            case jsonToValue js of
              String s -> pure $ Right $ valueToJson $ String (T.reverse s)
              _ -> pure $ Left $ CustomError "wrong json" Nothing

main :: IO ()
main = do
  r <- run methodMap Nothing $ valueToJson $ object
    [ "jsonrpc" .= String "2.0"
    , "id" .= Number 1
    , "method" .= String "reverse"
    , "params" .= V.fromList [String "123"]
    ]
  print $ jsonToValue r
