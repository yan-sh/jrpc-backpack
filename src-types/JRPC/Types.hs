{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}

module JRPC.Types where

import GHC.TypeLits
import GHC.Prim
import Prelude hiding (lookup)

data CustomError s o = CustomError s (Maybe o) 

data JsonRpcError n =
    ParseError
  | InvalidRequest
  | MethodNotFound n
  | InvalidParams n
  | InternalError n

newtype Named (name :: Symbol) a = Named a

class ToMethod f array object string json m where
  mkMethod
    :: (String -> string)
    -> (string -> object -> Maybe json)
    -> (array -> Maybe (json, array))
    -> f
    -> (Either array object -> m (Either (CustomError string object) json))

instance 
    ( Applicative m
    , ToMethodArray x object string array json m
    , ToMethodObject x object string json m
    ) => ToMethod x array object string json m where
  mkMethod fromString lookup split f = either
    do mkMethodArray @x split f
    do mkMethodObject @x fromString lookup f


class ToMethodObject f object string json m where
  mkMethodObject
    :: (String -> string)
    -> (string -> object -> Maybe json)
    -> f
    -> (object -> m (Either (CustomError string object) json))

instance (KnownSymbol n, Applicative m, ToMethodObject fs object string json m) => ToMethodObject (Named n (Maybe json) -> fs) object string json m where
  mkMethodObject fromString lookup f = \object ->
    mkMethodObject @fs fromString lookup
      (f $ Named $ lookup (fromString $ symbolVal' (proxy# @n)) object)
      object

instance ToMethodObject (m (Either (CustomError string object) json)) object string json m where
  mkMethodObject _ _ = const



class ToMethodArray f object string array json m where
  mkMethodArray
    :: (array -> Maybe (json, array))
    -> f
    -> (array -> m (Either (CustomError string object) json))

instance (KnownSymbol n, Applicative m, ToMethodArray fs object string array json m) => ToMethodArray (Named n (Maybe json) -> fs) object string array json m where
  mkMethodArray split f = \array ->
      case split array of
        Nothing       -> mkMethodArray @fs split (f (Named Nothing)) array
        Just (h, ps)  -> mkMethodArray @fs split (f (Named $ Just h)) ps

instance ToMethodArray (m (Either (CustomError string object) json)) object string array json m where
  mkMethodArray _ = const
