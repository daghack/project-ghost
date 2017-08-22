{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Utility.Aeson.Lua where

import           Control.Exception (throw)
import           Data.Either
import           Data.Scientific (toRealFloat, fromFloatDigits)
import           Foreign.Lua
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map.Lazy as M
import qualified Data.HashMap.Lazy as HM

instance ToLuaStack A.Value where
  push (A.Object obj) = push . M.fromList . HM.toList $ obj
  push (A.Array arr) = push . V.toList $ arr
  push (A.String str) = push str
  push (A.Number num) = push (toRealFloat num :: Double)
  push (A.Bool bool) = push bool
  push _ = push ()

instance FromLuaStack A.Value where
  peek index = peekAll index [peekBool, peekNum, peekStr, peekObj, peekArr, peekNull]
    where
      peekAll _ [] = throw $ LuaException "unable to convert lua obj to aeson value"
      peekAll index (h:t) = do
        test <- h index
        case test of
          Right val -> return val
          Left _ -> peekAll index t
      peekBool index = fmap A.Bool <$> peekEither index
      peekStr index = fmap A.String <$> peekEither index
      peekNum index =
        fmap (A.Number . fromFloatDigits) <$>
          (peekEither index :: Lua (Either String LuaNumber))
      peekArr index = fmap (A.Array . V.fromList) <$>
          (peekEither index :: Lua (Either String [A.Value]))
      peekObj index =
        fmap (A.Object . HM.fromList . M.toList) <$>
          (peekEither index :: Lua (Either String (M.Map T.Text A.Value)))
      peekNull index = (peekEither index :: Lua (Either String ())) >> pure (Right A.Null)

instance (A.ToJSON j) => ToLuaStack j where
  push = push . A.toJSON

instance (A.FromJSON j) => FromLuaStack j where
  peek i = do
    val <- peekEither i :: Lua (Either String A.Value)
    let val' = A.fromJSON <$> val
    case val' of
      Right (A.Success a) -> return a
      _ -> throw $ LuaException "unable to convert lua obj to aeson value"
