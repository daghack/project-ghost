{-# LANGUAGE OverloadedStrings #-}

module Domain.Character.Lua where

import           Control.Monad.Random
import           Data.Aeson
import           Data.Monoid
import           Domain.Character.Algebra
import           Foreign.Lua
import           Utility.Aeson.Lua
import           Utility.Lua
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import qualified Foreign.Lua as Lua

loadNameList :: String -> Lua.Lua NameList
loadNameList = fetchGlobal

readNameLists :: String -> Lua.Lua (NameList, NameList, NameList)
readNameLists var = do
  titles <- fetchGlobal $ var <> "_titles"
  firsts <- fetchGlobal $ var <> "_firstnames"
  lasts  <- fetchGlobal $ var <> "_lastnames"
  return (titles, firsts, lasts)
