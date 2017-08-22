module Utility.Lua where

import           Data.Aeson
import qualified Foreign.Lua as Lua

openLuaFile :: FilePath -> Lua.Lua ()
openLuaFile file = do
  Lua.loadfile file
  Lua.call 0 0

fetchGlobal :: (Lua.FromLuaStack j) => String -> Lua.Lua j
fetchGlobal varname = do
  top <- Lua.gettop
  Lua.getglobal varname
  top' <- Lua.gettop
  Lua.peek top' <* Lua.pop (top' - top)

