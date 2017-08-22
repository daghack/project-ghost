{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Character.Random where

import           Data.Aeson
import           Domain.Character.Algebra
import           Control.Monad.Random
import           Domain.Character.Lua
import           Utility.Lua
import qualified Data.Text as T
import qualified Foreign.Lua as Lua

randStatblock :: (MonadRandom m) => Statblock -> Int -> m Statblock
randStatblock base rangeAbs = do
  mods <- replicateM 6 $ getRandomR (negate rangeAbs, rangeAbs)
  return . toStatblock . zipWith (+) mods $ fromStatblock base

randomBoundedEnum :: (Bounded e, Enum e, MonadRandom m) => m e
randomBoundedEnum = randREnum (minBound, maxBound)
  where
    randREnum :: (Enum e, MonadRandom m) => (e, e) -> m e
    randREnum (rMin, rMax) = toEnum <$> getRandomR (fromEnum rMin, fromEnum rMax)

choice :: (MonadRandom m) => [a] -> m a
choice l = fromList $ zip l $ repeat 1

randClassType :: (MonadRandom m) => m ClassType
randClassType = randomBoundedEnum

randProfType :: (MonadRandom m) => m ProfType
randProfType = randomBoundedEnum

randCharacterRace :: (MonadRandom m) => m CharacterRace
randCharacterRace = randomBoundedEnum

randGender :: (MonadRandom m) => m Gender
randGender = randomBoundedEnum

randCharacterName :: (MonadRandom m) => [T.Text] -> [T.Text] -> [T.Text] -> m CharacterName
randCharacterName titles firsts lasts =
  CharacterName <$>
    choice firsts <*>
    choice lasts <*>
    choice titles

randCharacterClass :: (MonadRandom m) => Int -> m CharacterClass
randCharacterClass level = CharacterClass <$> randClassType <*> pure level

randCharacterProf :: (MonadRandom m) => Int -> m CharacterProf
randCharacterProf level = CharacterProf <$> randProfType <*> pure level

randCharacter :: (MonadRandom m) => NameList -> NameList -> NameList -> m Character
randCharacter titles firsts lasts = do
  gender <- randGender
  race <- randCharacterRace
  let titleList     = getNameFromNameList [] titles race gender
  let firstNameList = getNameFromNameList [] firsts race gender
  let lastNameList  = getNameFromNameList [] lasts  race gender
  Character <$>
    randCharacterName titleList firstNameList lastNameList <*>
    pure gender <*>
    randCharacterClass 1 <*>
    randCharacterProf 1 <*>
    pure race <*>
    randStatblock (Statblock 10 10 10 10 10 10) 4

genRandomCharacter :: FilePath -> String -> IO Character
genRandomCharacter namefile namesprefix = do
  (titles, firsts, lasts) <- Lua.runLua $
    openLuaFile namefile >> readNameLists namesprefix
  randCharacter titles firsts lasts
