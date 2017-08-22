{-# LANGUAGE TemplateHaskell #-}

module Domain.Resource.Algebra (
) where

import Lens.Micro.Platform
import qualified Data.Text as T

data Resource = Resource { _name :: T.Text }
makeLenses ''Resource
