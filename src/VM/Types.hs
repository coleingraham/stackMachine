{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VM.Types where

import           Control.DeepSeq
import           Data.Hashable       (Hashable)
import           Data.String         (IsString)
import qualified Data.Text           as T
import           Foreign.Storable
import           GHC.Generics        (Generic)

newtype Var = Var {
        unVar :: T.Text
    } deriving (Show,Eq,Ord,IsString,Hashable,Generic)

newtype Addr = Addr {
        unAddr :: Int
    } deriving (Show,Eq,Ord,Enum,Num,Hashable,Generic,NFData,Storable)

