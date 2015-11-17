{-# LANGUAGE OverloadedStrings #-}

module SHC.Types
    where

import           Data.Aeson
import           Trace.Hpc.Mix


data ConversionType = FullLines
                    | PartialLines
                    deriving (Show, Eq)

type CoverageEntry =
    ( [MixEntry]  -- Mix entries
    , [Integer]   -- Tix values
    , [String]    -- Entry source code
    )

data Hit = Full
         | Partial
         | None
         | Irrelevant
         deriving (Eq, Show)

type Lix = [Hit]

data PostResult = PostSuccess String
                | PostFailure String
                deriving (Show)

data Config = Config
    { suitesName  :: [String]
    , serviceName :: String
    , jobId       :: String
    , repoToken   :: Maybe String
    , gitInfo     :: GitInfo
    , hpcDir      :: FilePath
    , mixDir      :: FilePath
    , conversion  :: ConversionType
    }

data GitInfo = GitInfo
    { headRef :: Commit
    , branch  :: String
    , remotes :: [Remote]
    }

data Commit = Commit
    { hash           :: String
    , authorName     :: String
    , authorEmail    :: String
    , committerName  :: String
    , committerEmail :: String
    , message        :: String
    }

data Remote = Remote
    { name :: String
    , url  :: String
    }

instance ToJSON GitInfo where
    toJSON i = object [ "head"    .= headRef i
                      , "branch"  .= branch i
                      , "remotes" .= remotes i
                      ]

instance ToJSON Commit where
    toJSON c = object [ "id"              .= hash c
                      , "author_name"     .= authorName c
                      , "author_email"    .= authorEmail c
                      , "committer_name"  .= committerName c
                      , "committer_email" .= committerEmail c
                      , "message"         .= message c
                      ]

instance ToJSON Remote where
    toJSON r = object [ "name" .= name r
                      , "url"  .= url r
                      ]
