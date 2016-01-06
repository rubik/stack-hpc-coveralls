{-# LANGUAGE CPP, OverloadedStrings #-}

module SHC.Types
    where

import           Control.Monad (forM)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           Trace.Hpc.Mix

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif


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
    { packageName   :: String
    , suitesName    :: [String]
    , serviceName   :: String
    , jobId         :: String
    , repoToken     :: Maybe String
    , gitInfo       :: GitInfo
    , hpcDir        :: FilePath
    , mixDir        :: Maybe FilePath
    , conversion    :: ConversionType
    , stackProjects :: [StackProject]
    , fetchCoverage :: Bool
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

-- | Data returned by the @stack query@ command.
data StackQuery =
  StackQuery
  { stackQueryLocals :: [(String, FilePath)]
    -- ^ A list of pairs of @(package-name, filepath)@, where the
    -- @filepath@ is the absolute 'FilePath' where @package-name@ is
    -- located.
  } deriving (Show)

instance FromJSON StackQuery where
  parseJSON = withObject "StackQuery" $ \o -> StackQuery <$> (o .: "locals" >>= parseLocals)
    where
      parseLocals =
        withObject "StackQuery/locals" $ \o ->
          forM (HM.toList o) $ \(pkgName, val) -> do
            filepath <- withObject "StackQuery/locals/package" (.: "path") val
            return (T.unpack pkgName, filepath)


-- | Information we've collected about a stack project.
data StackProject =
  StackProject
  { stackProjectName   :: String
    -- ^ The name of this project.
  , stackProjectPath   :: Maybe FilePath
    -- ^ Path of the project relative to current path.  @Nothing@ iff
    -- the project's path is the current path.
  , stackProjectKey    :: String
    -- ^ Key that @ghc-pkg@ uses for this project.
  , stackProjectMixDir :: FilePath
    -- ^ The project's mix dir (cf. 'getMixDir').
  } deriving (Show)
