{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module MyLib where
import Data.Text
import System.Directory
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import Data.Fixed (Fixed(MkFixed))
import GHC.StaticPtr (IsStatic)
import Data.String
import qualified GitHub as GH

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype PkgName = MkPkgName Text deriving (IsString)

data PkgSrc = FileSysRef Text deriving (Show)

getPkgSrc :: PkgName -> IO ByteString
getPkgSrc (MkPkgName name) = runReq defaultHttpConfig $ do
  responseBody <$> req GET (https "hackage.haskell.org" /: "package" /: name /: "src" ) NoReqBody bsResponse mempty

getPkgCabalFile :: PkgName -> IO ByteString
getPkgCabalFile (MkPkgName name) = runReq defaultHttpConfig $ do
  responseBody <$> req GET (https "hackage.haskell.org" /: "package" /: name /: name <> ".cabal" ) NoReqBody bsResponse mempty

getGitZip :: GH.Name GH.Owner -> GH.Name GH.Repo -> Text -> IO ByteString
getGitZip (GH.untagName -> owner) (GH.untagName -> repoName) branchOrTagName = runReq defaultHttpConfig $ do
  responseBody <$> req GET (https "github.com" /: owner /: repoName /: "archive" /: branchOrTagName <> ".zip") NoReqBody bsResponse mempty

