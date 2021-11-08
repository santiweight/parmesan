{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Archive.Zip
import Control.Exception (assert)
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Foldable
import Data.List.NonEmpty
import Data.Proxy
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Distribution.Fields (Field, Name (Name), readFields, runParseResult)
import Distribution.PackageDescription (GenericPackageDescription (packageDescription), PackageDescription (sourceRepos), SourceRepo (repoLocation, repoBranch))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, readGenericPackageDescription)
import GHC.IO
import GHC.IO.Exception (assertError)
import qualified GitHub as GH
import qualified GitHub.Data.Repos as GH
import qualified GitHub.Endpoints.Repos as GH
import MyLib
import System.Directory
import System.FilePath
import System.Process (CreateProcess (cwd, std_in, std_out), callCommand, callProcess, createProcess, proc, readCreateProcess)
import qualified Text.Regex.TDFA as Re
import Text.URI
import Data.Maybe
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle.FD (openFile)
import GHC.Generics
import Text.Printf (printf)

data Config = Config { dryRun :: Bool } deriving (Show, Eq, Read, Generic)

migrate 

main :: IO ()
main = do
  currDir <- getCurrentDirectory
  flip assertError (pure ()) $ currDir == "/home/santiweight/quack"
  tmpExists <- doesDirectoryExist tmpFolderPath
  when tmpExists $
    removeDirectoryRecursive tmpFolderPath
  createTmpFolder
  createDirectoryIfMissing False $ tmpFolderPath </> "srcs"
  srcBs <- getPkgSrc "poker-base"
  cabalBs <- getPkgCabalFile "poker-base"
  -- print $ B.take 100 srcBs
  let ([], Right res) = runParseResult $ parseGenericPackageDescription cabalBs
  let pkgDesc = packageDescription res
  let (srcRepo : _) = sourceRepos pkgDesc
  let Just repoLoc = repoLocation srcRepo
  let Just srcRepoUri = mkURI @Maybe . T.pack $ repoLoc
  let Just (_, fmap unRText -> (usr :| (repoName : _))) = uriPath srcRepoUri
  let ghUsr = GH.mkName Proxy usr
  let ghRepoName = GH.mkName Proxy repoName
  Right repo <- GH.github' $ GH.repositoryR ghUsr ghRepoName
  let branchName = T.pack $ fromMaybe "main" $ repoBranch srcRepo
  repoZip <- getGitZip ghUsr ghRepoName branchName
  let repoZipPath = tmpFolderPath </> "foo"
  B.writeFile repoZipPath repoZip
  withArchive repoZipPath (unpackInto $ tmpFolderPath </> "bar")
  let eqNoNeqGoldenFolder = testFolderPath </> "golden" </> "eq-no-neq"
  let cmd = "sed -r '/^\\s+[a-zA-Z()0-9 ]*\\/=[a-zA-Z()0-9 ]*\\s+=.*$/d' <" <> (eqNoNeqGoldenFolder </> "case-02.in") <> " >" <> (eqNoNeqGoldenFolder </> "case-02.out")
  callCommand cmd
  let cloneCmd = "git clone " <> repoLoc
  gitClone (tmpFolderPath </> "srcs") srcRepoUri
  -- migrate eqNoNeqMigration (eqNoNeqGoldenFolder </> "case-02.in") (eqNoNeqGoldenFolder </> "case-02.out")
  let ?config = Config False
  -- migrateRepo eqNoNeq  (tmpFolderPath </> "srcs" </> T.unpack repoName)
  let repoFp = tmpFolderPath </> "srcs" </> T.unpack repoName
  migrateRepo eqNoNeq repoFp
  withCurrentDirectory repoFp $ do
    callCommand "git add ."
    let commitMsg :: String = "Eq of no Neq"
    callCommand $ printf "git commit -m \"%s\""  commitMsg
  pure ()

deleteAllContentsMigration = TextBased $ const ""

eqNoNeq = TextBased eqNoNeqMigration

data Migration = TextBased (Text -> Text)

migrateRepo :: (?config :: Config) => Migration -> FilePath -> IO ()
migrateRepo (TextBased f) fp = do
  allFiles <- getFilesRecursive fp
  print allFiles
  let hsFiles :: [FilePath] = T.unpack <$> Prelude.filter (".hs" `isSuffixOf`) (T.pack <$> allFiles)
  print hsFiles
  (\p -> migrate f p p) `mapM_` hsFiles
  readCreateProcess (proc "git" ["diff"]) {cwd = Just fp} ""
  pure ()

gitClone :: FilePath -> URI -> IO ()
gitClone relativeFp uri = do
  readCreateProcess (proc "git" ["clone", renderStr uri]) {cwd = Just relativeFp} ""
  pure ()

migrate :: (Text -> Text) -> FilePath -> FilePath -> IO ()
migrate f inPath outPath = do
  file <- T.readFile inPath
  T.writeFile outPath $ f file
  pure ()

eqNoNeqMigration :: Text -> Text
eqNoNeqMigration = id

sedProc inFile outFile =
  createProcess
    ( proc
        "sed"
        [ "-r",
          ""
        ]
    )
      { cwd = Just "/home/santiweight/quack/",
        std_in = inFile,
        std_out = outFile
      }

eqNoNeqRegex = "^\\s+([a-zA-Z0-9()\\s]* /= [a-zA-Z0-9()\\s]*|\\(/=\\))\\s+=.*$"

tmpFolderPath = "." </> "tmp"

testFolderPath = "/home/santiweight/quack/" </> "test"

createTmpFolder = createDirectoryIfMissing False tmpFolderPath

-- | Recursively get all subdirectories in the given directory.
--
-- @since 0.2.1.0
getSubdirsRecursive :: FilePath -> IO [FilePath]
getSubdirsRecursive = getDirFiltered doesDirectoryExist

-- | Recursively get all files and subdirectories in the given directory.
getDirRecursive :: FilePath -> IO [FilePath]
getDirRecursive = getDirFiltered (const $ pure True)

-- | Recursively get all files in the given directory.
--
-- @since 0.2.3.0
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive fp = getDirRecursive fp >>= filterM doesFileExist

{-# INLINE getDirFiltered #-}

-- | Recursively get all files and subdirectories in the given directory that
-- satisfy the given predicate. Note that the content of subdirectories not
-- matching the filter is ignored. In particular, that means something like
-- @getDirFiltered doesFileExist@ will /not/ recursively return all files.
--
-- @since 0.2.2.0
getDirFiltered ::
  -- | Filepath filter
  (FilePath -> IO Bool) ->
  FilePath ->
  IO [FilePath]
getDirFiltered p fp = do
  all' <- listDirectory fp
  all'' <- filterM p (mkRel <$> all')
  dirs <- filterM doesDirectoryExist all''
  case dirs of
    [] -> pure all''
    ds -> do
      next <- unsafeInterleaveIO $ foldMapA (getDirFiltered p) ds
      pure $ all'' ++ next
  where
    mkRel = (fp </>)
    foldMapA = (fmap fold .) . traverse