-- |
-- Description: Tests for "Web.Site.Route."
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.RoutesSpec (spec) where

import Data.List (intercalate)
import Hakyll
import Hakyll.Core.Provider
import qualified Hakyll.Core.Store as Store
import System.IO.Temp
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Web.Site.Routes

spec :: Spec
spec = do
  describe "dropExtensions" $ do
    describe "drops all extensions" $ do
      prop "for relative file path" $
        forAll (listOf chooseComponent) $ \dirs ->
          forAll chooseComponent $ \basename ->
            forAll (listOf chooseComponent) $ \exts ->
              let directory = concat [dir ++ "/" | dir <- dirs]
                  filename = directory ++ basename
                  fullname = filename ++ "." ++ intercalate "." exts
               in testRoute dropExtensions fullname `shouldReturn` (Just filename, False)

      prop "for absolute file path" $
        forAll (listOf chooseComponent) $ \dirs ->
          forAll chooseComponent $ \basename ->
            forAll (listOf chooseComponent) $ \exts ->
              let directory = "/" ++ concat [dir ++ "/" | dir <- dirs]
                  filename = directory ++ basename
                  fullname = filename ++ "." ++ intercalate "." exts
               in testRoute dropExtensions fullname `shouldReturn` (Just filename, False)

    describe "does not drop non-existent extension" $ do
      prop "for relative file path" $
        forAll (listOf chooseComponent) $ \dirs ->
          forAll chooseComponent $ \basename ->
            let directory = concat [dir ++ "/" | dir <- dirs]
                filename = directory ++ basename
             in testRoute dropExtensions filename `shouldReturn` (Just filename, False)

      prop "for absolute file path" $
        forAll (listOf chooseComponent) $ \dirs ->
          forAll chooseComponent $ \basename ->
            let directory = "/" ++ concat [dir ++ "/" | dir <- dirs]
                filename = directory ++ basename
             in testRoute dropExtensions filename `shouldReturn` (Just filename, False)

testRoute :: Routes -> FilePath -> IO (Maybe FilePath, UsedMetadata)
testRoute routes filepath = withSystemTempDirectory "test" $ \tmpdir -> do
  store <- Store.new True tmpdir
  provider <- newProvider store (const $ pure False) tmpdir
  runRoutes routes provider $ fromFilePath filepath

-- | Generate an arbitrary component of a file path.
--
-- I.e., a string with no '.' or '/'.
chooseComponent :: Gen String
chooseComponent = listOf1 character
  where
    character = arbitraryPrintableChar `suchThat` flip notElem ['.', '/']
