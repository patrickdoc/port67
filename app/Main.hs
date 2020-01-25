{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Aeson                 as A
import           Data.Aeson.Lens
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward
import           Development.Shake.FilePath
import           GHC.Generics               (Generic)
import           Slick
import           Slick.Pandoc
import           Text.Pandoc.Options
import           Text.Pandoc.Writers

import qualified Data.HashMap.Lazy as HML
import qualified Data.Text                  as T

import           Port67                     (port67Reader, port67Writer)

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
    SiteMeta { siteAuthor = "Patrick Dougherty"
             , baseUrl = "https://port67.org"
             , siteTitle = "Port67"
             }

outputFolder :: FilePath
outputFolder = "docs"

inputFolder :: FilePath
inputFolder = "site"

--Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ HML.union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta =
    SiteMeta { siteAuthor    :: String
             , baseUrl       :: String
             , siteTitle     :: String
             }
    deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for a blog post
data Post =
  Post
    { title    :: String
    , content  :: String
    , subFiles :: Maybe [String]
    } deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

data EnrichedPost =
  EnrichedPost
    { post     :: Post
    , url      :: String
    , subPosts :: [EnrichedPost]
    } deriving (Generic, Show, FromJSON, ToJSON, Binary)

getSubFiles :: Post -> [String]
getSubFiles p = case (subFiles p) of
    Nothing -> []
    Just x -> x

-- | Custom Markdown to HTML converter
mdToPort67' :: T.Text -> Action Post
mdToPort67' =
  mdToHTMLWithRdrWrtr' port67Reader port67Writer

-- | Build root index
buildIndex :: FilePath -> Action EnrichedPost
buildIndex srcPath = do
  -- load post and metadata
  postContent <- readFile' (inputFolder </> srcPath)
  postData <- mdToPort67' . T.pack $ postContent

  buildPost' srcPath postData

buildPost' :: FilePath -> Post -> Action EnrichedPost
buildPost' srcPath p@(Post _ _ Nothing) = do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  let postUrl = srcPath -<.> "html"
      enrichedPost = EnrichedPost { post = p, url = postUrl, subPosts = [] }
      -- Add additional metadata
      fullPostData = withSiteMeta $ toJSON enrichedPost
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> postUrl) . T.unpack $ substitute template fullPostData
  return enrichedPost
buildPost' srcPath p = do
  liftIO . putStrLn $ "Rebuilding index: " <> srcPath
  -- Build "subFiles"
  posts' <- forP (map ((takeDirectory srcPath) </>) (getSubFiles p)) buildIndex

  -- Create this index
  let postUrl = srcPath -<.> "html"
      enrichedPost = EnrichedPost { post = p, url = postUrl, subPosts = posts' }
      -- Add additional metadata
      fullPostData = withSiteMeta $ toJSON enrichedPost
  template <- compileTemplate' "site/templates/index.html"
  writeFile' (outputFolder </> postUrl) . T.unpack $ substitute template fullPostData
  return enrichedPost

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
    filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
    void $ forP filepaths $ \filepath ->
        copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  indices <- buildIndex "posts/index.md"
  copyStaticFiles

-- | Opening hooks for custom readers and writers
mdToHTMLWithRdrWrtr'
    :: (FromJSON a)
    => (ReaderOptions -> PandocReader T.Text)
    -> (WriterOptions -> PandocWriter)
    -> T.Text
    -> Action a
mdToHTMLWithRdrWrtr' rdr wrtr txt =
  loadUsing'
    (rdr defaultMarkdownOptions)
    (wrtr defaultHtml5Options)
    txt

main :: IO ()
main = do
  let shOpts = shakeOptions { shakeVerbosity = Chatty, shakeLintInside = ["site/"] }
  shakeArgsForward shOpts buildRules
