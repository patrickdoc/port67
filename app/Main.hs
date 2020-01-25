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
import           Text.Mustache
import           Text.Mustache.Compile
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
  liftIO . putStrLn $ "Rebuilding post: " <> show p
  let postUrl = srcPath -<.> "html"
      enrichedPost = EnrichedPost { post = p, url = postUrl, subPosts = [] }
      -- Add additional metadata
      fullPostData = withSiteMeta $ toJSON enrichedPost
  liftIO . putStrLn $ "Compiling post template"
  template <- compileTemplate' "site/templates/post.html"
  liftIO . putStrLn $ "Writing post"
  writeFile' (outputFolder </> postUrl) . T.unpack $ substitute template fullPostData
  return enrichedPost
buildPost' srcPath p = do
  liftIO . putStrLn $ "Rebuilding index: " <> show p
  -- Build "subFiles"
  posts' <- forP (map ((takeDirectory srcPath) </>) (getSubFiles p)) buildIndex

  -- Create this index
  let postUrl = srcPath -<.> "html"
      enrichedPost = EnrichedPost { post = p, url = postUrl, subPosts = posts' }
      -- Add additional metadata
      fullPostData = withSiteMeta $ toJSON enrichedPost
  template <- indexTemplate
  let (errors, val) = checkedSubstitute template fullPostData
  liftIO . putStrLn $ "Errors: " <> show errors
  writeFile' (outputFolder </> postUrl) . T.unpack $ val
  return enrichedPost

indexTemplate :: Action Template
indexTemplate = do
  -- Get post-list partial
  postListTemplate <- readFile' "site/templates/post-list.html"
  let (Right res) = compileTemplate "site/templates/post-list.html" (T.pack postListTemplate)
  -- Get post-list-item partial
  postListItemTemplate <- readFile' "site/templates/post-list-item.html"
  (Right res2) <- liftIO $ compileTemplateWithCache
                                ["."]
                                (cacheFromList [res])
                                "site/templates/post-list-item.html"
  (Right res3) <- liftIO $ compileTemplateWithCache
                                ["."]
                                (cacheFromList [res2])
                                "site/templates/post-list.html"
  let cache = cacheFromList [res2, res3]
  liftIO . putStrLn $ "cache: " <> show cache
  result <- liftIO $ compileTemplateWithCache ["."] cache "site/templates/index.html"
  case result of
    Right templ -> do
      liftIO . putStrLn $ "post-list-item: " <> show res2
      liftIO . putStrLn $ "post-list" <> show res
      liftIO . putStrLn $ "index: " <> show templ
      return templ
    Left err -> fail $ show err

otherCompileTemplate' :: FilePath -> Action Template
otherCompileTemplate' fp = do
  templateContent <- readFile' fp
  let result = compileTemplate (takeFileName fp) (T.pack templateContent)
  case result of
    Right templ -> do
      need (getPartials . ast $ templ)
      return templ
    Left err -> fail $ show err

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
