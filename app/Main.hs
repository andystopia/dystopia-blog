{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM, forM_, forever, mfilter, unless, when)
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM, try)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class
import Data.Either (fromRight)
import Data.Function ((&))
import Data.List (elemIndices, sortOn, zip4)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (fromStrict, toStrict)
import qualified Data.Text.Lazy.Encoding as BL
import Network.HTTP.Types (status500)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, acceptRequest, forkPingThread, receiveData, sendTextData)
import Network.WebSockets.Connection (defaultConnectionOptions)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents, listDirectory, canonicalizePath)
import System.FilePath (splitDirectories, splitExtension, splitFileName, (</>), replaceExtension)
import Text.DocTemplates (Context, toContext)
import Text.Pandoc
  ( Pandoc (Pandoc),
    PandocError (PandocFormatError, PandocResourceNotFound),
    PandocIO,
    PandocMonad (readFileLazy),
    readMarkdown,
    renderError,
  )
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Highlighting (breezeDark)
import Text.Pandoc.Options
import Text.Pandoc.Readers (readRST)
import Text.Pandoc.Templates
import Text.Pandoc.Writers (writeHtml5String)
import Text.Pandoc.Writers.Shared (lookupMetaString, setField)
import Web.Scotty
import System.Environment (getArgs)

-- a valid route is either...
data RouteKind
  = -- a blog post (an "article")
    Article
  | -- a collection of blog posts
    ArticleCollection
  | -- a topic collection (collection of article collections)
    TopicCollection
  | -- or it is not found, it must be one of these options
    RouteNotFound

class (Functor m, Applicative m, Monad m, MonadError PandocError m, PandocMonad m) => DystopiaMonad m where
  findArticle :: String -> m [FilePath]
  articleToPandoc :: FilePath -> m Pandoc
  listOfTopics :: FilePath -> m [FilePath]
  listOfArticles :: FilePath -> m [FilePath]
  renderArticle :: FilePath -> m T.Text
  renderTopics :: FilePath -> m T.Text
  renderCollection :: FilePath -> m T.Text
  rootDir :: m String
  rootDir = return "/"

newtype DystopiaRenderer a = MkDystopiaRenderer {undystopiarenderer :: PandocIO a}
  deriving (MonadIO, Functor, Applicative, Monad, MonadError PandocError, PandocMonad, MonadThrow, MonadCatch)

runDystopiaIO :: DystopiaRenderer a -> IO (Either PandocError a)
runDystopiaIO render = runIO $ undystopiarenderer $ do
  -- throw pandoc errors back up into the pandoc monad
  x <- try render
  case x of
    Left x -> throwError x
    Right x -> return x

dystopiaReaders :: M.Map String (T.Text -> DystopiaRenderer Pandoc)
dystopiaReaders =
  M.fromList
    [ ( ".md",
        readMarkdown
          ( def
              { readerStandalone = True,
                readerExtensions = githubMarkdownExtensions <> extensionsFromList [Ext_yaml_metadata_block]
              }
          )
      ),
      (".rst", readRST (def {readerStandalone = True, readerTabStop = 8}))
    ]

dystopiaGetBreadcrumbs :: String -> DystopiaRenderer [Context T.Text]
dystopiaGetBreadcrumbs filename = do
  prefix <- rootDir
  let directory = fst . splitFileName $ filename
  let eachDir = filter (/= ".") (splitDirectories directory)
  let fullPaths = (prefix <>) <$> scanr (flip (<>)) "" eachDir
  let blog_root = toContext $ M.fromList [("url" :: T.Text, T.pack prefix), ("text", "Blog" :: T.Text)]
  let breadcrumbs :: [Context T.Text] =
        blog_root
          : fmap
            ( toContext
                . (\(name, link) -> M.fromList [("url" :: T.Text, T.pack link <> "/"), ("text", T.pack name)])
            )
            (zip eachDir fullPaths)

  return breadcrumbs

instance DystopiaMonad DystopiaRenderer where
  findArticle filepath = do
    let (directory, requestedFileName) = splitFileName filepath
    contents <- liftIO $ getDirectoryContents directory
    return $ (directory </>) <$> filter ((== requestedFileName) . fst . splitExtension) contents

  listOfArticles filepath = do
    let directory = fst (splitFileName filepath)

    dirExists <- liftIO $ doesDirectoryExist directory

    unless dirExists $ do
      liftIO $ putStrLn "throwing exceptions"
      throwM . PandocResourceNotFound . T.pack $ directory

    allFiles <- liftIO $ do
      dirContents <- listDirectory directory
      filterM (fmap not . doesDirectoryExist) dirContents

    let selectedFiles = filter ((`elem` M.keys dystopiaReaders) . snd . splitExtension) allFiles
    let finalDirectory = (fromMaybe "dystopia" . listToMaybe . reverse . splitDirectories) directory
    let metaName = directory </> finalDirectory

    articles <- findArticle metaName

    let prefixed = (directory </>) <$> selectedFiles
    let withoutIndex = filter (`notElem` articles) prefixed

    return withoutIndex

  listOfTopics filepath = do
    -- first let's just peer inside the directory
    -- a directory of topics cannot have any blog
    -- posts so we discard the filename if there is one.
    let directory = fst (splitFileName filepath)

    dirExists <- liftIO $ doesDirectoryExist directory

    unless dirExists $ do
      liftIO $ putStrLn "throwing exceptions"
      throwM . PandocResourceNotFound . T.pack $ directory

    -- get a list of subdirectories
    subdirs <- liftIO $ do
      dirContents <- listDirectory directory
      filterM doesDirectoryExist dirContents

    -- create /path/to/dir/directory/directory name so that we can see
    -- if a directory contains if a file has the same name as itself
    let topicSearchPaths = map (\fp -> fp </> (fromMaybe "dystopia" . listToMaybe . reverse . splitDirectories) fp) subdirs

    concat <$> mapM findArticle topicSearchPaths

  articleToPandoc filepath = do
    let selected = splitExtension filepath
    let supportedExts = M.keys dystopiaReaders

    case selected of
      (filePath, ext) | ext `elem` supportedExts -> do
        let reader = dystopiaReaders M.! ext

        contents <- readFileLazy (filePath <> ext)
        reader $ toStrict $ BL.decodeUtf8 contents
      _ -> do
        throwError (PandocFormatError "in reader" ("Unsupported file extension in " <> T.pack filepath))

  renderArticle filepath = do
    md <- articleToPandoc filepath
    let templatePath = "templates/default.html5"
    templateSrc <- getTemplate templatePath
    template <- runWithPartials (compileTemplate templatePath templateSrc)

    defTemplate <- compileDefaultTemplate "markdown_github"
    let template_ = fromRight defTemplate template

    breadcrumbs <- dystopiaGetBreadcrumbs filepath
    writeHtml5String
      ( def
          { writerVariables =
              writerVariables def {writerListings = True, writerHighlightStyle = Just breezeDark}
                & setField "breadcrumb" breadcrumbs,
            writerTemplate = Just template_
          }
      )
      md

  renderTopics filepath = do
    topics <- listOfTopics filepath
    pandocList <- mapM articleToPandoc topics

    articlePandoc <- do
      liftIO $ putStrLn $ "Split directories" <> (show . splitDirectories $ filepath)
      let searchName = (fromMaybe "dystopia" . mfilter (/= ".") . listToMaybe . reverse . splitDirectories) filepath
      let searchPath = filepath </> searchName
      liftIO $ putStrLn $ "Search Name: " <> searchName
      fp <- findArticle searchPath
      liftIO $ print fp
      articleToPandoc (head fp)

    let templatePath = "templates/index-template.html"

    templateCompiled <- do
      templateSrc <- getTemplate templatePath
      runWithPartials (compileTemplate templatePath templateSrc)

    case templateCompiled of
      Left err -> do
        return (T.pack err)
      Right templ -> do
        let metas = (\(Pandoc meta _) -> meta) <$> pandocList

        let titles = lookupMetaString "title" <$> metas
        let descriptions = lookupMetaString "description" <$> metas
        let urls = T.pack . fst . splitFileName <$> topics

        let articleRefs = zip3 titles descriptions urls

        breadcrumbs <- dystopiaGetBreadcrumbs filepath

        let ctx_elems :: [Context T.Text] =
              [ toContext $
                  M.fromList
                    [("title" :: T.Text, title), ("description", description), ("url", url)]
                | (title, description, url) <- articleRefs
              ]

        let ctx :: Context T.Text =
              [ ("article" :: T.Text, ctx_elems),
                ("breadcrumb", breadcrumbs)
              ]
                & M.fromList
                & toContext
                & setField "collection" True

        writeHtml5String
          ( def
              { writerVariables =
                  writerVariables def {writerListings = True, writerHighlightStyle = Just breezeDark} <> ctx,
                writerTemplate = Just templ
              }
          )
          articlePandoc

  renderCollection filepath = do
    supportedFilePaths <- listOfArticles filepath
    pandocList <- mapM articleToPandoc supportedFilePaths

    articlePandoc <- do
      let searchPath = filepath </> last (splitDirectories filepath)
      fp <- findArticle searchPath
      articleToPandoc (head fp)

    let templatePath = "templates/index-template.html"

    templateCompiled <- do
      templateSrc <- getTemplate templatePath
      runWithPartials (compileTemplate templatePath templateSrc)

    case templateCompiled of
      Left err -> do
        -- determine a real error that we can throw in this situation
        -- so that we can bubble it back into the web UI.
        return $ T.pack err
      Right templ -> do
        let metas = (\(Pandoc meta _) -> meta) <$> pandocList

        let titles = lookupMetaString "title" <$> metas
        let descriptions = lookupMetaString "description" <$> metas
        let urls = T.pack . (<> ".html") . fst . splitExtension . snd . splitFileName <$> supportedFilePaths
        let dates = lookupMetaString "date" <$> metas

        let articleRefs = zip4 titles descriptions urls dates
        let articleRefsSorted = sortOn (Down . (\(_, _, _, d) -> d)) articleRefs

        breadcrumbs <- dystopiaGetBreadcrumbs filepath

        let ctx_elems :: [Context T.Text] =
              [ toContext $
                  M.fromList
                    [("title" :: T.Text, title), ("description", description), ("url", url), ("date", date)]
                | (title, description, url, date) <- articleRefsSorted
              ]

        let ctx :: Context T.Text =
              toContext $
                M.fromList
                  [ ("article" :: T.Text, ctx_elems),
                    ("breadcrumb", breadcrumbs)
                  ]

        writeHtml5String
          ( def
              { writerVariables =
                  writerVariables def {writerListings = True, writerHighlightStyle = Just breezeDark} <> ctx,
                writerTemplate = Just templ
              }
          )
          articlePandoc
  rootDir = return "/blog/"

main :: IO ()
main = do

  args <- getArgs

  
  case args of 
    ["debug"] -> do
      let port = 3106
      let settings = setPort port defaultSettings

      sapp <- scottyPart

      putStrLn $ "Launching preview server at: http:/[::]:" <> show port <> "/" 
      runSettings settings $ websocketsOr defaultConnectionOptions wsapp sapp
    ("build":rest) -> do
      if "--help" `elem` rest then do
        putStrLn "================="
        putStrLn "Dystopia Blogging"
        putStrLn "================="
        putStrLn "Author: @andystopia"
        putStrLn "Version: 0.1.0.0"
        putStrLn ""
        putStrLn "`build` options"
        putStrLn "---------------"
        putStrLn "  --path-to-folder -- flag to specify the default blog path should be"
        putStrLn "                      mirrored to the file structure for serving."
        putStrLn ""

      else do
        build <- runDystopiaIO $ buildBlog ("--path-to-folder" `elem` rest) "./"
        case build of 
          Left err -> error $ show err
          Right () -> return ()
    _ -> do
      putStrLn "================="
      putStrLn "Dystopia Blogging"
      putStrLn "================="
      putStrLn "Author: @andystopia"
      putStrLn "Version: 0.1.0.0"
      putStrLn ""
      putStrLn "Available Subcommands"
      putStrLn "---------------------"
      putStrLn "  debug -- launch a server to preview the blog"
      putStrLn "  build -- launch a server to build the blog"
      putStrLn ""


splitFrontmatter :: T.Text -> (T.Text, T.Text)
splitFrontmatter src =
  let lns = T.lines src
      which = elemIndices "---" lns
   in case which of
        (0 : (a : _)) -> (T.unlines (take (a - 1) (drop 1 lns)), T.unlines (drop (a + 1) lns))
        _ -> ("", src)

scottyRunDystopia :: DystopiaRenderer a -> ActionM a
scottyRunDystopia dystopia = do
  res <- liftIO $ runDystopiaIO dystopia
  case res of
    Left err -> do
      status status500
      html $ fromStrict $ renderError err
      throwM err
    Right res -> return res

scottyPart :: IO Application
scottyPart = do
  rootD <- fromRight "/" <$> runDystopiaIO rootDir

  scottyApp $ do
    get (regex ("^" <> rootD <> "(.*)$")) $ do
      filepath <- captureParam "1"

      topics <- scottyRunDystopia $ listOfTopics filepath
      if null topics
        then do
          case splitExtension filepath of
            (_, ext) | ext == ".html" -> do
              innerHtml <- scottyRunDystopia $ do
                fp <- findArticle ((fst . splitExtension) filepath)

                if null fp
                  then throwM (PandocResourceNotFound $ T.pack filepath)
                  else renderArticle (head fp)

              html (fromStrict innerHtml)
            _ -> do
              written <- scottyRunDystopia $ renderCollection filepath
              html $ fromStrict written
        else do
          written <- scottyRunDystopia $ renderTopics filepath
          html $ fromStrict written

buildBlog :: Bool -> FilePath -> DystopiaRenderer ()
buildBlog pathToFolder blogDir = do
  defaultPath <- rootDir

  outputDir <- liftIO $ canonicalizePath (if pathToFolder then "./dystopia-build/" </> ("." <> defaultPath) else "./dystopia-build")
  outputDirAndBlogDir <- liftIO $ canonicalizePath (outputDir </> blogDir)


  liftIO $ print outputDirAndBlogDir

  liftIO $ createDirectoryIfMissing pathToFolder outputDir

  topics <- (fmap . fmap) (fst . splitFileName) (listOfTopics blogDir)

  if null topics
    then do
      articles <- listOfArticles blogDir

      unless (null articles) $ do
        articleListHTML <- renderCollection blogDir

        liftIO $ TIO.writeFile (outputDirAndBlogDir </> "index.html") articleListHTML

        forM_ articles $ \article -> do
          let articleShortPathHTML = replaceExtension ((snd . splitFileName) article) "html"
          let articleFullPathHTML = outputDirAndBlogDir </> articleShortPathHTML 

          rendered <- renderArticle article

          liftIO $ TIO.writeFile articleFullPathHTML rendered

        liftIO $ putStrLn $ "Articles: " <> show articles
        return ()
    else do
      topicsRendered <- renderTopics blogDir
      liftIO $ TIO.writeFile (outputDirAndBlogDir </> "index.html") topicsRendered

      forM_ topics $ \topic -> do
        liftIO $ createDirectoryIfMissing False (outputDirAndBlogDir </> topic)
        buildBlog pathToFolder topic

wsapp :: ServerApp
wsapp pending = do
  print "ws connected"
  conn <- acceptRequest pending
  forkPingThread conn 30

  (msg :: T.Text) <- receiveData conn
  sendTextData conn $ ("initial> " :: T.Text) <> msg

  forever $ do
    sendTextData conn ("loop data" :: T.Text)
    threadDelay $ 1 * 1000000
