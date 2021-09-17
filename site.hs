--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad   (liftM)
import           Data.Char       (toLower)
import           Data.Monoid     ((<>))
import           Hakyll
import qualified Text.Pandoc      as Pandoc
import           System.FilePath (replaceExtension)
--------------------------------------------------------------------------------

withToc = defaultHakyllWriterOptions
    { Pandoc.writerTableOfContents = True
    , Pandoc.writerTemplate        = Just tocTemplate
    }
tocTemplate =
    either error id $ either (error . show) id $
    Pandoc.runPure $ Pandoc.runWithDefaultPartials $
    Pandoc.compileTemplate "" "\n$toc$\n$body$"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "akhramov personal blog"
    , feedDescription = "Latest posts"
    , feedAuthorName  = "Artem Khramov"
    , feedAuthorEmail = "akhramov+blog@pm.me"
    , feedRoot        = "https://akhramov.github.io"
    }

setExtensionAndLower :: String -> Routes
setExtensionAndLower extension = customRoute $
 (map toLower) . (`replaceExtension` extension) . toFilePath

main :: IO ()
main = hakyll $ do
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" ("Tag: " ++ tag)
                      `mappend` listField "posts" (postCtx tags) (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "images/**/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/Default.hs" $ do
      route   $ setExtensionAndLower "css"
      compile $ getResourceString >>= withItemBody (unixFilter "runhaskell" ["-icss"])

    match "about.org" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          underlying <- getUnderlying
          toc        <- getMetadataField underlying "tableOfContents"
          let writerOptions' = maybe defaultHakyllWriterOptions (const withToc) toc

          pandocCompilerWith defaultHakyllReaderOptions writerOptions'
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) <>
                    constField "title" "Archives"                   <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["rss.xml"] $ do
            route idRoute
            compile $ do
                loadAllSnapshots "posts/*" "content"
                    >>= recentFirst
                    >>= renderRss myFeedConfiguration (postCtx tags)

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom myFeedConfiguration (postCtx tags) posts

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- liftM (take 3) (recentFirst =<< loadAll "posts/*")

            let indexCtx =
                    listField "posts" (postCtx tags) (return posts) <>
                    constField "title" "home"                       <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags =
    dateField "date" "%B %e, %Y" <>
    tagsField "tags" tags <>
    defaultContext
