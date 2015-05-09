--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Data.Monoid ((<>), mconcat)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    let showPlayer = do
          route $ setExtension "html"
          compile $ pandocCompiler >>= loadAndApplyTemplate "templates/default.html" defaultContext >>= relativizeUrls

    let ranges' = ["all", "ten", "twenty", "thirty", "fifty", "hundred"] :: [String]
        ranges fileType = flip map ranges' $ \range -> fromGlob $ "breaks/" <> range <> "-*." <> fileType

    flip mapM (ranges "markdown") $ \url -> match url showPlayer

    flip mapM (ranges "png") $ \url -> match url $ do
      route idRoute
      compile copyFileCompiler

    match "lastMatches.markdown" $ do
      route $ constRoute "lastMatches.html"
      compile $ pandocCompiler >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        pandocCompiler
        playerPages <- mapM loadAll (ranges "markdown")
        let indexCtx = mconcat (zipWith (\str posts -> listField str defaultContext (return posts)) ranges' playerPages)
                       `mappend` constField "title" "Home" `mappend` defaultContext
        getResourceBody >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls 
        
    -- match "index.html" $ do
    --     route $ constRoute "lastMatches.markdown"
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let indexCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Home"                `mappend`
    --                 defaultContext

    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --             >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

-- withPosts = do
--     pandocCompiler
--     posts <- loadAll "breaks/*"
--     let indexCtx =
--             listField "posts" defaultContext (return posts) `mappend`
--             constField "title" "Home"                `mappend`
--             defaultContext
--     getResourceBody >>= applyAsTemplate indexCtx
--         >>= loadAndApplyTemplate "templates/default.html" indexCtx
--         >>= relativizeUrls 
