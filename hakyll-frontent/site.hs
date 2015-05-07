--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- match (fromList ["about.rst", "contact.markdown"]) $ do
    --     route   $ setExtension "html"
    --     compile $ pandocCompiler
    --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --         >>= relativizeUrls

    match "breaks/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
        -- compile withPosts


    match "lastMatches.markdown" $ do
      route $ constRoute "lastMatches.html"
      compile $ pandocCompiler >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


    match "index.html" $ do
      route idRoute
      compile withPosts
        
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

withPosts = do
    pandocCompiler
    posts <- loadAll "breaks/*"
    let indexCtx =
            listField "posts" defaultContext (return posts) `mappend`
            constField "title" "Home"                `mappend`
            defaultContext
    getResourceBody >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls 
