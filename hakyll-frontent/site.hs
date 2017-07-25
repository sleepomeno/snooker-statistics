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
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    let showPlayer = do
          route $ setExtension "html"
          compile $ getResourceBody >>= loadAndApplyTemplate "templates/default.html" defaultContext >>= relativizeUrls

    let ranges' = ["all", "ten", "twenty", "thirty", "fifty", "hundred"] :: [String]
        ranges fileType = flip map ranges' $ \range -> fromGlob $ "breaks/" <> range <> "-*." <> fileType
    let rivalries fileType = fromGlob $ "rivalries/*." <> fileType

    match (rivalries "html") $ showPlayer
    match (rivalries "png") $ do
      route idRoute
      compile copyFileCompiler

    match "breaks/player-*.html" showPlayer

    match "breaks/*.png" $ do
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
        playerPages <- loadAll "breaks/*.html"
        rivalries' <- loadAll (rivalries "html")
        let indexCtx = listField "players" defaultContext (return playerPages) `mappend` listField "rivalries" defaultContext (return rivalries') `mappend` constField "title" "Home" `mappend` defaultContext
        getResourceBody >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls 
        
    match "templates/*" $ compile templateCompiler
