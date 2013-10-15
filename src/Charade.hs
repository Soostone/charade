{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

-------------------------------------------------------------------------------
import           Control.Lens hiding (elements)
import qualified Data.Configurator as C
import           Data.Maybe
import           Data.Monoid
import           Heist
import           Heist.Charade
import           Snap
import           Snap.Extras.SpliceUtils.Interpreted
import           Snap.Extras.Tabs
import           Snap.Util.FileServe
import           Snap.Snaplet.Heist
import           System.FilePath
------------------------------------------------------------------------------


data App = App
    { _heist :: Snaplet (Heist App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

------------------------------------------------------------------------------
-- The web app
------------------------------------------------------------------------------

charadeAppInit :: SnapletInit App App
charadeAppInit = makeSnaplet "charade" "A heist charade" Nothing $ do
    rootDir <- getSnapletFilePath
    cfg <- getSnapletUserConfig
    tdir <- liftM (fromMaybe (error "Must specify tdir in charade.cfg")) $
              liftIO $ C.lookup cfg "tdir"
    staticRoute <- liftIO $ C.lookupDefault
      (error "charade: Must specify staticRoute in charade.cfg")
      cfg "staticRoute"
    staticDir <- liftIO $ C.lookupDefault
      (error "charade: Must specify staticDir in charade.cfg")
      cfg "staticDir"

    -- I didn't use the "templates" directory like we usually use.  This
    -- probably needs to be a configurable parameter.
    h <- nestSnaplet "heist" heist $
           heistInit' "" $ mempty { hcLoadTimeSplices = defaultLoadTimeSplices
                                  , hcInterpretedSplices =
                                      "staticscripts" ## scriptsSplice "static/js" "/"
                                  }
    addRoutes [ ("",          heistServe)
              , ("heist/heistReload", with heist $ failIfNotLocal heistReloader)
              , (staticRoute, serveDirectory staticDir)
              ]

    initTabs h
    addTemplatesAt h "" (rootDir </> tdir)

    charadeInit h cfg
    return $ App h

main :: IO ()
main = do
  (_,s,_) <- runSnaplet (Just "charade") charadeAppInit
  quickHttpServe s

