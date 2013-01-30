{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import Control.Lens
import Heist
import Snap
import Snap.Snaplet.Heist

------------------------------------------------------------------------------
-- | Not sure whether we want these types specifically reified into an
-- enumeration like this or to make them dynamic strings.  At any rate, I'm
-- putting it like this now to record my thoughts on some of the types needed.
data GenTypes
  = IntT
  -- ^ Integers
  | DecimalT
  -- ^ Decimals
  | EnumTextT
  -- ^ Text that comes from an enumeration.  This is useful for generating
  -- things like first names, last names, and other lists.
  | ChildLoopT
  -- ^ This type would be used on a heist tag that functions as a
  -- runChildren-style loop.
  deriving (Read, Show, Eq, Enum)

------------------------------------------------------------------------------
-- The web app
------------------------------------------------------------------------------

data App = App
    { _heist :: Snaplet (Heist App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

charadeInit :: SnapletInit App App
charadeInit = makeSnaplet "charade" "A heist charade" Nothing $ do
    -- I didn't use the "templates" directory like we usually use.  This
    -- probably needs to be a configurable parameter.
    h <- nestSnaplet "heist" heist $ heistInit ""

    addRoutes [ ("", cHeistServe) ]
    return $ App h

main :: IO ()
main = do
  (_,s,_) <- runSnaplet Nothing charadeInit
  quickHttpServe s
