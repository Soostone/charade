{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Lens
import           Data.Monoid
import qualified Data.Text as T
import           Heist
import           Heist.Interpreted
import           Snap
import           Snap.Snaplet.Heist
import           Text.XmlHtml

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
-- | Uses the \"fake\" attribute to determine what type of random data should
-- be generated for this node.  Usage might look something like this:
--
-- > <ul>
-- >  <personListing fake="loop 5">
-- >   <li>
-- >    <firstName fake="enum firstName"/> <lastName fake="enum lastName"/>
-- >   </li>
-- >  </personListing>
-- > </ul>
fakeNode :: Node -> [Node]
fakeNode n = case getAttribute "fake" n of
    Nothing -> [n]
    Just ty -> go (T.splitOn " " ty)
  where
    -- | Use the first token as the generator type and the rest of the list as
    -- parameters.
    go [] = []
    go (_type:params) = undefined


charadeSplice :: Monad n => Splice n
charadeSplice = do
    (Element n attrs ch) <- getParamNode
    let ch' = concat $ map fakeNode ch
    runNode $ Element n attrs ch'


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

    -- Heist doesn't have a catch-all splice, and attribute splices won't work
    -- since we want to modify the actual node, so we use a load time
    -- interpreted splice attached to the body tag.
    addConfig h $ mempty { hcLoadTimeSplices = [("body", charadeSplice)] }
    return $ App h

main :: IO ()
main = do
  (_,s,_) <- runSnaplet Nothing charadeInit
  quickHttpServe s
