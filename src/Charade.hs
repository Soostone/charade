{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Lens
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read
import           Heist
import           Heist.Interpreted
import           Snap
import           Snap.Snaplet.Heist
import           System.Random
import           Test.QuickCheck.Gen
import           Text.XmlHtml
------------------------------------------------------------------------------
import           Heist.Charade

------------------------------------------------------------------------------
-- | Not sure whether we want these types specifically reified into an
-- enumeration like this or to make them dynamic strings.  At any rate, I'm
-- putting it like this now to record my thoughts on some of the types needed.
data GenTypes
  = IntT
  -- ^ Integers
  | RealT
  -- ^ Reals
  | EnumTextT
  -- ^ Text that comes from an enumeration.  This is useful for generating
  -- things like first names, last names, and other lists.
  | ChildLoopT
  -- ^ This type would be used on a heist tag that functions as a
  -- runChildren-style loop.
  deriving (Read, Show, Eq, Enum)


------------------------------------------------------------------------------
-- | Integer generation
genInt :: [Text] -> Gen [Node]
genInt [] = genInt' 0 100
genInt [a,b] = genInt' (read $ T.unpack a) (read $ T.unpack b)
genInt _ = error "invalid number of parameters to int generator"

genInt' :: Int -> Int -> Gen [Node]
genInt' a b = fmap ((:[]) . TextNode . T.pack . show) $ choose (a,b)


------------------------------------------------------------------------------
-- | Real generation
genReal :: [Text] -> Gen [Node]
genReal [] = genReal' 0 1
genReal [a,b] = genReal' (read $ T.unpack a) (read $ T.unpack b)
genReal _ = error "invalid number of parameters to real generator"

genReal' :: Double -> Double -> Gen [Node]
genReal' a b = fmap ((:[]) . TextNode . T.pack . show) $ choose (a,b)


------------------------------------------------------------------------------
-- | Loop generation
genLoop :: Node -> [Text] -> Gen [Node]
genLoop node [] = genLoop' node 5
genLoop node [n] = genLoop' node $ either error fst (decimal n)
genLoop node [a,b] = do
    let minCount = either error fst (decimal a) :: Int
        maxCount = either error fst (decimal b) :: Int
    count <- choose (minCount, maxCount)
    genLoop' node count
genLoop _ _ = error "invalid number of parameters to real generator"

genLoop' :: Node -> Int -> Gen [Node]
genLoop' node count =
    liftM concat $ vectorOf count $ liftM concat
                 $ mapM fakeNode (childNodes node)

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
fakeNode :: Node -> Gen [Node]
fakeNode n@(Element t a c) = case getAttribute "fake" n of
    Nothing -> do
      c' <- liftM concat $ mapM fakeNode c
      return [Element t a c']
    Just ty -> dispatchGenerator n (T.splitOn " " ty)
fakeNode n = return [n]


-- | Use the first token as the generator type and the rest of the list as
-- parameters.
dispatchGenerator :: Node -> [Text] -> Gen [Node]
dispatchGenerator _ [] = return []
dispatchGenerator node (_type:params) =
    case T.unpack _type of
      "int"     -> genInt params
      "decimal" -> genReal params
      "loop"    -> genLoop node params
      _         -> error "generator type not recognized"


--charadeSplice :: Monad n => Splice n
charadeSplice = do
    (Element n attrs ch1) <- getParamNode
    stdGen <- liftIO getStdGen
    let ch2 = unGen (mapM fakeNode ch1) stdGen 1
    ch3 <- runNodeList (concat ch2)
    stopRecursion
    return [Element n attrs ch3]


------------------------------------------------------------------------------
-- The web app
------------------------------------------------------------------------------

data App = App
    { _heist :: Snaplet (Heist App)
    , _randomSource :: Maybe StdGen
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
    return $ App h Nothing

main :: IO ()
main = do
  (_,s,_) <- runSnaplet Nothing charadeInit
  quickHttpServe s
