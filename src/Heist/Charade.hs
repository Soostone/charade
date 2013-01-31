{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Heist.Charade
  ( charadeProductionSplice
  ) where

import           Heist
import           Heist.Interpreted
import           Text.XmlHtml


------------------------------------------------------------------------------
-- | Charade needs to provide a splice that can be bound to eliminate all
-- charade-specific attributes from the markup.
charadeProductionSplice :: Monad n => Splice n
charadeProductionSplice = return . (:[]) . removeFake =<< getParamNode


removeFake :: Node -> Node
removeFake (Element tag attrs ch) = Element tag attrs' $ map removeFake ch
  where
    attrs' = filter ((/="fake") . fst) attrs


