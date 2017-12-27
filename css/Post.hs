{-# LANGUAGE OverloadedStrings #-}

module Post (postListStyle) where

import Clay
import qualified Clay.Media as Media

postListStyle :: Css
postListStyle = do
  ".postList" ? do
    query Clay.all [Media.minWidth 960] $ do
      maxWidth     (45 :: Size Percentage)
      sym2 padding (px 5) (px 10)
      display      inlineBlock
      verticalAlign vAlignTop
