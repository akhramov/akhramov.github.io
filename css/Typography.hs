{-# LANGUAGE OverloadedStrings #-}
module Typography (textStyle) where

import Clay

noMargin :: Css
noMargin = sym margin (px 0)

textFont :: Css
textFont = do
  fontFamily ["Europa", "Helvetica"] [sansSerif]
  textRendering  optimizeLegibility

textStyle :: Css
textStyle = do
  mapM_ (? noMargin) [h2, h3, h4, h5, h6]
  body ? textFont
