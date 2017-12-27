{-# LANGUAGE OverloadedStrings #-}

module Code (codeStyle) where

import           Clay
import           Prelude hiding (div, span)

lineNumbersStyle :: Css
lineNumbersStyle = do
  borderRight  solid (px 1) "#AAAAAA"
  textAlign    (alignSide sideRight)
  color        "#AAAAAA"
  paddingRight (px 5)
  paddingLeft  (px 5)

codeFont :: Css
codeFont = do
  fontFamily     ["Monaco", "Courier New"] [monospace]
  textRendering  optimizeLegibility

sourceCodeContainerStyle :: Css
sourceCodeContainerStyle = do
  paddingLeft (px 5)
  codeFont

sourceCodeStyle :: Css
sourceCodeStyle = do
  border solid (px 1) transparent
  sym borderRadius (px 5)
  backgroundColor "#efeff4"

  span # ".dt" ? color "#902000"
  span # ".dv" ? color "#40a070"
  span # ".bn" ? color "#40a070"
  span # ".fl" ? color "#40a070"
  span # ".ch" ? color "#4070a0"
  span # ".st" ? color "#4070a0"
  span # ".fu" ? color "#06287e"

  span # ".kw" ? do
    color      "#007020"
    fontWeight bold

  span # ".co" ? do
    color "#60a0b0"
    fontStyle italic

  span # ".ot" ? color "#007020"

  span # ".al" ? do
    color red
    fontWeight bold

  span # ".er" ? do
    color red
    fontWeight bold

codeStyle :: Css
codeStyle =  do
  td # ".lineNumbers" ? lineNumbersStyle
  td # ".sourceCode"  ? sourceCodeContainerStyle
  div # ".sourceCode" ? sourceCodeStyle
