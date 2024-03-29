{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}

module Layout (layoutStyle) where

import           Data.List.NonEmpty (fromList)
import           Clay
import qualified Clay.Media as Media

fullHeight :: Css
fullHeight = height (100 :: Size Percentage)

bodyStyle :: Css
bodyStyle = do
  sym margin (px 0)
  fullHeight

blockquoteStyle :: Css
blockquoteStyle = do
  boxShadow $ fromList [black `bsColor` shadow (px -4) (px 0)]
  marginLeft (px 10)
  paddingLeft (px 30)

containerStyle :: Css
containerStyle = do
  fullHeight
  maxWidth                   (px 960)
  padding                    (px 0) 15 0 15
  backgroundColor            "#fff"
  display                    grid
  "grid-template-columns" -: "2fr 8fr"
  "grid-template-rows"    -: "auto 50px"
  sym2 margin                (px 0) auto

  query Clay.all [Media.maxWidth 600] $ do
    "grid-template-columns" -: "1fr"
    "grid-template-rows"    -: "50px auto 50px"

    header <? do
      zIndex 10

      after & do
        content   $ stringContent ""
        display     block
        background  white
        width       (110 :: Size Percentage)
        zIndex      -1
        height      (px 50)
        left      $ px -10
        position    fixed
        boxShadow $ fromList [black `bsColor` shadow (px 1) (px 4)]

      "grid-column-end" -: "span 2"

      nav <? do
        display        flex
        justifyContent spaceBetween
        width          (px 220)
        marginTop      (px 14)
        marginLeft     (px 0)
        a ? do
          display inlineBlock

footerStyle :: Css
footerStyle = do
  "grid-column-end" -: "span 2"
  sym2 padding         (px 10) (px 5)

headerStyle :: Css
headerStyle = do
  nav <? do
    marginTop  (px 30)
    marginLeft (px 20)
    position   fixed
    a ? do
      display block
      sym padding (px 5)

linkStyle :: Css
linkStyle = textDecoration none

layoutStyle :: Css
layoutStyle = do
  html                   ? fullHeight
  body                   ? bodyStyle
  "#container"           ? containerStyle
  blockquote             ? blockquoteStyle
  footer                 ? footerStyle
  header                <? headerStyle
  a                      ? linkStyle
  ".pageTitle"           ? textTransform capitalize
