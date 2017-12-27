import Clay

import Typography
import Post
import Code
import Layout

main :: IO ()
main = putCss $ do
  layoutStyle
  textStyle
  postListStyle
  codeStyle
