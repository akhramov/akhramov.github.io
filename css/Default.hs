import           Clay

import           Code
import           Layout
import           Post
import           Typography

main :: IO ()
main = putCss $ do
  layoutStyle
  textStyle
  postListStyle
  codeStyle
