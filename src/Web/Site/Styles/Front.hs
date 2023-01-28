-- |
-- Description: Extra stylesheet generation for front page.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Styles.Front (style) where

import Clay hiding (style)
import Prelude hiding (div)

-- |
-- Style for front page which Clay will render into a stylesheet.
style :: Css
style = do
  ".latest-update" ? do
    sym margin $ em 2
    sym padding $ em 2
    border (px 2) solid $ rgb 20 20 50
    backgroundColor $ rgb 250 250 250
