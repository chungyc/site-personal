-- |
-- Description: Extra stylesheet generation for error pages.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: yoo.chul.chung@gmail.com
module Web.Site.Styles.Error (style) where

import Clay hiding (style)
import Prelude hiding (div)

-- |
-- Style for error pages which Clay will render into a stylesheet.
style :: Css
style = do
  -- For single letters used like a big image all by itself.
  -- E.g., a big question mark for a 404 page.
  div # ".letter-image" ? fontSize (cm 10)
