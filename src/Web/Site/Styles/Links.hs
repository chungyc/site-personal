-- |
-- Description: Extra stylesheet generation for pages in the links collection.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
module Web.Site.Styles.Links (style) where

import Clay (Css, fontSize, h2, h3, h4, h5, h6, rem, (?))
import Prelude hiding (div, rem)

-- |
-- Style for link collection pages which Clay will render into a stylesheet.
--
-- Mainly for reducint the heading sizes.
style :: Css
style = do
  h2 ? fontSize (rem 1.4)
  h3 ? fontSize (rem 1.3)
  h4 ? fontSize (rem 1.2)
  h5 ? fontSize (rem 1.1)
  h6 ? fontSize (rem 1)
