-- |
-- Description: Common functions for use by stylesheets on this website.
-- Copyright: Copyright (C) 2023 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Common functions for use across stylesheets on this website.
module Web.Site.Styles (narrowWidth, wideWidth) where

import Clay

-- | The width below which a display is considered very narrow.
-- For example, on a phone screen.
narrowWidth :: Size LengthUnit
narrowWidth = em 30

-- | The width above which a display is considered very wide.
-- For example, with full screen on a wide monitor.
wideWidth :: Size LengthUnit
wideWidth = em 60
