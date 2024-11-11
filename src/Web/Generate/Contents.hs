-- |
-- Description: Support generation of table of contents in programmatically generated pages on this website.
-- Copyright: Copyright (C) 2024 Yoo Chung
-- License: All rights reserved
-- Maintainer: web@chungyc.org
--
-- Support generation of table of contents in programmatically generated pages on this website.
-- This is for pages where the HTML is directly generated using Haskell code.
--
-- For example, the following generates HTML for a table of contents:
--
-- >>> import Text.Blaze.Html.Renderer.Pretty (renderHtml)
-- >>> :{
-- putStr $ renderHtml$ toc $
--   [ Entry "First section" "#first"
--     [ Entry "First sub-section" "#first-sub" []
--     , Entry "Second sub-section" "#second-sub" []
--     ]
--   , Entry "Second section" "#second" []
--   ]
-- :}
-- <nav class="toc">
--     <h2>
--         Contents
--     </h2>
--     <ul>
--         <li>
--             <a href="#first">
--                 First section
--             </a>
--             <ul>
--                 <li>
--                     <a href="#first-sub">
--                         First sub-section
--                     </a>
--                 </li>
--                 <li>
--                     <a href="#second-sub">
--                         Second sub-section
--                     </a>
--                 </li>
--             </ul>
--         </li>
--         <li>
--             <a href="#second">
--                 Second section
--             </a>
--         </li>
--     </ul>
-- </nav>
module Web.Generate.Contents (Entry (..), toc) where

import Control.Monad (unless)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

-- | An entry in the table of contents.
data Entry
  = Entry
      -- | Title of the section.
      Html
      -- | Anchor to the section.
      AttributeValue
      -- | Sub-entries under the section.
      [Entry]

-- | Generate HTML for the table of contents.
toc :: [Entry] -> Html
toc xs = nav ! class_ "toc" $ do
  h2 "Contents"
  toList xs

-- | Generate an HTML list from a list of entries in a table of contents.
toList :: [Entry] -> Html
toList xs = ul $ mapM_ fromEntry xs

-- | Generate the HTML for an entry in a table of contents.
fromEntry :: Entry -> Html
fromEntry (Entry title' anchor subentries) = do
  li $ do
    a ! href anchor $ title'
    unless (null subentries) $ toList subentries
