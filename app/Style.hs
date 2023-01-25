module Main (main) where

import Clay
import Web.Site.Style (stylesheet)

main :: IO ()
main = putCss stylesheet
