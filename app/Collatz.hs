import Diagrams.Backend.Rasterific
import Diagrams.Prelude
import Diagrams.Sequence
import Mathematics.Collatz

main :: IO ()
main = do
  renderRasterific "site/images/article/math/collatz/length/10000.png" (dims2D 1600 1000) $ plot $ map collatzLength [1..10000]
  renderRasterific "site/images/article/math/collatz/length/100000.png" (dims2D 1600 1000) $ plot $ map collatzLength [1..100000]
  renderRasterific "site/images/article/math/collatz/length/1000000.png" (dims2D 1600 1000) $ plot $ map collatzLength [1..1000000]
