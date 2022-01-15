import System.IO
import System.Environment

main :: IO ()
main = do
  (regex : _) <- getArgs
  print regex
