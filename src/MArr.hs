
import Data.Array.ST
import Data.Array

main :: IO ()
main = putStrLn$ show$ hoge --fuga

type Board = Array (Int, Int) String

--fuga :: Board
---fuga = newArray (mi, ma) "Aho"

hoge :: Board
hoge = runSTArray$ do
  t <- newArray ((1,1), (2,2)) "Aho" :: MArray (Int, Int) String
--  t2 <- newArray ((1,1), (2,2)) "Aho" :: MArray (Int, Int) String
  writeArray t (1,2) "Baka"
--  writeArray t2 (2,2) "Baka"
  return t
