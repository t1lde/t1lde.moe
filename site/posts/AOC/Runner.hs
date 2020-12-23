import System.FilePath
--------------------------------------------------------------------------------
import System.Environment
import qualified GHC.IO.Encoding as Enc
import qualified Data.List as L
--------------------------------------------------------------------------------
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
--------------------------------------------------------------------------------
main :: IO ()
main = do
  (Enc.setLocaleEncoding Enc.utf8)

  (Just [x1 ,x2]) <- getArgs <&> traverse (readMaybe @(Int))

  runDay x1 x2

  where
    loadInput :: Int -> (String -> Maybe b) -> IO (b)
    loadInput day parse = do
      (Just inp) <- fmap parse $ readFile (basepath </> mconcat ["input", (show day), ".txt"])
      return inp

    basepath :: FilePath
    basepath =  "site/posts/AOC"

    runDay :: Int -> Int -> IO ()
    runDay 1 1 = (loadInput 1 (parseLines @(String))) >>= (Day1.run1 >>> print)
    runDay 1 2 = (loadInput 1 (parseLines @(String))) >>= (Day1.run2 >>> print)
    runDay 2 1 = (loadInput 2 (fromString >>> pure))  >>= (Day2.run1 >>> print)
    runDay 2 2 = (loadInput 2 (fromString >>> pure))  >>= (Day2.run2 >>> print)
    runDay 3 1 = (loadInput 3 (parseLines @(Text)))   >>= (Day3.run1 >>> print)
    runDay 3 2 = (loadInput 3 (parseLines @(Text)))   >>= (Day3.run2 >>> print)
    runDay 4 1 = (loadInput 4 (fromString >>> pure))  >>= (Day4.run1 >>> print)
    runDay 4 2 = (loadInput 4 (fromString >>> pure))  >>= (Day4.run2 >>> print)
    runDay 5 1 = (loadInput 5 (parseLines @(String)))  >>= (Day5.run1 >>> print)
    runDay 5 2 = (loadInput 5 (parseLines @(String)))  >>= (Day5.run2 >>> print)
    runDay 6 1 = (loadInput 6 (pure))  >>= (Day6.run1 >>> print)
    runDay 6 2 = (loadInput 6 (pure))  >>= (Day6.run2 >>> print)
    runDay 7 1 = (loadInput 7 (parseLines @(Text)))  >>= (Day7.run1 >>> print)
    runDay 7 2 = (loadInput 7 (parseLines @(Text)))  >>= (Day7.run2 >>> print)
    runDay 8 1 = (loadInput 8 (parseLines @(Text)))  >>= (Day8.run1 >>> print)
    runDay 8 2 = (loadInput 8 (parseLines @(Text)))  >>= (Day8.run2 >>> print)
    runDay 9 1 = (loadInput 9 (parseRead @Int))  >>= (Day9.run1 >>> print)
    runDay 9 2 = (loadInput 9 (parseRead @Int))  >>= (Day9.run2 >>> print)
    runDay 10 1 = (loadInput 10 (parseRead @Int))  >>= (Day10.run1 >>> print)
    runDay 10 2 = (loadInput 10 (parseRead @Int))  >>= (Day10.run2 >>> print)
    runDay _ _ = pure ()

    parseLines :: (IsString a) => (String -> Maybe [a])
    parseLines =  L.lines >>> (fmap fromString ) >>> pure

    parseRead :: (Read a) => String -> Maybe [a]
    parseRead = L.lines >>> (traverse (readMaybe))
