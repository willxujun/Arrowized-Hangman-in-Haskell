{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import Data.Either
import System.Random

newtype Circuit a b = Circuit {
    unCircuit :: a -> (Circuit a b, b)
}

instance Cat.Category Circuit where
    id = Circuit $ \a -> (Cat.id, a)
    (.) = dot
      where
        (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
            let (cir1', b) = cir1 a
                (cir2', c) = cir2 b
            in  (cir2' `dot` cir1', c)

instance Arrow Circuit where
    arr f = Circuit $ \a -> (arr f, f a)
    first (Circuit f) = Circuit $ \(b,d) ->
            let (cir, c) = f b
            in  (first cir, (c,d))

instance ArrowChoice Circuit where
  left c@(Circuit f) = Circuit $ \input ->
    case input of
      Left l -> let (newCir, out) = f l in (left newCir, Left out)
      Right r -> (left c, Right r)

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit cir ls = case ls of
  [] -> []
  (x:xs) -> let (cir', c) = unCircuit cir x
            in  c: runCircuit cir' xs

--circuit that saves its current accumulator value by closure
accum:: a -> (b -> a -> (c, a)) -> Circuit b c
accum acc f = Circuit $ \x ->
  let (output, acc') = f x acc
  in  (accum acc' f, output)

accum' acc f = Circuit $ \x ->
  let acc' = f x acc
  in  (accum' acc' f, acc')

total :: Num a => Circuit a a
total = accum' 0 (+)

mean1 :: Fractional a => Circuit a a
mean1 = (total &&& (arr (const 1) >>> total) ) >>> arr (uncurry (/))

mean2 :: Fractional a => Circuit a a
mean2 = proc input -> do
  sum   <- total -< input
  count <- total -< 1
  returnA -< sum / count

dictionary = ["extremelyhard", "notsohard", "superhard", "yesitshard"]

generator1 :: Random a => (a, a) -> StdGen -> Circuit () a
generator1 range rng = Circuit $ \() ->
  let (value, newGen) = randomR range rng
  in  (generator1 range newGen, value)

generator2 range rng = accum rng $ \() currGen -> randomR range currGen

pickWord :: StdGen -> Circuit () String
pickWord rng = generator2 (0, length dictionary - 1) rng >>^ (!!) dictionary

oneshot:: Circuit () Bool
oneshot = accum True $ \() acc -> (acc, False)

delayedEcho :: a -> Circuit a a
delayedEcho init = accum init $ \x acc -> (acc,x)

leftToMaybe (Left v) = Just v
leftToMaybe _ = Nothing

getWord :: StdGen -> Circuit () String
getWord rng = oneshot >>> arr (\a -> if a then Left () else Right ()) >>> left (pickWord rng) >>> arr leftToMaybe >>> accum' Nothing mplus >>> arr (fromMaybe "")

attempts = 5

livesLeft hung = "Lives: [" ++ replicate (attempts - hung) '#'
                 ++ replicate hung ' ' ++ "]"

hangman :: StdGen -> Circuit String (Bool, [String])
hangman rng = proc input -> do
    word <- getWord rng -< ()
    status <- updateStatus -< input

    let letter = listToMaybe input

    guessed <- if not (isCommand input) && status == 1
               then updateGuess -< (word, letter)
               else returnA -< []
    hung <- if not (isCommand input) && status == 1
            then updateHung -< (word, letter)
            else returnA -< -1

    hasNotEnded <- delayedEcho True -< not (word == guessed || hung >= attempts)
    let result = if isCommand input || status == 0
                 then (if input == "continue" then ["ok continue"] else ["pausing..."])
                 else if word == guessed
                      then [guessed, "you won."]
                      else if hung >= attempts
                           then [guessed, livesLeft hung, "You died."]
                           else [guessed, livesLeft hung]
    returnA -< (hasNotEnded, result)
  where
    isCommand "pause" = True
    isCommand "continue" = True
    isCommand _ = False

    updateStatus :: Circuit String Int
    updateStatus = accum' 0 $ \str curr ->
      case str of
        --is keyword
        "pause" -> 0
        "continue" -> 1
        --is other stuff
        _ -> curr

    updateGuess :: Circuit (String, Maybe Char) String
    updateGuess = accum' (repeat '_') $ \(word, maybeLetter) currentGuess ->
      case maybeLetter of
        Nothing -> take (length word) currentGuess
        Just letter -> let ls = zip word currentGuess
                       in map (\(w, g) -> if w == letter then w else g) ls
    updateHung :: Circuit (String, Maybe Char) Int
    updateHung = arr (\(w, c) -> case c of
                         Nothing -> 0
                         Just l -> if l `elem` w then 0 else 1)
                 >>> total

pause :: Circuit String (Bool, [String])
pause = Circuit $ \_ -> (pause, (True, ["paused"]))

main :: IO ()
main = do
  rng <- getStdGen
  interact $ unlines
    . (:) "Welcome to Arrow Hangman. type \"continue\" to start/resume and \"pause\" to pause."
    . concatMap snd . takeWhile fst
    . runCircuit (hangman rng)
    . (:) ""
    . lines
