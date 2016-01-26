module Board where

import List exposing (append)
import List.Extra exposing (init, last)


-- Type definitions
type alias Point = (Int, Int)
type alias Line = (Point, Point)
type alias Snake = List Point
type alias Direction = Point


-- Constants
left = (-1, 0)
right = (1, 0)
up = (0, -1)
down = (0, 1)

board = (80, 24)

defaultSnake = [(44, 12), (35, 12)]


-- Utility functions
{-| Checks if a point is within a the board -}
inRange : Point -> Bool
inRange (x, y) =
    let
        (w, h) = board
    in
        x >= 0 && y >= 0 && x < w && y < h


{-| Adds two points together -}
plus : Point -> Point -> Point
plus (x', y') (x, y) = (x' + x, y' + y)


{-| Minuses two points -}
minus : Point -> Point -> Point
minus (x', y') (x, y) = (x', y') `plus` (-x, -y)


{-| Calculates the Manhattan length of a vector -}
norm : Point -> Int
norm (x, y) = (abs x) + (abs y)


{-| Find the direction of a vector -}
normal : Point -> Point
normal (x, y) =
    let len = norm (x, y) in
        ( x // len, y // len )


{-| Zips two lists into a list of pairs
 -  http://elm-lang.org/examples/zip -}
zip : List a -> List b -> List (a, b)
zip xs ys =
    case (xs, ys) of
        ((x :: xs'), (y :: ys')) ->
            (x,y) :: zip xs' ys'
        (_, _) ->
            []


{-| Converts a list of points into a list of lines -}
unpack : List Point -> List Line
unpack xs =
    case xs of
        x :: xs' ->
            zip (x :: xs') xs'
        _ ->
            []


{-| Converts a list of lines into a list of points -}
pack : List Line -> List Point
pack lines = case List.unzip lines of
    ((firstPt :: _), remainingPts) ->
        (firstPt :: remainingPts)
    ([], []) ->
        []
    _ ->
        Debug.crash "Invalid List Line"


{-| Progresses the head of the snake -}
moveHead : Direction -> Snake -> Snake
moveHead dir snake = case unpack snake of
    (h', h) :: tail ->
        let hHat = normal (h' `minus` h) in
        pack <| if hHat == dir
            then ( (h' `plus` dir), h ) :: tail
            else ( (h' `plus` dir), h' ) :: (h', h) :: tail
    _ ->
        Debug.crash "Invalid snake"


{-| Progresses the tail of the snake -}
moveTail : Snake -> Snake
moveTail snake = let lines = unpack snake in
    case (init lines, last lines) of
        (Just head, Just (t', t)) ->
            let tHat = normal (t' `minus` t) in
            pack <| if (t `plus` tHat) == t'
                then head
                else head `append` [( t', (t `plus` tHat) )]
        _ ->
            Debug.crash "Invalid snake"


{-| Progresses the snake towards a direction -}
crawl : Direction -> Snake -> Snake
crawl dir snake = (moveHead dir >> moveTail) snake
