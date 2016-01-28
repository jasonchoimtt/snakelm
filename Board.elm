module Board where

import List.Extra exposing (init, last)


-- Type definitions
type alias Point = (Int, Int)
type alias Line = (Point, Point) -- Line segment
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
{-| Checks if the point is within the board -}
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


{-| Find the direction of a vector
 -  ONLY WORKS IF the vector is along the x- or y-axis -}
normal : Point -> Point
normal (x, y) =
    let len = norm (x, y) in
        ( x // len, y // len )


{-| Cross product -}
cross : Point -> Point -> Int
cross (x', y') (x, y) = x' * y - y' * x


{-| Dot product -}
dot : Point -> Point -> Int
dot (x', y') (x, y) = x' * x + y' * y


{-| Checks if the point lies within the line segment
 - http://stackoverflow.com/questions/328107/ -}
liesOn : Point -> Line -> Bool
liesOn pt (p, q) =
    (p `minus` pt) `cross` (pt `minus` q) == 0 &&
    (p `minus` pt) `dot` (pt `minus` q) >= 0 &&
    (p `minus` pt) `dot` (pt `minus` q) <= (p `minus` q) `dot` (p `minus` q)


{-| Converts a list of points into a list of lines -}
unpack : List Point -> List Line
unpack xs =
    case xs of
        x :: xs' ->
            List.map2 (,) (x :: xs') xs'
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
                else head `List.append` [( t', (t `plus` tHat) )]
        _ ->
            Debug.crash "Invalid snake"


{-| Progresses the snake towards a direction -}
crawl : Direction -> Snake -> Snake
crawl dir snake = (moveHead dir >> moveTail) snake


{-| Determines if the snake lives, i.e. its head has not collided with the wall
 -  or itself -}
isAlive : Snake -> Bool
isAlive snake = case unpack snake of
    (h', h) :: tail ->
        inRange h' && not ((List.any) (liesOn h') tail)
    _ ->
        Debug.crash "Invalid snake"
