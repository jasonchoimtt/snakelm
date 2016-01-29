module Board where

import List.Extra exposing (init, last)
import Random


-- Type definitions
type alias Point = (Int, Int)
type alias Line = (Point, Point) -- Line segment
type alias Snake = List Point
type alias Direction = Point

{-| Game state record -}
type alias Game =
    { snake: Snake
    , dir: Point
    , dead: Bool
    , score: Int
    , food: Point
    , seed: Random.Seed
    }


-- Constants
left = (-1, 0)
right = (1, 0)
up = (0, 1)
down = (0, -1)

board = (80, 24)

defaultSnake = [(44, 12), (35, 12)]
defaultDir = right


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


{-| Determines if a point lies on the snake -}
liesOnSnake : Point -> Snake -> Bool
liesOnSnake pt snake = ((List.any) (liesOn pt) (unpack snake))


{-| Determines if the snake lives, i.e. its head has not collided with the wall
 -  or itself -}
isAlive : Snake -> Bool
isAlive snake = case snake of
    head :: tail ->
        inRange head && not (head `liesOnSnake` tail)
    _ ->
        Debug.crash "Invalid snake"


{-| Generates food coordinates that do not lie on the snake -}
generateFood : Snake -> Random.Seed -> (Point, Random.Seed)
generateFood snake seed =
    let
        (w, h) = board
        foodGen = Random.pair (Random.int 0 (w - 1)) (Random.int 0 (h - 1))
        (food, seed') = Random.generate foodGen seed
    in
        if food `liesOnSnake` snake
            then generateFood snake seed'
            else (food, seed')


{-| Generates a new game given a seed -}
generateGame : Random.Seed -> Game
generateGame seed =
    let
        (food, seed') = generateFood defaultSnake seed
    in
        { snake = defaultSnake
        , dir = defaultDir
        , dead = False
        , score = 0
        , food = food
        , seed = seed'
        }


{-| Proceeds the game by one tick, taking new input -}
tickGame : Maybe Direction -> Game -> Game
tickGame inputDir game =
    let
        notDead f game = if game.dead then game else f game

        snakeHead snake = case snake of
            head :: _ -> head
            _ -> Debug.crash "Invalid snake"

        doCrawl inputDir game =
            let
                dir = Maybe.withDefault game.dir inputDir
                snake = crawl dir game.snake
            in
                { game | snake = snake, dir = dir }

        doDeath game =
            let
                dead = not (isAlive game.snake)
            in
                { game | dead = dead }

        doFood game =
            if game.food == snakeHead game.snake
                then let (food, seed) = generateFood game.snake game.seed in
                    { game
                        | score = game.score + 1
                        , food = food
                        , seed = seed
                    }
                else game
    in
        game
            |> notDead (doCrawl inputDir)
            |> notDead doDeath
            |> notDead doFood
