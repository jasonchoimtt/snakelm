module Tests.Board where

import Graphics.Element exposing (Element)
import Random
import List
import Debug

import ElmTest exposing
    ( suite, test
    , assert, assertEqual
    , elementRunner
    )

import Board exposing
    ( left, right, up, down, board
    , plus, minus
    , norm, normal
    , cross, dot
    , liesOn
    , inRange
    , unpack, pack
    , moveHead, moveTail, crawl
    , liesOnSnake, isAlive
    , generateFood, generateGame, tickGame
    )


assertNot = assert << not

fieldEqual accessor x y = accessor x == accessor y


vectors = suite "Board vectors"
    [ test "plus"
        <| assertEqual (5, 6) (plus (3, 5) (2, 1))
    , suite "norm"
        [ test "x-direction" <| assertEqual 5 (norm (-5, 0))
        , test "y-direction" <| assertEqual 3 (norm (0, 3))
        , test "zero vector" <| assertEqual 0 (norm (0, 0))
        ]
    , suite "normal"
        [ test "x-direction" <| assertEqual (-1, 0) (normal (-5, 0))
        , test "y-direction" <| assertEqual (0, 1) (normal (0, 3))
        , test "zero vector" <| assertEqual (0, 0) (normal (0, 0))
        ]
    , suite "cross"
        [ test "parallel" <| assertEqual 0 ((3, 0) `cross` (6, 0))
        , test "perpendicular" <| assertEqual 18 ((3, 0) `cross` (0, 6))
        , test "zero" <| assertEqual 0 ((3, 0) `cross` (0, 0))
        , test "slanted" <| assertEqual 12 ((6, 0) `cross` (3, 2))
        , test "non-associative" <| assertEqual -12 ((3, 2) `cross` (6, 0))
        ]
    , suite "dot"
        [ test "parallel" <| assertEqual 8 ((2, 0) `dot` (4, 0))
        , test "anti-parallel" <| assertEqual -8 ((2, 0) `dot` (-4, 0))
        , test "perpendicular" <| assertEqual 0 ((3, 0) `dot` (0, 3))
        , test "zero vector" <| assertEqual 0 ((0, 0) `dot` (5, 5))
        , test "45 degrees" <| assertEqual 6 ((3, 0) `dot` (2, 2))
        ]
    , suite "liesOn"
        [ test "in segment"
            <| assert ((4, 2) `liesOn` ((-3, 2), (5, 2)))
        , test "at end point"
            <| assert ((4, 2) `liesOn` ((4, 2), (5, 2)))
        , test "out of segment"
            <| assertNot ((-4, 2) `liesOn` ((-3, 2), (5, 2)))
        , test "slanted line"
            <| assert ((5, 3) `liesOn` ((0, 0), (50, 30)))
        , test "out of line"
            <| assertNot ((3, 3) `liesOn` ((0, 1), (5, 5)))
        , test "vertical line"
            <| assert ((0, 0) `liesOn` ((0, -5), (0, 5)))
        ]
    ]


utils = suite "Board utils"
    [ suite "inRange"
        [ test "normal case" <| assert (inRange (5, 3))
        , test "out of range" <| assertNot (inRange (80, 20))
        , test "out of range" <| assertNot (inRange (-1, 0))
        ]
    , suite "unpack"
        [ test "normal case"
            <| assertEqual [ ((1, 2), (5, 2)), ((5, 2), (5, 6)) ]
                    (unpack [(1, 2), (5, 2), (5, 6)])
        , test "minimal length"
            <| assertEqual [ ((3, 4), (5, 6)) ]
                    (unpack [(3, 4), (5, 6)])
        ]
    , suite "pack"
        [ test "normal case"
            <| assertEqual [(1, 2), (5, 2), (5, 6)]
                    (pack [ ((1, 2), (5, 2)), ((5, 2), (5, 6)) ])
        , test "minimal length"
            <| assertEqual [(3, 4), (5, 6)]
                    (pack [ ((3, 4), (5, 6)) ])
        ]
    ]


moveSnake = suite "Board snake moving"
    [ suite "moveHead"
        [ test "same direction"
            <| assertEqual [(0, 2), (5, 2), (5, 6)]
                    (moveHead left [(1, 2), (5, 2), (5, 6)])
        , test "different direction"
            <| assertEqual [(1, 1), (1, 2), (5, 2), (5, 6)]
                    (moveHead up [(1, 2), (5, 2), (5, 6)])
        ]
    , suite "moveTail"
        [ test "non-ending segment"
            <| assertEqual [(1, 2), (5, 2), (5, 5)]
                    (moveTail [(1, 2), (5, 2), (5, 6)])
        , test "ending segment"
            <| assertEqual [(1, 2), (5, 2)]
                    (moveTail [(1, 2), (5, 2), (5, 3)])
        ]
    , suite "crawl"
        [ test "case 1"
            <| assertEqual [(0, 2), (5, 2), (5, 5)]
                    (crawl left [(1, 2), (5, 2), (5, 6)])
        , test "case 2"
            <| assertEqual [(0, 2), (5, 2)]
                    (crawl left [(1, 2), (5, 2), (5, 3)])
        , test "case 3"
            <| assertEqual [(1, 1), (1, 2), (5, 2)]
                    (crawl up [(1, 2), (5, 2), (5, 3)])
        ]
    , suite "liesOnSnake"
        [ test "head is on snake"
            <| assert ((1, 2) `liesOnSnake` [(1, 2), (5, 2), (5, 6)])
        , test "middle of snake"
            <| assert ((5, 4) `liesOnSnake` [(1, 2), (5, 2), (5, 6)])
        , test "outside snake"
            <| assertNot ((0, 0) `liesOnSnake` [(1, 2), (5, 2), (5, 6)])
        ]
    , suite "isAlive"
        [ test "trivial case"
            <| assert (isAlive [(3, 5), (8, 5)])
        , test "hit wall"
            <| assertNot (isAlive [(-1, 5), (6, 5)])
        , test "almost hit wall"
            <| assert (isAlive [(0, 5), (6, 5)])
        , test "hit self"
            <| assertNot (isAlive [(0, 5), (3, 5), (3, 2), (0, 2), (0, 7)])
        , test "almost hit self"
            <| assert (isAlive [(1, 5), (3, 5), (3, 2), (0, 2), (0, 7)])
        ]
    ]


gameLoop = suite "Board game loop"
    [ suite "generateFood"
        [ test "in range"
            <| (
                let
                    seed = Random.initialSeed 123456
                    (food, _) = generateFood [(0, 0), (0, 10)] seed
                in
                    assert (inRange food)
               )
        , test "only one possible cell"
            <| (
                let
                    (w, h) = board
                    seed = Random.initialSeed 123456

                    snake' = List.concatMap
                        (\y -> let (l, r) = ((0, y), (w - 1, y)) in
                            if y % 2 == 0 then [l, r] else [r, l])
                        [0..(h - 1)]
                    snake = case snake' of
                        _ :: tail -> (1, 0) :: tail
                        _ -> Debug.crash "Invalid snake"

                    (food, _) = generateFood snake seed
                in
                    assertEqual (0, 0) food
               )
        ]
    , suite "tickGame"
        (let baseGame = { snake = [(44, 12), (35, 12)]
                        , dir = right
                        , dead = False
                        , score = 0
                        , food = (0, 0)
                        , seed = Random.initialSeed 123456
                        }
        in
            [ test "dead game ignored"
                <| assertEqual
                        { baseGame | dead = True }
                        (tickGame Nothing { baseGame | dead = True })
            , test "no direction change"
                <| assertEqual
                        { baseGame | snake = [(45, 12), (36, 12)] }
                        (tickGame Nothing baseGame)
            , test "with direction change"
                <| assertEqual
                        { baseGame | snake = [(44, 11), (44, 12), (36, 12)]
                                   , dir = up }
                        (tickGame (Just up) baseGame)
            , test "eating food"
                <| assert <|
                    List.all
                        (\f -> f
                            { baseGame | snake = [(45, 12), (36, 12)]
                                       , score = 1 }
                            (tickGame Nothing { baseGame | food = (45, 12) })
                        )
                        [ fieldEqual .snake
                        , fieldEqual .dir
                        , fieldEqual .dead
                        , fieldEqual .score
                        , \_ g -> g.food /= (45, 12)
                        , \_ g -> not (g.food `liesOnSnake` g.snake)
                        ]
            , test "hit wall"
                <| assertEqual
                        { baseGame | snake = [(-1, 12), (9, 12)]
                                   , dead = True
                                   , dir = left }
                        (tickGame (Just left)
                            { baseGame | snake = [(0, 12), (10, 12)] })
            , test "hit self"
                <| assertEqual
                        { baseGame
                            | snake = [(0, 5), (3, 5), (3, 2), (0, 2), (0, 7)]
                            , dead = True
                            , dir = left }
                        (tickGame (Just left)
                            { baseGame
                                | snake = [(1, 5), (3, 5), (3, 2), (0, 2), (0, 8)] })
            , test "walk backwards"
                <| assertEqual
                        { baseGame
                            | snake = [(43, 12), (44, 12), (36, 12)]
                            , dead = True
                            , dir = left }
                        (tickGame (Just left) baseGame)
            ]
        )
    ]


tests = suite "Board"
    [ vectors
    , utils
    , moveSnake
    , gameLoop
    ]


main = elementRunner tests
