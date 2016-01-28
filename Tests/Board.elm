module Tests.Board where

import Graphics.Element exposing (Element)

import ElmTest exposing
    ( suite, test
    , assertEqual, assertNotEqual
    , elementRunner
    )

import Board exposing
    ( left, right, up, down
    , plus, minus
    , norm, normal
    , cross, dot
    , liesOn
    , inRange
    , unpack, pack
    , moveHead, moveTail, crawl, isAlive
    )


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
            <| assertEqual True ((4, 2) `liesOn` ((-3, 2), (5, 2)))
        , test "at end point"
            <| assertEqual True ((4, 2) `liesOn` ((4, 2), (5, 2)))
        , test "out of segment"
            <| assertEqual False ((-4, 2) `liesOn` ((-3, 2), (5, 2)))
        , test "slanted line"
            <| assertEqual True ((5, 3) `liesOn` ((0, 0), (50, 30)))
        , test "out of line"
            <| assertEqual False ((3, 3) `liesOn` ((0, 1), (5, 5)))
        , test "vertical line"
            <| assertEqual True ((0, 0) `liesOn` ((0, -5), (0, 5)))
        ]
    ]


utils = suite "Board utils"
    [ suite "inRange"
        [ test "normal case" <| assertEqual True (inRange (5, 3))
        , test "out of range" <| assertEqual False (inRange (80, 20))
        , test "out of range" <| assertEqual False (inRange (-1, 0))
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
    , suite "isAlive"
        [ test "trivial case"
            <| assertEqual True (isAlive [(3, 5), (8, 5)])
        , test "hit wall"
            <| assertEqual False (isAlive [(-1, 5), (6, 5)])
        , test "almost hit wall"
            <| assertEqual True (isAlive [(0, 5), (6, 5)])
        , test "hit self"
            <| assertEqual False
                    (isAlive [(0, 5), (3, 5), (3, 2), (0, 2), (0, 7)])
        , test "almost hit self"
            <| assertEqual True
                    (isAlive [(1, 5), (3, 5), (3, 2), (0, 2), (0, 7)])
        ]
    ]


tests = suite "Board"
    [ vectors
    , utils
    , moveSnake
    ]


main = elementRunner tests
