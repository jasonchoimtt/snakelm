module Tests.Board where

import Graphics.Element exposing (Element)

import ElmTest exposing
    ( suite, test
    , assertEqual, assertNotEqual
    , elementRunner)

import Board exposing
    ( left, right, up, down
    , plus, minus
    , norm, normal
    , inRange
    , zip
    , unpack, pack
    , moveHead, moveTail, crawl)


utils = suite "Board utilities"
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
    , suite "inRange"
        [ test "normal case" <| assertEqual True (inRange (5, 3))
        , test "out of range" <| assertEqual False (inRange (80, 20))
        , test "out of range" <| assertEqual False (inRange (-1, 0))
        ]
    , suite "zip"
        [ test "normal case"
            <| assertEqual [(1, 2), (3, 4), (5, 6)] (zip [1, 3, 5] [2, 4, 6])
        , test "different lengths"
            <| assertEqual [(1, 2), (3, 4)] (zip [1, 3] [2, 4, 6])
        , test "empty" 
            <| assertEqual [] (zip [] [])
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
            <| assertEqual[(0, 2), (5, 2)]
                    (crawl left [(1, 2), (5, 2), (5, 3)])
        , test "case 3"
            <| assertEqual[(1, 1), (1, 2), (5, 2)]
                    (crawl up [(1, 2), (5, 2), (5, 3)])
        ]
    ]


tests = suite "Board"
    [ utils
    , moveSnake
    ]


main = elementRunner tests
