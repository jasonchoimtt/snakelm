import List
import Debug

import Graphics.Element exposing (show, Element)
import Graphics.Collage exposing (collage, outlined, solid, path, traced, rect, move, filled)
import Color

import Keyboard
import Random
import Time
import Window

import Board exposing (Game, Direction)


type alias Arrows = { x : Int, y : Int }


-- View
render : (Int, Int) -> Game -> Element
render (w', h') game =
    let
        (boardW, boardH) = Board.board
        (w, h) = (toFloat w', toFloat h')
        cell = min (w / boardW) (h / boardH)
        (offsetX, offsetY) = ((toFloat boardW) * cell / 2, (toFloat boardH) * cell / 2)
        coordsAt (x, y) = ((toFloat x) * cell - offsetX, (toFloat y) * cell - offsetY)

        border = (uncurry rect) (cell * (boardW + 1), cell * (boardH + 1))
                    |> outlined (solid Color.black)
                    |> move (-cell / 2, -cell / 2)

        snakeLine = solid Color.black
        snake = traced { snakeLine | width = cell * 2 / 3 }
                    <| path (List.map coordsAt game.snake)
        food = rect cell cell
                    |> filled Color.red
                    |> move (coordsAt game.food)
    in
        collage w' h'
            <| [border, snake, food]


-- Input
frameInput : Signal (Maybe Direction)
frameInput =
    let
        fps = Time.fps 12
        dirInput =
            Signal.map
                (\{ x, y } -> case (x, y) of
                    (-1, 0) -> Just Board.left
                    (1, 0) -> Just Board.right
                    (0, 1) -> Just Board.up
                    (0, -1) -> Just Board.down
                    _ -> Nothing
                )
                Keyboard.arrows
        lastDirInput : Signal (Maybe Direction)
        lastDirInput =
            Signal.foldp
                (\next cur -> Maybe.oneOf [next, cur]) Nothing dirInput
    in
        Signal.sampleOn
            fps
            lastDirInput


-- Game state
gameLoop : Signal Game
gameLoop =
    Signal.foldp
        Board.tickGame
        (Board.generateGame (Random.initialSeed Time.))
        frameInput


main : Signal Element
main = Signal.map2 render Window.dimensions gameLoop
