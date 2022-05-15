module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Time exposing (Posix, every)


gridWidth : Int
gridWidth =
    50


gridHeight : Int
gridHeight =
    50



-- Types


type alias Model =
    { counter : Int
    , grid : Grid
    }


type Msg
    = SetGrid Grid
    | Tick Posix


type alias Grid =
    Array (Array State)


type State
    = Alive
    | Dead



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { counter = 0, grid = Array.empty }
    , Random.generate SetGrid gridGenerator
    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    every 100 Tick



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGrid grid ->
            ( { model | grid = grid }, Cmd.none )

        Tick _ ->
            ( { model
                | counter = model.counter + 1
                , grid = updateGrid model.grid
              }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text <| String.fromInt model.counter ]
        , showGrid model.grid
        ]


showGrid : Grid -> Html Msg
showGrid grid =
    div [] <| (Array.toList grid |> List.map showRow)


showRow : Array State -> Html Msg
showRow rowDict =
    div [ style "margin" "0", style "padding" "0", style "line-height" "0" ] <|
        (Array.toList rowDict |> List.map showCell)


showCell : State -> Html Msg
showCell state =
    let
        background =
            case state of
                Alive ->
                    "black"

                Dead ->
                    "DeepSkyBlue"
    in
    span
        [ style "width" "12px"
        , style "height" "12px"
        , style "background-color" background
        , style "display" "inline-block"
        , style "border-radius" "50%"
        ]
        []



-- Grid updates


updateGrid : Grid -> Grid
updateGrid grid =
    Array.indexedMap (updateRow grid) grid


updateRow : Grid -> Int -> Array State -> Array State
updateRow grid y column =
    Array.indexedMap
        (\x state -> updateCell (getAliveCount grid x y) state)
        column


updateCell : Int -> State -> State
updateCell aliveCount state =
    case ( state, aliveCount ) of
        ( Dead, 3 ) ->
            Alive

        ( Dead, _ ) ->
            Dead

        ( Alive, 2 ) ->
            Alive

        ( Alive, 3 ) ->
            Alive

        ( Alive, _ ) ->
            Dead


getAliveCount : Grid -> Int -> Int -> Int
getAliveCount grid cellX cellY =
    getNeighborCoordinates cellX cellY
        |> List.filterMap (getCellState grid)
        |> List.filter (\s -> s == Alive)
        |> List.length


getCellState : Grid -> ( Int, Int ) -> Maybe State
getCellState grid ( x, y ) =
    Array.get y grid |> Maybe.andThen (Array.get x)


getNeighborCoordinates : Int -> Int -> List ( Int, Int )
getNeighborCoordinates cellX cellY =
    let
        shiftCoordinates =
            [ ( -1, -1 )
            , ( -1, 0 )
            , ( -1, 1 )
            , ( 0, -1 )
            , ( 0, 1 )
            , ( 1, -1 )
            , ( 1, 0 )
            , ( 1, 1 )
            ]
    in
    List.map
        (\( xShift, yShift ) -> ( cellX + xShift, cellY + yShift ))
        shiftCoordinates



-- Generators


stateGenerator : Generator State
stateGenerator =
    Random.uniform Alive [ Dead ]


columnGenerator : Generator (Array State)
columnGenerator =
    Random.list gridWidth stateGenerator
        |> Random.map Array.fromList


gridGenerator : Generator Grid
gridGenerator =
    Random.list gridHeight columnGenerator
        |> Random.map Array.fromList
