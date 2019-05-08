module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Random exposing (Generator)
import Time exposing (Posix, every)


gridWidth : Int
gridWidth =
    25


gridHeight : Int
gridHeight =
    25



-- Types


type alias Model =
    { counter : Int
    , grid : Grid
    }


type Msg
    = NoOp
    | SetGrid Grid
    | Tick Posix


type alias Grid =
    Dict Int (Dict Int State)


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
    ( { counter = 0, grid = Dict.empty }
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
        NoOp ->
            ( model, Cmd.none )

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
    div [] <|
        (grid |> Dict.toList |> List.map showRow)


showRow : ( Int, Dict Int State ) -> Html Msg
showRow ( _, rowDict ) =
    div [ style "margin" "0", style "padding" "0", style "line-height" "0" ] <|
        (rowDict |> Dict.toList |> List.map showCell)


showCell : ( Int, State ) -> Html Msg
showCell ( _, state ) =
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
    Dict.map (updateRow grid) grid


updateRow : Grid -> Int -> Dict Int State -> Dict Int State
updateRow grid y column =
    Dict.map
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
    Dict.get y grid |> Maybe.andThen (Dict.get x)


isInsideGrid : Int -> Int -> Bool
isInsideGrid x y =
    x >= 0 && x < gridWidth && y >= 0 && y < gridHeight


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
    List.filterMap
        (\( xShift, yShift ) ->
            let
                neighborX =
                    cellX + xShift

                neighborY =
                    cellY + yShift
            in
            if isInsideGrid neighborX neighborY then
                Just ( neighborX, neighborY )

            else
                Nothing
        )
        shiftCoordinates



-- Generators


stateGenerator : Generator State
stateGenerator =
    Random.uniform Alive [ Dead ]


columnGenerator : Generator (Dict Int State)
columnGenerator =
    Random.list gridWidth stateGenerator
        |> Random.map (List.indexedMap (\i state -> ( i, state )))
        |> Random.map Dict.fromList


gridGenerator : Generator Grid
gridGenerator =
    Random.list gridHeight columnGenerator
        |> Random.map (List.indexedMap (\i state -> ( i, state )))
        |> Random.map Dict.fromList
