module WorkLog.Main exposing (main)

--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.

import Browser
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Maybe.Extra as Maybe
import WorkLog.NumberInput as NumberInput exposing (NumberInput)
import WorkLog.Task as Task exposing (Task)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { totalMinutes : NumberInput
    , tasks : Dict Int (Task NumberInput)
    , distribution : Result Task.TimeDistributionProblem (Dict Int (Task Int))
    }


init : Model
init =
    { totalMinutes = NumberInput.fromValue <| 8 * 60
    , tasks = Dict.empty
    , distribution = Err Task.NoWork
    }


type Msg
    = TotalEntered String
    | TaskAdded
    | TaskRemoved { id : Int }
    | TaskRenamed { id : Int, newName : String }
    | TaskMinutesSpentSet { id : Int, rawMinutes : String }


view : Model -> Html Msg
view model =
    Element.layout [] <| viewLayout model


viewLayout : Model -> Element Msg
viewLayout model =
    Element.column
        [ Element.paddingXY 100 50
        , Element.spacing 50
        , Font.size 12
        , Font.family
            [ Font.typeface "Helvetica"
            , Font.typeface "Arial"
            , Font.typeface "DejaVu Sans"
            , Font.sansSerif
            ]
        ]
        [ viewMinutesToLog model.totalMinutes
        , case model.totalMinutes.parsed of
            Just _ ->
                Element.none

            Nothing ->
                Element.paragraph errorFont
                    [ Element.text <| "\"" ++ model.totalMinutes.raw ++ "\" is not a valid number." ]
        , case model.distribution of
            Ok _ ->
                Element.none

            Err problem ->
                viewTimeDistributionProblem problem
        , Element.column [ Element.spacing 20 ]
            [ viewTasks model.tasks
            , Input.button buttonStyle
                { onPress = Just TaskAdded
                , label = Element.text "Add task"
                }
            ]
        , case model.distribution of
            Err _ ->
                Element.none

            Ok distribution ->
                viewDistribution distribution
        ]


viewMinutesToLog : NumberInput -> Element Msg
viewMinutesToLog { raw } =
    Element.paragraph []
        [ Element.text "I need to log "
        , Input.text inputStyle
            { onChange = TotalEntered
            , text = raw
            , placeholder = Nothing
            , label = Input.labelHidden "Minutes to log"
            }
        , Element.text " minutes."
        ]


viewTimeDistributionProblem : Task.TimeDistributionProblem -> Element none
viewTimeDistributionProblem problem =
    Element.paragraph
        errorFont
        [ Element.text <|
            case problem of
                Task.NoWork ->
                    "There is no work to distribute time between."

                Task.InvalidTime ->
                    "Some time values are not valid. Please correct them."

                Task.Overwork ->
                    "You have worked more than you need to log. Are you OK?"
        ]


viewTasks : Dict Int (Task NumberInput) -> Element Msg
viewTasks tasks =
    if Dict.isEmpty tasks then
        Element.none

    else
        Element.table
            [ Element.spacingXY 10 0 ]
            { data = Dict.toList tasks
            , columns =
                [ { header = Element.text "The task"
                  , width = Element.fill
                  , view =
                        \( id, task ) ->
                            Input.text inputStyle
                                { onChange = \input -> TaskRenamed { id = id, newName = input }
                                , text = task.name
                                , placeholder = Nothing
                                , label = Input.labelHidden "Task ID"
                                }
                  }
                , { header = Element.text "took"
                  , width = Element.fill
                  , view =
                        \( id, task ) ->
                            let
                                extraStyle =
                                    if Maybe.isJust task.minutesSpent.parsed then
                                        []

                                    else
                                        [ Border.color errorColor
                                        , Font.color errorColor
                                        ]
                            in
                            Input.text (inputStyle ++ extraStyle)
                                { onChange = \input -> TaskMinutesSpentSet { id = id, rawMinutes = input }
                                , text = task.minutesSpent.raw
                                , placeholder = Nothing
                                , label = Input.labelHidden "Minutes spent on task"
                                }
                  }
                , { header = Element.text "minutes of my time."
                  , width = Element.fill
                  , view =
                        \( id, _ ) ->
                            Element.el [ Element.alignBottom ] <|
                                Input.button buttonStyle
                                    { onPress = Just <| TaskRemoved { id = id }
                                    , label = Element.text "Remove task"
                                    }
                  }
                ]
            }


viewDistribution : Dict Int (Task Int) -> Element msg
viewDistribution distribution =
    Element.table [ Element.spacingXY 10 10 ]
        { data = Dict.toList distribution
        , columns =
            [ { header = Element.text "For the task"
              , width = Element.shrink
              , view =
                    \( _, task ) ->
                        Element.text task.name
              }
            , { header = Element.text "log"
              , width = Element.shrink
              , view =
                    \( _, task ) ->
                        Element.text <| String.fromInt task.minutesSpent
              }
            , { header = Element.text "minutes of work."
              , width = Element.shrink
              , view = always Element.none
              }
            ]
        }


inputStyle : List (Element.Attribute msg)
inputStyle =
    [ Border.color <| Element.rgb 0 0 0
    , Border.widthEach
        { bottom = 1, left = 0, right = 0, top = 0 }
    ]


buttonStyle : List (Element.Attribute msg)
buttonStyle =
    [ Element.padding 5
    , Border.color (Element.rgb 0 0 0)
    , Border.width 1
    ]


errorFont : List (Element.Attr decorative msg)
errorFont =
    [ Font.color (Element.rgb 1 0 0)
    ]


errorColor : Element.Color
errorColor =
    Element.rgb 1 0 0


update : Msg -> Model -> Model
update msg model =
    model
        |> updateInput msg
        |> distributeTime


updateInput : Msg -> Model -> Model
updateInput msg model =
    case msg of
        TotalEntered rawInput ->
            { model | totalMinutes = NumberInput.fromRawInput rawInput }

        TaskAdded ->
            let
                newTasks =
                    model.tasks |> Task.add
            in
            { model | tasks = newTasks }

        TaskRemoved { id } ->
            let
                newTasks =
                    model.tasks |> Task.remove id
            in
            { model | tasks = newTasks }

        TaskRenamed { id, newName } ->
            let
                newTasks =
                    model.tasks |> Task.update id (Task.rename newName)
            in
            { model | tasks = newTasks }

        TaskMinutesSpentSet { id, rawMinutes } ->
            let
                newTasks =
                    model.tasks |> Task.update id (Task.setMinutesSpent rawMinutes)
            in
            { model | tasks = newTasks }


distributeTime : Model -> Model
distributeTime model =
    let
        target =
            model.totalMinutes.parsed |> Maybe.withDefault -1

        newDistribution =
            model.tasks
                |> Task.distributeTime target
    in
    { model | distribution = newDistribution }
