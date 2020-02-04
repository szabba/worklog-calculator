module WorkLog.Main exposing (main)

--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.

import Browser
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import WorkLog.NumberInput as NumberInput exposing (NumberInput)
import WorkLog.Task as Task exposing (Task)


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
    H.div []
        [ viewMinutesToLog model.totalMinutes
        , viewIsNotAValidNumber H.p model.totalMinutes
        , viewTasks model.tasks
        , H.button
            [ HE.onClick TaskAdded ]
            [ H.text "Add task" ]
        , viewDistribution model.distribution
        ]


viewMinutesToLog : NumberInput -> Html Msg
viewMinutesToLog { raw } =
    H.p []
        [ H.text "I need to log "
        , H.input [ HE.onInput TotalEntered, HA.value raw ] []
        , H.text " minutes."
        ]


viewTasks : Dict Int (Task NumberInput) -> Html Msg
viewTasks tasks =
    H.table [] <|
        List.concatMap viewTask <|
            Dict.toList tasks


viewTask : ( Int, Task NumberInput ) -> List (Html Msg)
viewTask ( id, task ) =
    let
        wrapInRow attrs children =
            H.tr []
                [ H.td ([ HA.colspan 6 ] ++ attrs)
                    children
                ]
    in
    [ viewTaskRow id task
    , viewIsNotAValidNumber wrapInRow task.minutesSpent
    ]


viewTaskRow : Int -> Task NumberInput -> Html Msg
viewTaskRow id task =
    H.tr [] <|
        List.map (H.td [] << List.singleton)
            [ H.text "Task"
            , H.input
                [ HE.onInput <| \input -> TaskRenamed { id = id, newName = input }
                , HA.value task.name
                ]
                []
            , H.text "took up"
            , H.input
                [ HE.onInput <| \input -> TaskMinutesSpentSet { id = id, rawMinutes = input }
                , HA.value task.minutesSpent.raw
                ]
                []
            , H.text "minutes of my time."
            , H.button
                [ HE.onClick <| TaskRemoved { id = id } ]
                [ H.text "Remove task" ]
            ]


viewIsNotAValidNumber :
    (List (H.Attribute msg) -> List (Html msg) -> Html msg)
    -> NumberInput
    -> Html msg
viewIsNotAValidNumber tag numberInput =
    case numberInput.parsed of
        Just _ ->
            H.text ""

        Nothing ->
            tag
                [ HA.style "color" "red" ]
                [ H.text <| "\"" ++ numberInput.raw ++ "\" is not a valid number." ]


viewDistribution :
    Result Task.TimeDistributionProblem (Dict Int (Task Int))
    -> Html msg
viewDistribution result =
    case result of
        Err problem ->
            H.p
                [ HA.style "color" "red " ]
                [ H.text <|
                    case problem of
                        Task.NoWork ->
                            "There is no work to distribute time between."

                        Task.InvalidTime ->
                            "Some time values are not valid. Please correct them."

                        Task.Overwork ->
                            "You have worked more than you need to log. Are you OK?"
                ]

        Ok distribution ->
            viewOkDistribution distribution


viewOkDistribution : Dict Int (Task Int) -> Html msg
viewOkDistribution distribution =
    H.table [] <| List.map viewDistributionTask <| Dict.values distribution


viewDistributionTask : Task Int -> Html msg
viewDistributionTask { name, minutesSpent } =
    H.tr [] <|
        List.map (H.td [] << List.singleton << H.text) <|
            [ "For task"
            , "\"" ++ name ++ "\""
            , "log"
            , String.fromInt minutesSpent
            , " minutes."
            ]


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
