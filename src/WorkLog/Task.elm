module WorkLog.Task exposing (Task, TimeDistributionProblem(..), add, distributeTime, empty, remove, rename, setMinutesSpent, update)

--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.

import Dict exposing (Dict)
import WorkLog.Calculator as Calculator
import WorkLog.NumberInput as NumberInput exposing (NumberInput)


type alias Task minutes =
    { name : String
    , minutesSpent : minutes
    }


type TimeDistributionProblem
    = InvalidTime
    | NoWork
    | Overwork


empty : Task NumberInput
empty =
    { name = ""
    , minutesSpent = NumberInput.fromRawInput ""
    }


rename : String -> Task minutes -> Task minutes
rename newName task =
    { task | name = newName }


setMinutesSpent : String -> Task NumberInput -> Task NumberInput
setMinutesSpent rawMinutes task =
    { task | minutesSpent = NumberInput.fromRawInput rawMinutes }


add : Dict Int (Task NumberInput) -> Dict Int (Task NumberInput)
add tasks =
    tasks
        |> Dict.insert (nextID tasks) empty


nextID : Dict Int (Task minutes) -> Int
nextID tasks =
    tasks
        |> Dict.keys
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0


remove : Int -> Dict Int (Task minutes) -> Dict Int (Task minutes)
remove =
    Dict.remove


update : Int -> (Task minutes -> Task minutes) -> Dict Int (Task minutes) -> Dict Int (Task minutes)
update id updateFn tasks =
    tasks
        |> Dict.update id (Maybe.map updateFn)


distributeTime : Int -> Dict Int (Task NumberInput) -> Result TimeDistributionProblem (Dict Int (Task Int))
distributeTime target tasks =
    let
        names =
            tasks |> Dict.map (always .name)
    in
    tasks
        |> validateAll
        |> Result.map (Dict.map (always .minutesSpent))
        |> Result.map (Calculator.distribute target)
        |> Result.andThen (reconstruct names)


validateAll : Dict Int (Task NumberInput) -> Result TimeDistributionProblem (Dict Int (Task Int))
validateAll tasks =
    let
        -- FIXME: Gnarly loop.
        loop : List ( Int, Task NumberInput ) -> List ( Int, Task Int ) -> Result TimeDistributionProblem (Dict Int (Task Int))
        loop inputTasks valid =
            case inputTasks of
                [] ->
                    Ok <| Dict.fromList valid

                ( id, first ) :: rest ->
                    case first |> validateOne of
                        Nothing ->
                            Err InvalidTime

                        Just validTask ->
                            loop rest <| ( id, validTask ) :: valid
    in
    loop (Dict.toList tasks) []


reconstruct : Dict Int String -> Result Calculator.Problem (Dict Int Int) -> Result TimeDistributionProblem (Dict Int (Task Int))
reconstruct names problem =
    case problem of
        Err Calculator.NegativeTime ->
            Err InvalidTime

        Err Calculator.NoWork ->
            Err NoWork

        Err Calculator.Overwork ->
            Err Overwork

        Ok assignedTimes ->
            let
                fillName id minutes =
                    { name = names |> Dict.get id |> Maybe.withDefault "N/A"
                    , minutesSpent = minutes
                    }
            in
            Ok <| Dict.map fillName assignedTimes


validateOne : Task NumberInput -> Maybe (Task Int)
validateOne { name, minutesSpent } =
    case minutesSpent.parsed of
        Just minutes ->
            Just
                { name = name
                , minutesSpent = minutes
                }

        Nothing ->
            Nothing
