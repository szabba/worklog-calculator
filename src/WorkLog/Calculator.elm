module WorkLog.Calculator exposing (Problem(..), distribute)

--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.

import Dict exposing (Dict)
import List.Extra as List


type Problem
    = NegativeTime
    | NoWork
    | Overwork


distribute : Int -> Dict Int Int -> Result Problem (Dict Int Int)
distribute target realTimes =
    if target < 0 || hasNegatives realTimes then
        Err NegativeTime

    else if Dict.isEmpty realTimes then
        Err NoWork

    else if target < realTotal realTimes then
        Err Overwork

    else if target == realTotal realTimes then
        Ok realTimes

    else
        let
            diff =
                target - realTotal realTimes
        in
        realTimes
            |> Dict.toList
            |> List.sortBy timeThenID
            |> loop diff (Dict.size realTimes)
            |> Dict.fromList
            |> Ok


loop : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
loop diffLeft step assignment =
    if diffLeft <= 0 then
        assignment

    else
        assignment
            |> List.indexedMap (addToFirst diffLeft)
            |> loop (diffLeft - step) step


timeThenID ( Int, Int ) -> (Int, Int )
timeThenID ( id, time ) =
    ( time, id )


addToFirst : Int -> Int -> ( Int, Int ) -> ( Int, Int )
addToFirst n ix ( id, time ) =
    if ix < n then
        ( id, time + 1 )

    else
        ( id, time )


hasNegatives : Dict Int Int -> Bool
hasNegatives realTimes =
    realTimes
        |> Dict.values
        |> List.any (\x -> x < 0)


realTotal : Dict Int Int -> Int
realTotal realTimes =
    realTimes
        |> Dict.values
        |> List.sum
