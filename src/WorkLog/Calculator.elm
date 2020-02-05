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


distribute : Int -> Dict comparable Int -> Result Problem (Dict comparable Int)
distribute target realTimes =
    if target < 0 || hasNegatives realTimes then
        Err NegativeTime

    else if Dict.isEmpty realTimes then
        Err NoWork

    else
        let
            realTotal =
                realTimes |> Dict.values |> List.sum

            diff =
                target - realTotal

            scale =
                toFloat target / toFloat realTotal
        in
        if target < realTotal then
            Err Overwork

        else
            let
                scaledTimes =
                    realTimes |> Dict.map (scaleBy scale)

                scaledTotal =
                    scaledTimes |> Dict.foldl (\_ time acc -> acc + time) 0

                overshoot =
                    scaledTotal - target
            in
            scaledTimes
                |> Dict.toList
                |> List.sortBy timeThenID
                |> List.indexedMap (takeFromFirst overshoot)
                |> Dict.fromList
                |> Ok


scaleBy : Float -> id -> Int -> Int
scaleBy factor _ time =
    ceiling <| factor * toFloat time


timeThenID : ( id, Int ) -> ( Int, id )
timeThenID ( id, time ) =
    ( time, id )


takeFromFirst : Int -> Int -> ( id, Int ) -> ( id, Int )
takeFromFirst n ix ( id, time ) =
    if ix < n then
        ( id, time - 1 )

    else
        ( id, time )


hasNegatives : Dict id Int -> Bool
hasNegatives realTimes =
    realTimes
        |> Dict.values
        |> List.any (\x -> x < 0)
