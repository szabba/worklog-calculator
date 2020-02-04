module Tests.WorkLog.Calculator exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import WorkLog.Calculator as Calculator


suite : Test
suite =
    describe "WorkLog.Calculator.distribute"
        [ fuzz2 sensibleNumber distribution "never assigns a negative time" <|
            \total realTimes ->
                case Calculator.distribute total realTimes of
                    Ok distributedTimes ->
                        distributedTimes
                            |> Dict.filter (\_ v -> v < 0)
                            |> Expect.equalDicts Dict.empty
                            |> Expect.onFail (Debug.toString distributedTimes ++ " contains negative assignments.")

                    _ ->
                        Expect.pass
        , fuzz2 sensibleNumber distribution "never assigns less than the real time spent" <|
            \total realTimes ->
                case Calculator.distribute total realTimes of
                    Ok distributedTimes ->
                        let
                            realTotal =
                                List.sum <| Dict.values realTimes

                            assignedTotal =
                                List.sum <| Dict.values distributedTimes
                        in
                        assignedTotal |> Expect.atLeast realTotal

                    _ ->
                        Expect.pass
        , fuzz2 sensibleNumber distribution "never assigns less than the total" <|
            \total realTimes ->
                case Calculator.distribute total realTimes of
                    Ok distributedTimes ->
                        distributedTimes
                            |> Dict.values
                            |> List.sum
                            |> Expect.equal total

                    _ ->
                        Expect.pass
        , test "complains when the target total is negative" <|
            \() ->
                Dict.fromList [ ( 0, 10 ) ]
                    |> Calculator.distribute -100
                    |> Expect.equal (Err Calculator.NegativeTime)
        , test "complains when task have negative assignments" <|
            \() ->
                Dict.fromList [ ( 0, -10 ) ]
                    |> Calculator.distribute 100
                    |> Expect.equal (Err Calculator.NegativeTime)
        , test "complains when no work was done" <|
            \() ->
                Dict.empty
                    |> Calculator.distribute 10
                    |> Expect.equal (Err Calculator.NoWork)
        , test "complains when work to report exceeds total" <|
            \() ->
                Dict.fromList [ ( 0, 100 ) ]
                    |> Calculator.distribute 10
                    |> Expect.equal (Err Calculator.Overwork)
        , test "proposes input assignment when it matches the total" <|
            \() ->
                let
                    realTimes =
                        Dict.fromList [ ( 0, 30 ), ( 11, 70 ) ]
                in
                realTimes
                    |> Calculator.distribute 100
                    |> Expect.equal (Ok realTimes)
        ]


distribution : Fuzzer (Dict Int Int)
distribution =
    Fuzz.map Dict.fromList <| list (Fuzz.tuple ( sensibleNumber, sensibleNumber ))


sensibleNumber : Fuzzer Int
sensibleNumber =
    Fuzz.intRange -100 100
