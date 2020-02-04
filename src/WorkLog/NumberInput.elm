module WorkLog.NumberInput exposing (NumberInput, fromRawInput, fromValue)

--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


type alias NumberInput =
    { raw : String
    , parsed : Maybe Int
    }


fromValue : Int -> NumberInput
fromValue v =
    { raw = String.fromInt v
    , parsed = Just v
    }


fromRawInput : String -> NumberInput
fromRawInput raw =
    { raw = raw
    , parsed = String.toInt raw
    }
