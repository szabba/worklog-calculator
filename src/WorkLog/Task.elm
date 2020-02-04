module WorkLog.Task exposing (Task, add, empty, remove, rename, setMinutesSpent, update)

--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.

import Dict exposing (Dict)
import WorkLog.NumberInput as NumberInput exposing (NumberInput)


type alias Task minutes =
    { name : String
    , minutesSpent : minutes
    }


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
