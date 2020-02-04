module WorkLog.Task exposing (Task, add, empty, remove, rename, setMinutesSpent, update)

--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.

import Dict exposing (Dict)
import WorkLog.NumberInput as NumberInput exposing (NumberInput)


type alias Task =
    { name : String
    , minutesSpent : NumberInput
    }


empty : Task
empty =
    { name = ""
    , minutesSpent = NumberInput.fromRawInput ""
    }


rename : String -> Task -> Task
rename newName task =
    { task | name = newName }


setMinutesSpent : String -> Task -> Task
setMinutesSpent rawMinutes task =
    { task | minutesSpent = NumberInput.fromRawInput rawMinutes }


add : Dict Int Task -> Dict Int Task
add tasks =
    tasks
        |> Dict.insert (nextID tasks) empty


nextID : Dict Int Task -> Int
nextID tasks =
    tasks
        |> Dict.keys
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0


remove : Int -> Dict Int Task -> Dict Int Task
remove =
    Dict.remove


update : Int -> (Task -> Task) -> Dict Int Task -> Dict Int Task
update id updateFn tasks =
    tasks
        |> Dict.update id (Maybe.map updateFn)
