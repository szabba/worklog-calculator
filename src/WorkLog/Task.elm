module WorkLog.Task exposing (Task, empty, rename, setMinutesSpent)

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
