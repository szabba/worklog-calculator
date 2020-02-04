module WorkLog.Main exposing (main)

--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import WorkLog.NumberInput as NumberInput exposing (NumberInput)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { totalMinutes : NumberInput
    }


init : Model
init =
    { totalMinutes = NumberInput.fromValue <| 8 * 60
    }


type Msg
    = TotalEntered String


view : Model -> Html Msg
view model =
    H.div []
        [ viewMinutesToLog model.totalMinutes ]


viewMinutesToLog : NumberInput -> Html Msg
viewMinutesToLog { raw } =
    H.p []
        [ H.text "I need to log "
        , H.input [ HE.onInput TotalEntered, HA.value raw ] []
        , H.text " minutes."
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        TotalEntered rawInput ->
            { model | totalMinutes = NumberInput.fromRawInput rawInput }
