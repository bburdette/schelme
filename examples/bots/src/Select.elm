module Select exposing (Model, init)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI



{- dialog/pane displaying a list of strings and allowing the user to select one and
   click ok, or dismiss by canceling or clicking outside the pane.
-}


type alias Model =
    { strings : List String
    , selected : Maybe Int
    }


type Msg
    = Noop


type Command
    = Canceled
    | Selected String
    | None


init : List String -> Model
init strings =
    { strings = List.sort strings
    , selected = Nothing
    }


view : Model -> Element Msg
view model =
    Element.none


update : Msg -> Model -> ( Model, Command )
update msg model =
    ( model, None )
