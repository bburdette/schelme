module SelectString exposing (view)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI



{- dialog/pane displaying a list of strings and allowing the user to select one and
   click ok, or dismiss by canceling or clicking outside the pane.
-}


view : List String -> (String -> m) -> m -> Element m
view strings selectmsg cancelmsg =
    column [] <|
        List.indexedMap (\i s -> row [ onClick (selectmsg s) ] [ text s ]) model.strings
