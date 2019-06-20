module SelectString exposing (view)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as EE
import Element.Font as Font
import Element.Input as EI



{- dialog/pane displaying a list of strings and allowing the user to select one and
   click ok, or dismiss by canceling or clicking outside the pane.
-}


view : String -> List String -> (String -> m) -> m -> Element m
view title strings selectmsg cancelmsg =
    column [ BG.color <| rgb 1 1 1 ] <|
        (el [ Font.bold ] <| text title)
            :: List.indexedMap (\i s -> row [ EE.onClick (selectmsg s) ] [ text s ]) strings
