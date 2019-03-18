module Main exposing (Model, Msg(..), main, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import Eval
import Prelude


type Msg
    = ProgramTextChanged String
    | Eval


type alias Model =
    { programText : String
    , programOutput : Result String String
    }


buttonStyle =
    [ Background.color <| rgb255 52 101 164
    , Font.color <| rgb 1 1 1
    , Border.color <| rgb255 32 74 135
    , paddingXY 10 5
    , Border.rounded 3
    ]


init =
    { programText = """(defn (test a b) (+ a b))
  (test 123 456)"""
    , programOutput = Ok ""
    }


view : Model -> Element Msg
view model =
    column [ width fill ]
        [ EI.multiline [ width fill, height shrink ]
            { onChange = ProgramTextChanged
            , text = model.programText
            , placeholder = Nothing
            , label = EI.labelAbove [] <| text "schelme code here: "
            , spellcheck = False
            }
        , EI.button buttonStyle
            { onPress = Just Eval
            , label = text "Eval"
            }
        , el [ Font.bold ] <| text "Result"
        , case model.programOutput of
            Err e ->
                el [ Font.color <| rgb255 204 0 0 ] <| text e

            Ok t ->
                el [ Font.color <| rgb255 115 210 22 ] <| text t
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProgramTextChanged txt ->
            { model | programText = txt }

        Eval ->
            { model
                | programOutput =
                    Eval.compile model.programText
                        |> Result.andThen
                            (\prog ->
                                Eval.run prog Prelude.prelude
                                    |> Result.andThen
                                        (\( ns, term ) ->
                                            Ok <| Eval.showTerm term
                                        )
                            )
            }


main =
    Browser.sandbox
        { init = init
        , view = \model -> layout [] <| view model
        , update = update
        }
