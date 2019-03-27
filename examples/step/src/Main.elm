module Main exposing (Model, Msg(..), main, view)

-- import EvalStep

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as EI
import EvalStep as Eval exposing (Term(..), showTerm)
import PreludeStep as Prelude exposing (evalArgsSideEffector)


type Msg
    = ProgramTextChanged String
    | Eval


type alias Color =
    ( Float, Float, Float )


type alias Model =
    { programText : String
    , programOutput : Result String String
    , finalNamespace : Eval.NameSpace Color
    , color : Color
    }


buttonStyle =
    [ Background.color <| rgb255 52 101 164
    , Font.color <| rgb 1 1 1
    , Border.color <| rgb255 32 74 135
    , paddingXY 10 5
    , Border.rounded 3
    ]


setColor : Prelude.NoEvalSideEffector Color
setColor ns a argterms =
    case argterms of
        [ TNumber r, TNumber g, TNumber b ] ->
            Ok ( ns, ( r, g, b ), TList [] )

        _ ->
            Err (String.concat ("setColor args should be 3 numbers!  " :: List.map Eval.showTerm argterms))


preludeNColor =
    Prelude.prelude
        |> Dict.insert "setColor"
            (TSideEffector (evalArgsSideEffector setColor))


programText =
    """(defn (test a b) (+ a b))
(def x 123)
(defn (setRed r) (setColor 0.1 0.1 r))
(def y 456)
(test x y)
(setRed 0.7)"""


pg2 =
    """(and true false)"""


init =
    { programText = pg2
    , programOutput = Ok ""
    , finalNamespace = Dict.empty
    , color = ( 1, 1, 1 )
    }


viewNamespace : Eval.NameSpace a -> Element Msg
viewNamespace ns =
    column [ width fill ] <|
        List.map
            (\( name, term ) ->
                row [ width fill, spacing 7 ] [ el [ width fill ] <| text name, el [ width fill ] <| text <| Eval.showTerm term ]
            )
            (Dict.toList ns)


view : Model -> Element Msg
view model =
    let
        ( r, g, b ) =
            Debug.log "color"
                model.color
    in
    column [ width fill ]
        [ row [ width fill ]
            [ EI.multiline [ width fill, height shrink, alignTop ]
                { onChange = ProgramTextChanged
                , text = model.programText
                , placeholder = Nothing
                , label = EI.labelAbove [ Font.bold ] <| text "schelme code here: "
                , spellcheck = False
                }
            , column [ width fill ] [ el [ Font.bold ] <| text "initial namespace", viewNamespace preludeNColor ]
            ]
        , EI.button buttonStyle
            { onPress = Just Eval
            , label = text "Eval"
            }
        , row [ width fill ]
            [ column [ width fill, alignTop, spacing 5 ]
                [ el [ Font.bold ] <| text "Program output:"
                , case model.programOutput of
                    Err e ->
                        paragraph [ Font.color <| rgb255 204 0 0 ] [ text e ]

                    Ok t ->
                        paragraph [ Font.color <| rgb255 115 210 22 ] [ text t ]
                , el [ Font.bold ] <| text "Side effect color:"
                , el [ width (px 150), height (px 150), Background.color (rgb r g b) ] <| text "    "
                ]
            , column [ width fill, alignTop ] [ el [ Font.bold ] <| text "final namespace", viewNamespace model.finalNamespace ]
            ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ProgramTextChanged txt ->
            { model | programText = txt }

        Eval ->
            let
                rs =
                    Eval.compile model.programText
                        |> Result.andThen
                            (\prog ->
                                Eval.run preludeNColor model.color prog
                            )
            in
            case rs of
                Ok ( finalns, color, output ) ->
                    { model
                        | programOutput = Ok (showTerm output)
                        , finalNamespace = finalns
                        , color = color
                    }

                Err e ->
                    { model
                        | programOutput = Err e
                        , finalNamespace = Dict.empty
                        , color = ( 1, 1, 1 )
                    }


main =
    Browser.sandbox
        { init = init
        , view = \model -> layout [] <| view model
        , update = update
        }
