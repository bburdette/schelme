module BotGame exposing (applyBotPositions, arena, assignBotPositions, botColors, botPositions, collide, collideArray, collideD2, colorString, defaultBotPositions, drawBot, drawBots, emptyBot, gameStep, getBotColor, isDead, toSvgXY, unDead, updateElt, velCollide)

import Array as A exposing (Array)
import BotLang exposing (Bot, BotControl(..), Vec, botPixelRad, botRadius, botlang, vecPlus)
import Dict exposing (Dict)
import Element exposing (..)
import EvalStep exposing (EvalBodyStep(..), Term(..))
import Run exposing (compile, evalBodyLimit)
import StateGet
import StateSet
import Svg as S exposing (Svg)
import Svg.Attributes as SA


updateElt : Int -> (a -> a) -> Array a -> Array a
updateElt idx updfn array =
    case A.get idx array of
        Just item ->
            A.set idx (updfn item) array

        Nothing ->
            array


emptyBot : Bot
emptyBot =
    { programText = ""
    , name = ""
    , program = Err "uncompiled"
    , step = EbError "no program"
    , position = ( 0, 0 )
    , velocity = ( 0, 0 )
    , accel = ( 0, 0 )
    , dead = False
    }


botColors : Array BotLang.Color
botColors =
    A.fromList
        [ ( 1, 0, 0 )
        , ( 0, 1, 0 )
        , ( 0, 0, 1 )
        , ( 0.75, 0.25, 0 )
        , ( 0, 0.75, 0.25 )
        , ( 0.25, 0, 0.75 )
        , ( 0.75, 0.25, 0.5 )
        , ( 0.5, 0.75, 0.25 )
        , ( 0.25, 0.5, 0.75 )
        ]


colorString : BotLang.Color -> String
colorString ( r, g, b ) =
    let
        ts =
            \v -> String.fromFloat (v * 255)
    in
    "rgb(" ++ ts r ++ "," ++ ts g ++ "," ++ ts b ++ ")"


botPositions : Float -> Int -> List ( Float, Float )
botPositions radius count =
    case count of
        0 ->
            []

        1 ->
            [ ( 0, 0 ) ]

        nz ->
            let
                ang =
                    2 * pi / toFloat nz
            in
            List.map
                (\i ->
                    let
                        a =
                            toFloat i * ang
                    in
                    ( cos a * radius, sin a * radius )
                )
                (List.range 0 (nz - 1))


getBotColor : Int -> BotLang.Color
getBotColor idx =
    Maybe.withDefault ( 0, 0, 0 ) <| A.get (modBy (A.length botColors) idx) botColors


applyBotPositions : Array ( Float, Float ) -> Array Bot -> Array Bot
applyBotPositions locs bots =
    A.indexedMap
        (\i b ->
            { b
                | position = Maybe.withDefault b.position (A.get i locs)
                , accel = ( 0, 0 )
                , velocity = ( 0, 0 )
            }
        )
        bots


defaultBotPositions : Float -> Array Bot -> Array Bot
defaultBotPositions radius bots =
    let
        locs =
            A.fromList
                (botPositions radius (A.length bots))
    in
    A.indexedMap
        (\i b ->
            { b
                | position = Maybe.withDefault ( 0, 0 ) (A.get i locs)
                , accel = ( 0, 0 )
                , velocity = ( 0, 0 )
            }
        )
        bots


unDead : Array Bot -> Array Bot
unDead bots =
    A.map (\bot -> { bot | dead = False }) bots


drawBot : Int -> Bot -> Svg a
drawBot i bot =
    let
        ( x, y ) =
            toSvgXY bot.position
    in
    S.circle
        [ SA.cx (String.fromFloat x)
        , SA.cy (String.fromFloat y)
        , SA.r botPixelRad
        , SA.fill (colorString (getBotColor i))
        ]
        []


drawBots : Array Bot -> Bool -> Element a
drawBots bots sumo =
    el [ width fill, height fill ] <|
        html <|
            S.svg [ SA.width "500", SA.height "500", SA.viewBox "0 0 500 500" ] <|
                arena sumo
                    :: List.indexedMap drawBot (A.toList bots)


arena : Bool -> Svg a
arena sumo =
    S.rect
        ([ SA.x "0"
         , SA.y "0"
         , SA.width "500"
         , SA.height "500"
         , SA.rx "3"
         , SA.ry "3"
         ]
            ++ (if sumo then
                    [ SA.stroke "darkred"
                    , SA.strokeWidth "10"
                    ]

                else
                    []
               )
        )
        []


toSvgXY : Vec -> Vec
toSvgXY ( x, y ) =
    ( x * 250 + 250, y * 250 + 250 )


collideArray : Array Bot -> Array Bot
collideArray bots =
    let
        cm1 =
            A.length bots - 1
    in
    List.foldr
        (\i1 bots1 ->
            List.foldr
                (\i2 bots2 ->
                    case ( A.get i1 bots2, A.get i2 bots2 ) of
                        ( Just b1, Just b2 ) ->
                            case collide b1 b2 of
                                Just ( c1, c2 ) ->
                                    bots2
                                        |> A.set i1 c1
                                        |> A.set i2 c2

                                Nothing ->
                                    bots2

                        _ ->
                            bots2
                )
                bots1
                (List.range (i1 + 1) cm1)
        )
        bots
        (List.range 0 (cm1 - 1))


collideD2 : Float
collideD2 =
    (2 * botRadius) ^ 2


collide : Bot -> Bot -> Maybe ( Bot, Bot )
collide b1 b2 =
    if b1.dead || b2.dead then
        Just ( b1, b2 )

    else
        let
            ( x1, y1 ) =
                b1.position

            ( x2, y2 ) =
                b2.position

            dx =
                x2 - x1

            dy =
                y2 - y1

            d2 =
                dx * dx + dy * dy
        in
        if d2 > collideD2 then
            Nothing

        else
            let
                d =
                    sqrt d2

                ux =
                    dx / d

                uy =
                    dy / d

                ( v1, v2 ) =
                    velCollide ( b1.velocity, b2.velocity ) ( ux, uy )
            in
            Just ( { b1 | velocity = v1 }, { b2 | velocity = v2 } )


velCollide : ( Vec, Vec ) -> Vec -> ( Vec, Vec )
velCollide ( ( v1x, v1y ), ( v2x, v2y ) ) ( ux, uy ) =
    let
        proj1 =
            v1x * ux + v1y * uy

        ( fb1x, fb1y ) =
            ( ux * proj1, uy * proj1 )

        proj2 =
            v2x * -ux + v2y * -uy

        ( fb2x, fb2y ) =
            ( -ux * proj2, -uy * proj2 )

        v1 =
            ( v1x - fb1x + fb2x, v1y - fb1y + fb2y )

        v2 =
            ( v2x - fb2x + fb1x, v2y - fb2y + fb1y )
    in
    ( v1, v2 )


isDead : Vec -> Bool
isDead ( x, y ) =
    let
        lower =
            -1 + botRadius

        upper =
            1 - botRadius
    in
    x < lower || x > upper || y < lower || y > upper


gameStep :
    { model
        | bots : Array Bot
        , prints : Dict Int (List String)
        , evalsPerTurn : Int
        , sumo : Bool
    }
    -> Float
    ->
        { model
            | bots : Array Bot
            , prints : Dict Int (List String)
            , evalsPerTurn : Int
            , sumo : Bool
        }
gameStep model millis =
    let
        botControl =
            BotControl { botidx = 0, bots = model.bots, prints = model.prints }

        -- run all bots scripts, with botcontrol as the state.
        (BotControl nbc) =
            List.foldr
                (\idx (BotControl bc) ->
                    case A.get idx bc.bots of
                        Just bot ->
                            let
                                -- replace botcontrol in the step state with an update botcontrol
                                botstep =
                                    StateSet.setEvalBodyStepState bot.step (BotControl { bc | botidx = idx })

                                -- run the script, updating botcontrol
                                updstep =
                                    evalBodyLimit botstep model.evalsPerTurn
                            in
                            -- get the updated botcontrol.
                            case StateGet.getEvalBodyStepState updstep of
                                Just (BotControl updbc) ->
                                    BotControl { updbc | bots = updateElt idx (\b -> { b | step = updstep }) updbc.bots }

                                Nothing ->
                                    -- if none, then likely an error.  update the bot step.
                                    BotControl { bc | bots = updateElt idx (\b -> { b | step = updstep }) bc.bots }

                        Nothing ->
                            BotControl bc
                )
                botControl
                (List.range 0 (A.length model.bots))

        -- physics update.
        nbots =
            A.map
                (\bot ->
                    if bot.dead then
                        bot

                    else
                        let
                            vel =
                                vecPlus bot.velocity bot.accel

                            pos =
                                vecPlus bot.position vel

                            dead =
                                if model.sumo then
                                    isDead pos

                                else
                                    False
                        in
                        { bot
                            | velocity = vel
                            , position = pos
                            , dead = dead
                            , step =
                                if dead then
                                    EbError "dead"

                                else
                                    bot.step
                        }
                )
                nbc.bots
    in
    { model
        | bots = collideArray nbots
        , prints = nbc.prints
    }


assignBotPositions :
    { model
        | bots : Array Bot
        , prints : Dict Int (List String)
        , go : Bool
    }
    -> List ( Float, Float )
    ->
        { model
            | bots : Array Bot
            , prints : Dict Int (List String)
            , go : Bool
        }
assignBotPositions model ps =
    let
        compiledBots =
            A.indexedMap
                (\idx bot ->
                    let
                        p =
                            compile bot.programText

                        s =
                            p
                                |> Result.map
                                    (\prog ->
                                        EbStart botlang
                                            (BotControl
                                                { botidx = idx
                                                , bots = model.bots
                                                , prints = Dict.empty
                                                }
                                            )
                                            prog
                                    )
                                |> Result.withDefault (EbError "no program")
                    in
                    { bot
                        | program = p
                        , step = s
                    }
                )
                model.bots
    in
    { model
        | bots = unDead <| applyBotPositions (A.fromList ps) compiledBots
        , prints = Dict.empty
        , go = True
    }
