module BotGame exposing (applyBotPositions, arena, botColors, botPositions, collide, collideArray, collideD2, colorString, defaultBotPositions, drawBot, drawBots, emptyBot, getBotColor, isDead, toSvgXY, unDead, velCollide)

import Array as A exposing (Array)
import BotLang exposing (Bot, BotControl(..), Vec, allreference, botPixelRad, botRadius, botSpawnRadius, botlang, botreference, vecPlus)
import Element exposing (..)
import EvalStep exposing (EvalBodyStep(..), NameSpace, Term(..))
import Svg as S exposing (Svg)
import Svg.Attributes as SA


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
