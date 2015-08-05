module Radio where

import Signal exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Color
import Window
import List
import Text

-- model
type alias State =
  { name: String
  , url: String
  , playing: Bool
  }

type alias Radio =
  { name: String
  , url: String
  , color: Color.Color
  }

radio name url (r,g,b) =
  { name = name
  , url = url
  , color = Color.rgb r g b
  }

radios =
    [ radio "mitre" "http://buecrplb01.cienradios.com.ar/Mitre790.aac" (17, 63, 140)
    , radio "continental" "http://1351.live.streamtheworld.com:80/CONTINENTAL_SC" (241, 141, 5)
    , radio "metro" "http://108.166.161.217:8615/metro.mp3" (0, 161, 203)
    , radio "radio argentina" "http://wmserver3.aginet.com.ar:13574/;stream/1/" (97, 174, 36)
    , radio "los 40" "http://5133.live.streamtheworld.com:80/LOS40_ARGENTINA_SC" (208, 209, 2)
    , radio "la 100" "http://buecrplb01.cienradios.com.ar/la100.aac" (50, 116, 44)
    , radio "espn" "http://edge.espn.cdn.abacast.net/espn-deportesmp3-48" (215, 0, 96)
    , radio "imagina" "http://7309.live.streamtheworld.com:80/IMAGINA_ARGENTINA_SC" (229, 64, 40)
    , radio "rock & pop" "http://69.4.236.136:9988/;" (1, 164, 164)
    , radio "del plata" "http://bel-se-1.se.amdelplata.activecds.telecomcdn.com.ar/amdelplata.mp3" (97, 97, 97)
    , radio "cadena 3" "http://coe-se-2.se.cadena3.activecds.telecomcdn.com.ar/am700.mp3" (60, 181, 181)
    , radio "radio argentina" "http://wmserver3.aginet.com.ar:13574/;stream/1/" (180, 178, 174)
    , radio "nova" "http://buecrplb01.cienradios.com.ar/fm979.mp3" (142, 68, 35)
    , radio "sonic" "http://live.chicago.cdn.sonic.fm:8000/live128" (215, 0, 96)
    , radio "el mundo" "http://radiostream.elmundoradio.com:8332/;" (50, 116, 44)
    ]

initialName =
    case List.head radios of
      Just r ->
          r.name
      Nothing ->
          ""

initialUrl =
    case List.head radios of
      Just r ->
          r.url
      Nothing ->
          ""

initialState =
    { name = initialName
    , url = initialUrl
    , playing = False
    }

-- utils
chunk n list =
   chunk2 [] n list

chunk2 acc n list =
  case list of
    [] -> List.reverse acc
    _ ->
      let aux = List.take n list
          newList = List.drop n list
      in
        chunk2 (aux :: acc) n newList

-- view
view state (x,y) =
    let numberofcards = cards x
    in
      List.map (\r -> square x y r) radios
          |> List.append [(control x y state.playing)]
          |> chunk numberofcards
          |> List.map (\r -> flow right r)
          |> flow down

square x y radio =
    let numberofcards = cards x
        xSize = x // numberofcards
        ySize = y // numberofcards
    in
      container xSize ySize middle (flow down [txt radio.name])
          |> Graphics.Element.color radio.color
          |> clickable (Signal.message actions.address (Click radio))

control x y playing =
    let text =
        if playing then
            "pause"
        else
            "play"
        numberofcards = cards x
        xSize = x // numberofcards
        ySize = y // numberofcards
    in
      container xSize ySize middle (flow down [txt text])
          |> Graphics.Element.color Color.black
          |> clickable (Signal.message actions.address Control)

txt string =
    Text.fromString string
        |> Text.color Color.white
        |> Text.height 25
        |> Text.typeface ["sans-serif", "helvetica"]
        |> Graphics.Element.centered

cards x =
    if | x < 300 -> 1
       | x < 600 -> 2
       | x < 850 -> 3
       | otherwise -> 4

-- actions
type Actions = Click Radio | Control

actions : Signal.Mailbox Actions
actions = Signal.mailbox Control

update action model =
  case action of
    Control ->
        let playing =
            if model.playing then
                False
            else
                True
        in
          { model |
            playing <- playing
          }
    Click radio ->
        { model |
          url <- radio.url
        , name <- radio.name
        , playing <- True
        }

model = foldp (\actions model -> update actions model) initialState actions.signal

-- inputs
main =
  view <~ model ~ Window.dimensions

port name : Signal State
port name =
     identity <~ model