module Radio where

import Signal exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Window
import List
import Text exposing (..)

-- model
type alias State =
  { name: String
  , url: String
  , playing: Bool
  }

type alias Radio =
  { name: String
  , url: String
  , color: Color
  }

initialState =
  { name = ""
  , url = ""
  , playing = False
  }

radio name url (r,g,b) =
  { name = name
  , url = url
  , color = Color.rgb r g b
  }

radios =
    [ radio "mitre" "http://buecrplb01.cienradios.com.ar/mitremdz.mp3" (17, 63, 140)
    , radio "continental" "http://1351.live.streamtheworld.com:80/CONTINENTAL_SC" (241, 141, 5)
    , radio "metro" "http://108.166.161.217:8615/metro.mp3" (0, 161, 203)
    , radio "radio argentina" "http://wmserver3.aginet.com.ar:13574/;stream/1/" (97, 174, 36)
    , radio "los 40" "http://5133.live.streamtheworld.com:80/LOS40_ARGENTINA_SC" (208, 209, 2)
    , radio "la 100" "http://buecrplb01.cienradios.com.ar/la100.aac" (50, 116, 44)
    , radio "espn" "http://edge.espn.cdn.abacast.net/espn-deportesmp3-48" (1, 164, 164)
    , radio "imagina" "http://7309.live.streamtheworld.com:80/IMAGINA_ARGENTINA_SC" (229, 64, 40)
    , radio "rock & pop" "http://69.4.236.136:9988/;" (215, 0, 96)
    -- , radio "radio 10" "rtmp://radio10.stweb.tv:1935/radio10/"
    ]

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
    List.map (\r -> square x y r) radios
        |> List.append [(controls x y state.playing)]
        |> chunk 3
        |> List.map (\r -> flow right r)
        |> flow down

square x y radio =
    container (x // 3) (y // 3) middle (flow down [txt radio.name])
        |> Graphics.Element.color radio.color
        |> clickable (Signal.message actions.address (Click radio))

controls x y playing =
    let text =
        if playing then
            "Pause"
        else
            "Play"
    in
      container (x // 3) (y // 3) middle (flow down [txt text])
          |> Graphics.Element.color black
          |> clickable (Signal.message actions.address Control)

txt string =
    Text.fromString string
        |> Text.color white
        |> Text.monospace
        |> Text.height 25
        |> Graphics.Element.centered

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