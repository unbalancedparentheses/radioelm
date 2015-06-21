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
  { radio: Radio
  , playing: Bool
  , radios: List Radio
  }

type alias Radio =
  { name: String
  , url: String
  }

radio name url =
  { name = name
  , url = url
  }

initialState =
  { radio = {name = "", url = ""}
  , playing = False
  , radios =
    [ radio "mitre" "http://buecrplb01.cienradios.com.ar/mitremdz.mp3"
    , radio "continental" "http://1351.live.streamtheworld.com:80/CONTINENTAL_SC"
    , radio "metro" "http://108.166.161.217:8615/metro.mp3"
    , radio "radio argentina" "http://wmserver3.aginet.com.ar:13574/;stream/1/"
    , radio "los 40" "http://5133.live.streamtheworld.com:80/LOS40_ARGENTINA_SC"
    , radio "la 100" "http://buecrplb01.cienradios.com.ar/la100.aac"
    , radio "espn" "http://edge.espn.cdn.abacast.net/espn-deportesmp3-48"
    , radio "imagina" "http://7309.live.streamtheworld.com:80/IMAGINA_ARGENTINA_SC"
    , radio "rock & pop" "http://69.4.236.136:9988/;"
    -- , radio "radio 10" "rtmp://radio10.stweb.tv:1935/radio10/"
    ]
  }

-- view
txt string =
    Text.fromString string
        |> Text.color white
        |> Text.monospace
        |> Graphics.Element.leftAligned

square x y myColor radio =
    Graphics.Element.color myColor (container (x // 2) (y // 2) midBottom (txt radio.name))
    |> clickable (Signal.message actions.address (Click radio))

view state (x,y) =
    List.foldl (\radio result ->
                  List.append result
                  [flow right [square x y lightBlue radio]]
               )
               []
               state.radios
            |> List.append [show state.radio.name] |> flow down

-- actions
type Actions = Click Radio | Stop

actions : Signal.Mailbox Actions
actions = Signal.mailbox Stop

update action model =
  case action of
    Click radio ->
      let playing =
          if model.radio.name == radio.name && model.playing then
              False
          else
              True
      in { model |
           radio <- radio,
           playing <- playing
         }

model = foldp (\actions model -> update actions model) initialState actions.signal

-- inputs
main =
  view <~ model ~ Window.dimensions

port name : Signal State
port name =
     (\x -> x) <~ model