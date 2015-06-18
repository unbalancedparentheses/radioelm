module Radio where

import Signal exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Window
import List

-- model
type alias State =
  { name: String
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
  { name = ""
  , playing = False
  , radios =
    [ radio "mitre" "http://buecrplb01.cienradios.com.ar/mitremdz.mp3"
    , radio "continental" "http://1351.live.streamtheworld.com:80/CONTINENTAL_SC"
    , radio "metro" "http://108.166.161.217:8615/metro.mp3"
    , radio "el mundo" "http://radiostream.elmundoradio.com:8332/"
    ]
  }

-- view
square x y myColor name =
  color myColor (container (x // 2) (y // 2) midBottom (show name))
    |> clickable (Signal.message actions.address (Click name))

view s (x,y) =
    List.foldl (\radio result ->
                  List.append result
                  [flow right [square x y lightBlue radio.name]]
               )
               []
               s.radios
            |> List.append [show s.name] |> flow down

-- actions
type Actions = Click String | Stop

actions : Signal.Mailbox Actions
actions = Signal.mailbox Stop

update action model =
  case action of
    Click x ->
      if model.name == x && model.playing then
        { model | name <- x,
          playing <- False
        }
      else
        { model | name <- x,
          playing <- True
        }


model = foldp (\actions model -> update actions model) initialState actions.signal

-- inputs
main =
  view <~ model ~ Window.dimensions

port name : Signal String
port name =
     Signal.map .name model