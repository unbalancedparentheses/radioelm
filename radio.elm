import Signal exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Window


-- model

type alias State =
    { name: String
    , playing: Bool
    }

initialState =
  { name = ""
  , playing = False
  }

-- view
square (x, y) myColor name =
  color myColor (container (x // 2) (y // 2) middle (show name))
    |> clickable (Signal.message actions.address (Play name))

view s (x,y) =
  flow down [
    flow right [
      square (x, y) lightBlue "a"
    , square (x, y) lightRed "b"
    ],
    flow right [
      square (x, y) lightPurple "c"
    , square (x, y) green "d"
    ]
    , show s]

-- actions
type Actions = Play String | Pause

actions : Signal.Mailbox Actions
actions = Signal.mailbox Pause

update action model =
  case action of
    Play x ->
      {model | name <- x}


-- inputs

main =
  let model =
    foldp (\actions model -> update actions model) initialState actions.signal
  in
    view <~ model ~ Window.dimensions
