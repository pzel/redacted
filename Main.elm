--import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
--import Keyboard
import Char exposing (fromCode)
import Random exposing (Generator(..), Seed, float, initialSeed, step)
import Tuple exposing (second)

main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Config =
  { host : String, seed : Int }

type alias RNG =
  (Generator Float, Seed)

type alias Model =
  { config : Config
  , currentInput : String
  , output : String
  , rng : RNG
  }

type Msg
  = Input String
  | Reshuffle
  | More

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch []
--  Sub.batch [ Keyboard.downs KeyDown ]

init : Config -> (Model, Cmd Msg)
init config =
  (Model config "" "" (initSeed config), Cmd.none)

initSeed : Config -> RNG
initSeed c =
  (float 0 1, initialSeed c.seed)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input s ->
      {model
        | currentInput = s
        , output = redact s model.rng} ! []
    Reshuffle ->
      {model
        | output = redact model.currentInput (newRng model.rng)
        , rng = (newRng model.rng)} ! []
    More ->
      {model
        | output = redact model.output (newRng model.rng)
        , rng = (newRng model.rng)} ! []

view : Model -> Html Msg
view model =
  div [] [
     button [onClick Reshuffle] [ text "Redact again" ]
    , button [onClick More] [ text "Redact more" ]
    , div [] []
    , textarea [id "main-input"
               , autofocus True
               , onInput Input
               , value model.currentInput] []
    , div [id "main-output"] [text model.output]
    ]

redact : String -> RNG -> String
redact s (gen,seed) =
  let (result, newSeed) = String.foldl (maybeRedact gen) ("",seed) s
  in String.reverse result

maybeRedact : Generator Float -> Char -> (String, Seed) -> (String, Seed)
maybeRedact gen c (acc,seed) =
  let (p, nextSeed) = Random.step gen seed
      maybeHead = String.uncons acc
      c1 = case (maybeHead, c) of
             (_, '\n') -> c
             -- end of redacted word, see if we continue
             (Just ('█',_), ' ') -> if p < 0.4 then '█' else c
             -- middle of redacted word, continue
             (Just ('█',_), _) -> '█'
             -- a new word, we may start redacting with low p.
             (Just (' ',_), c) -> if p < 0.2 then '█' else c
             -- middle of nonredacted word, just continue
             (_, c) -> c
  in (String.cons c1 acc, nextSeed)

newRng : RNG -> RNG
newRng (gen,seed) =
  let (_, newSeed) = Random.step gen seed
  in (gen,newSeed)
