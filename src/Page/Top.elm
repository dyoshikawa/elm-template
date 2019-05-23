module Page.Top exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { count : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0, Cmd.none )



-- UPDATE


type Msg
    = Something


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Something ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "TOP PAGE" ]
        , div [] [ text <| String.fromInt model.count ]
        ]
