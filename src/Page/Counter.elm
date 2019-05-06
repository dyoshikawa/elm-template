module Page.Counter exposing (Model, Msg, init, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (..)
import Url



-- MODEL


type alias Model =
    { count : Int }


init : ( Model, Cmd Msg )
init =
    ( { count = 0
      }
    , Cmd.none
    )



--UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            let
                newCount =
                    model.count + 1
            in
            ( { model | count = newCount }, Cmd.none )

        Decrement ->
            let
                newCount =
                    model.count - 1
            in
            ( { model | count = newCount }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
