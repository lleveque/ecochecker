module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, table, tr, td, th, thead)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode



-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions = (\_ -> Sub.none) }



-- MODEL


type alias Model =
  {
    status : Status,
    referential : Referential
  }

type Status = OK | KO String

type alias Referential =
  { 
    title : String,
    url : String,
    description : String,
    version : String,
    updated_at : String,
    author : Author,
    criteres : List Criterion
  }

referentialDecoder : Json.Decode.Decoder Referential
referentialDecoder = Json.Decode.map7 Referential
  ( Json.Decode.field "title" Json.Decode.string)
  ( Json.Decode.field "url" Json.Decode.string)
  ( Json.Decode.field "description" Json.Decode.string)
  ( Json.Decode.field "version" Json.Decode.string)
  ( Json.Decode.field "updated_at" Json.Decode.string)
  ( Json.Decode.field "author" authorDecoder)
  ( Json.Decode.field "criteres" (Json.Decode.list criterionDecoder))

type alias Author =
  {
    name : String,
    url : String
  }

authorDecoder : Json.Decode.Decoder Author
authorDecoder = Json.Decode.map2 Author
    ( Json.Decode.field "name" Json.Decode.string )
    ( Json.Decode.field "url" Json.Decode.string )

type alias Criterion =
  {
    id : String,
    url : String,
    critere : String,
    thematique : String,
    objectif : String,
    miseEnOeuvre : String,
    controle : String
  }

criterionDecoder : Json.Decode.Decoder Criterion
criterionDecoder = Json.Decode.map7 Criterion
  ( Json.Decode.field "id" Json.Decode.string )
  ( Json.Decode.field "url" Json.Decode.string )
  ( Json.Decode.field "critere" Json.Decode.string )
  ( Json.Decode.field "thematique" Json.Decode.string )
  ( Json.Decode.field "objectif" Json.Decode.string )
  ( Json.Decode.field "miseEnOeuvre" Json.Decode.string )
  ( Json.Decode.field "controle" Json.Decode.string )

emptyRef = { title = "", url = "", description = "", version = "", updated_at = "", author = { name = "", url = "" }, criteres = [] }

init: Json.Decode.Value -> (Model, Cmd Msg)
init jsonReferential =
    case Json.Decode.decodeValue referentialDecoder jsonReferential of

        Ok referential ->
          (
            { referential = referential
            , status = OK
            }
          , Cmd.none
          )

        Err e ->
          (
            { referential = emptyRef
            , status = KO (Json.Decode.errorToString e)
            }
          , Cmd.none
          )


-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      ( model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = case model.status of
  KO msg ->
    div [] [ text ( "Erreur : " ++ msg ) ]
  
  OK ->
    div [] [ table [] (tableHeader :: (tableRows model)) ]

tableHeader : Html Msg
tableHeader =
  thead []
    [ th [] [ text "#" ]
    , th [] [ text "Critère" ]
    , th [] [ text "Statut" ]
    ]

tableRows : Model -> List (Html Msg)
tableRows model = List.map viewCriterion model.referential.criteres

viewCriterion : Criterion -> Html Msg
viewCriterion c =
  tr []
    [ td [] [ text c.id ]
    , td [] [ text c.critere ]
    , td [] [ text "Non conforme" ]
    ]