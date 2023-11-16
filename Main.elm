module Main exposing (..)

import Browser
import Dict
import Html exposing (Html, Attribute, div, input, text, table, tr, td, th, thead)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode
import List.Extra
import Round


-- MAIN


main = Browser.element { init = init, update = update, view = view, subscriptions = (\_ -> Sub.none) }



-- MODEL


type alias Model =
  {
    status : Status,
    referential : Referential
  }

type Status = OK | KO String

type CriterionStatus = Conforme | EnDeploiement | NonConforme | NonApplicable

type alias Referential =
  { 
    title : String,
    url : String,
    description : String,
    version : String,
    updated_at : String,
    author : Author,
    criteres : Dict.Dict String Criterion
  }

referentialDecoder : Json.Decode.Decoder Referential
referentialDecoder = Json.Decode.map7 Referential
  ( Json.Decode.field "title" Json.Decode.string)
  ( Json.Decode.field "url" Json.Decode.string)
  ( Json.Decode.field "description" Json.Decode.string)
  ( Json.Decode.field "version" Json.Decode.string)
  ( Json.Decode.field "updated_at" Json.Decode.string)
  ( Json.Decode.field "author" authorDecoder)
  ( Json.Decode.field "criteres" (Json.Decode.list criterionDecoder |> Json.Decode.andThen ( \l -> Dict.fromList l |> Json.Decode.succeed )))

type alias Author =
  {
    name : String,
    url : String
  }

authorDecoder : Json.Decode.Decoder Author
authorDecoder = Json.Decode.map2 Author
    ( Json.Decode.field "name" Json.Decode.string )
    ( Json.Decode.field "url" Json.Decode.string )

type alias CriterionID = String

type alias Criterion =
  {
    --id : String,
    category : Int,
    item : Int,
    url : String,
    critere : String,
    --thematique : String,
    objectif : String,
    miseEnOeuvre : String,
    controle : String,
    status : CriterionStatus
  }

criterionDecoder : Json.Decode.Decoder (String, Criterion)
criterionDecoder = Json.Decode.map8 Criterion
  --( Json.Decode.field "id" Json.Decode.string )
  ( Json.Decode.field "id" Json.Decode.string |> (Json.Decode.andThen ( \v -> String.split "." v |> List.head |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0 |> Json.Decode.succeed )))
  ( Json.Decode.field "id" Json.Decode.string |> (Json.Decode.andThen ( \v -> String.split "." v |> List.Extra.getAt 1 |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0 |> Json.Decode.succeed )))
  ( Json.Decode.field "url" Json.Decode.string )
  ( Json.Decode.field "critere" Json.Decode.string )
  --( Json.Decode.field "thematique" Json.Decode.string )
  ( Json.Decode.field "objectif" Json.Decode.string )
  ( Json.Decode.field "miseEnOeuvre" Json.Decode.string )
  ( Json.Decode.field "controle" Json.Decode.string )
  ( Json.Decode.succeed NonConforme )
  |> Json.Decode.andThen (\c -> Json.Decode.succeed (getID c, c))

getID : Criterion -> String
getID c = (String.fromInt c.category) ++ "." ++ (String.fromInt c.item)

categories =
  [ "Stratégie"
  , "Spécifications"
  , "Architecture"
  , "UX/UI"
  , "Contenus"
  , "Frontend"
  , "Backend"
  , "Hébergement"
  ]



emptyRef = { title = "", url = "", description = "", version = "", updated_at = "", author = { name = "", url = "" }, criteres = Dict.empty }

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


type Msg = SetStatus String CriterionStatus


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetStatus criterionID status ->
      let
        updatedCriteria = Dict.update criterionID (updateCriterionStatus status) model.referential.criteres
        oldRef = model.referential
        updatedRef = { oldRef | criteres = updatedCriteria }
      in
        ( { model | referential = updatedRef }, Cmd.none )

updateCriterionStatus : CriterionStatus -> Maybe Criterion -> Maybe Criterion
updateCriterionStatus status mCriterion =
  case mCriterion of
    Just c -> Just { c | status = status }
    Nothing -> Nothing

-- VIEW

view : Model -> Html Msg
view model = case model.status of
  KO msg ->
    div [] [ text ( "Erreur : " ++ msg ) ]
  
  OK ->
    div [] [ viewScore (model.referential.criteres |> Dict.values), table [] (List.concat (List.indexedMap (categoryTable model) categories)) ]

viewScore : List Criterion -> Html Msg
viewScore criteria =
  let
    nbTotal = criteria |> List.length
    notApplicable = criteria |> List.filter (\c -> c.status == NonApplicable) |> List.length
    conforme = criteria |> List.filter (\c -> c.status == Conforme) |> List.length
    score = 100.0 * toFloat conforme / (toFloat nbTotal - toFloat notApplicable) |> round |> String.fromInt |> (\s -> s++"%")
  in
    th [] [ div [ class "score" ] [ div [class "progressbar" ] [ div [ class "progress", style "width" score ] [] ], text score ] ]


categoryTable model index category = (catHeader model index category :: tableHeader :: (tableRows model (index+1)))

catHeader model index category =
  let
    progressBar = viewScore (model.referential.criteres |> Dict.values |> List.filter (\c -> c.category == index+1))
  in
    thead [ class "category-header" ]
    [ th [colspan 2] [ text ("Catégorie " ++ (String.fromInt (index+1)) ++ " : " ++ category) ]
    , progressBar
    ]

tableHeader : Html Msg
tableHeader =
  thead []
    [ th [] [ text "#" ]
    , th [] [ text "Critère" ]
    , th [ class "status" ] [ text "Statut" ]
    ]

tableRows : Model -> Int -> List (Html Msg)
tableRows model cat =
  Dict.toList model.referential.criteres
  |> List.filter (\(_, c) -> c.category == cat)
  |> List.sortBy funnyID
  |> List.map viewCriterion

funnyID : (String, Criterion) -> Int
funnyID (_, c) = c.category * 100 + c.item

statusString : CriterionStatus -> String
statusString s = case s of
  NonConforme -> "Non conforme ❌"
  NonApplicable -> "Non applicable ☠️"
  Conforme -> "Conforme ✅"
  EnDeploiement -> "En déploiement 🏗️"

statusClass : CriterionStatus -> String
statusClass s = case s of
  NonConforme -> "status-ko"
  NonApplicable -> "status-na"
  Conforme -> "status-ok"
  EnDeploiement -> "status-wip"

rotateStatus : CriterionStatus -> CriterionStatus
rotateStatus s = case s of
  NonConforme -> EnDeploiement
  EnDeploiement -> Conforme
  Conforme -> NonApplicable
  NonApplicable -> NonConforme

viewCriterion : (String, Criterion) -> Html Msg
viewCriterion (id, c) =
  tr [ class (statusClass c.status)]
    [ td [] [ text id ]
    , td [ class "criteria-cell" ] [ text c.critere ]
    , td [ class "status", onClick (SetStatus (getID c) (rotateStatus c.status))] [ text (statusString c.status) ]
    ]