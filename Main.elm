module Main exposing (..)

import Browser
import Dict
--import Html exposing Html, Attribute, div, input, text, table, tr, td, th, thead, summary, details)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode
import List.Extra
import Round
import Markdown.Parser as Markdown
import Markdown.Renderer
import Url

-- MAIN


main = Browser.element { init = init, update = update, view = view, subscriptions = (\_ -> Sub.none) }



-- MODEL


type alias Model =
  {
    status : Status,
    referential : Referential,
    evaluations : Dict.Dict String Evaluation,
    textCandidate : String,
    isCandidateValidURL : Bool,
    selectedSite : Maybe String,
    sortOrder : Order
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
    criteres : Dict.Dict String Criterion
  }

type alias Author =
  {
    name : String,
    url : String
  }

emptyRef =
  { title = ""
  , url = ""
  , description = ""
  , version = ""
  , updated_at = ""
  , author =
    { name = ""
    , url = ""
    }
  , criteres = Dict.empty
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

authorDecoder : Json.Decode.Decoder Author
authorDecoder = Json.Decode.map2 Author
    ( Json.Decode.field "name" Json.Decode.string )
    ( Json.Decode.field "url" Json.Decode.string )

type alias Criterion =
  {
    category : Int,
    item : Int,
    url : String,
    critere : String,
    objectif : String,
    miseEnOeuvre : String,
    controle : String
  }

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

type CriterionStatus = AEvaluer | NonConforme | EnDeploiement | Conforme | NonApplicable

type alias Evaluation = Dict.Dict String CriterionStatus

criterionDecoder : Json.Decode.Decoder (String, Criterion)
criterionDecoder = Json.Decode.map7 Criterion
  ( Json.Decode.field "id" Json.Decode.string |> (Json.Decode.andThen ( \v -> String.split "." v |> List.head |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0 |> Json.Decode.succeed )))
  ( Json.Decode.field "id" Json.Decode.string |> (Json.Decode.andThen ( \v -> String.split "." v |> List.Extra.getAt 1 |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0 |> Json.Decode.succeed )))
  ( Json.Decode.field "url" Json.Decode.string )
  ( Json.Decode.field "critere" Json.Decode.string )
  ( Json.Decode.field "objectif" Json.Decode.string )
  ( Json.Decode.field "miseEnOeuvre" Json.Decode.string )
  ( Json.Decode.field "controle" Json.Decode.string )
  |> Json.Decode.andThen (\c -> Json.Decode.succeed (getID c, c))

getID : Criterion -> String
getID c = (String.fromInt c.category) ++ "." ++ (String.fromInt c.item)

type Order = FunnyID | StatusInc | StatusDec

init: Json.Decode.Value -> (Model, Cmd Msg)
init jsonReferential =
    case Json.Decode.decodeValue referentialDecoder jsonReferential of

        Ok referential ->
          (
            { referential = referential
            , status = OK
            , evaluations = Dict.empty
            , selectedSite = Nothing
            , textCandidate = "https://www.esaip.org"
            , isCandidateValidURL = True
            , sortOrder = FunnyID
            }
          , Cmd.none
          )

        Err e ->
          (
            { referential = emptyRef
            , status = KO (Json.Decode.errorToString e)
            , evaluations = Dict.empty
            , selectedSite = Nothing
            , textCandidate = ""
            , isCandidateValidURL = False
            , sortOrder = FunnyID
            }
          , Cmd.none
          )


-- UPDATE


type Msg =
  InputText String
  | LaunchEvaluation
  | SetStatus String CriterionStatus
  | NukeSelectedSite
  | SetOrder Order


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =

  case msg of

    InputText text ->
      let
        valid = case (Url.fromString text) of
          Just url -> True
          Nothing -> False
      in
        ({ model | textCandidate = text, isCandidateValidURL = valid }, Cmd.none)

    LaunchEvaluation -> ({ model | selectedSite = Just model.textCandidate }, Cmd.none)

    NukeSelectedSite -> ({ model | selectedSite = Nothing }, Cmd.none)

    SetStatus criterionID status ->
      case model.selectedSite of
        Nothing -> ( model, Cmd.none )
        Just url ->
          let
            evaluation = Dict.get url model.evaluations |> Maybe.withDefault Dict.empty
            updatedEvaluation = Dict.insert criterionID status evaluation
            updatedEvaluations = Dict.insert url updatedEvaluation model.evaluations
          in
            ( { model | evaluations = updatedEvaluations }, Cmd.none )

    SetOrder order -> ({ model | sortOrder = order }, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model = case model.status of
  KO msg ->
    div [] [ text ( "Erreur : " ++ msg ) ]
  
  OK ->
    div []
      ( case model.selectedSite of
          Nothing ->
            [ h1 [] [ text "Bloc-notes RGESN" ]
            , span [] [ text "Bienvenue sur l'éco-évaluateur de sites web !" ]
            , div []
              [ span [] [ text "Entrez l'adresse du site que vous allez évaluer : " ]
              , label [ for "currentSite" ] [ text "Nouveau site à évaluer" ]
              , input [ id "currentSite", type_ "url", value model.textCandidate, placeholder "https://www.esaip.org/", onInput InputText ] []
              , button (if model.isCandidateValidURL then [ onClick LaunchEvaluation ] else []) [ text "Go !" ]
              , if (Dict.isEmpty model.evaluations)
                then (div [] [])
                else ( div []
                  [ span [] [ text "ou reprenez une évaluation en cours : " ]
                  , label [ for "evaluatedSites" ] [ text "Sites déjà évalués" ]
                  , select [ id "evaluatedSites" ] (List.map (\url -> option [] [ text url ]) (Dict.keys model.evaluations))
                  ])
              ]
            ]
          Just site ->
            [ h1 [] [ text "Évaluation du site ", a [ href site ] [ text site ] ]
            , button [ onClick NukeSelectedSite ] [ text "Évaluer un autre site" ]
            , p [] [ text "Score de conformité : " ]
            , viewScore (model.referential.criteres
              |> Dict.keys
              |> List.map (statusFromId (getEvaluation model)))
            , div [] (List.indexedMap (categoryTable model) categories)
            --, button [ class "noprint", attribute "onclick" "window.print()" ] [ text "🖨 Imprimer ce rapport" ] -- TODO make a port to JS
            ]
        )

getEvaluation : Model -> Evaluation
getEvaluation model =
  case model.selectedSite of
    Just url -> Dict.get url model.evaluations |> Maybe.withDefault Dict.empty
    Nothing -> Dict.empty

viewScore : List CriterionStatus -> Html Msg
viewScore statuses =
  let
    nbTotal = statuses |> List.length
    notApplicable = statuses |> List.filter (\v -> v == NonApplicable) |> List.length
    conforme = statuses |> List.filter (\v -> v == Conforme) |> List.length
    score = 100.0 * toFloat conforme / (toFloat nbTotal - toFloat notApplicable) |> round |> String.fromInt |> (\s -> s++"%")
  in
    div [ class "score" ] [ div [class "progressbar" ] [ div [ class "progress", style "width" score ] [] ], text score ]

categoryTable : Model -> Int -> String -> Html Msg
categoryTable model index category =
  details [ attribute "open" "" ]
    [ catHeader model index category
    , table [] (tableHeader model.sortOrder :: (tableRows model (index+1)))
    ]

catHeader : Model -> Int -> String -> Html Msg
catHeader model index category =
  let
    statuses =
      model.referential.criteres
      |> Dict.filter (\k c -> c.category == index+1)
      |> Dict.keys
      |> List.map (statusFromId (getEvaluation model))
    progressBar = viewScore statuses
  in
    summary [ class "category-header" ]
    [ span [ class "category-title" ] [ text ("Catégorie " ++ (String.fromInt (index+1)) ++ " : " ++ category) ]
    , progressBar
    ]

statusFromId : Evaluation -> String -> CriterionStatus
statusFromId evaluation id = Dict.get id evaluation |> Maybe.withDefault AEvaluer

tableHeader : Order -> Html Msg
tableHeader order =
  thead []
    [ th [ onClick (SetOrder FunnyID) ] [ text "#" ]
    , th [] [ text "Critère" ]
    , th [ class "status", onClick (SetOrder ( if order == StatusInc then StatusDec else StatusInc )) ] [ text "Statut" ]
    ]

tableRows : Model -> Int -> List (Html Msg)
tableRows model cat =
  Dict.toList model.referential.criteres
  |> List.filter (\(_, c) -> c.category == cat)
  |> List.sortBy
    ( case model.sortOrder of
        FunnyID -> funnyID model
        StatusInc -> statusIncOrder model
        StatusDec -> statusDecOrder model
    )
  |> List.map ( viewCriterion (getEvaluation model) )

funnyID : Model -> (String, Criterion) -> Int
funnyID _ (_, c) = c.category * 100 + c.item

statusIncOrder : Model -> (String, Criterion) -> Int
statusIncOrder model (id, c) =
  let
    sRank = statusFromId (getEvaluation model) id |> statusRank
  in
    10000 * sRank + (funnyID model (id,c))

statusDecOrder : Model -> (String, Criterion) -> Int
statusDecOrder model (id, c) =
  let
    sRank = statusFromId (getEvaluation model) id |> statusRankInv
  in
    10000 * sRank + (funnyID model (id,c))

statusRank : CriterionStatus -> Int
statusRank s = case s of
  Conforme -> 1
  EnDeploiement -> 2
  NonConforme -> 3
  AEvaluer -> 4
  NonApplicable -> 5

statusRankInv : CriterionStatus -> Int
statusRankInv s = case s of
  NonConforme -> 1
  EnDeploiement -> 2
  Conforme -> 3
  AEvaluer -> 4
  NonApplicable -> 5

statusString : CriterionStatus -> String
statusString s = case s of
  AEvaluer -> "À évaluer 📝"
  NonConforme -> "Non conforme ❌"
  NonApplicable -> "Non applicable ☠️"
  Conforme -> "Conforme ✅"
  EnDeploiement -> "En déploiement 🏗️"

statusClass : CriterionStatus -> String
statusClass s = case s of
  AEvaluer -> "status-tbd"
  NonConforme -> "status-ko"
  NonApplicable -> "status-na"
  Conforme -> "status-ok"
  EnDeploiement -> "status-wip"

rotateStatus : CriterionStatus -> CriterionStatus
rotateStatus s = case s of
  AEvaluer -> Conforme
  Conforme -> NonConforme
  NonConforme -> EnDeploiement
  EnDeploiement -> NonApplicable
  NonApplicable -> AEvaluer

viewCriterion : Evaluation -> (String, Criterion) -> Html Msg
viewCriterion evaluation (id, c) =
  let
    status = statusFromId evaluation id
  in
    tr [ class (statusClass status)]
      [ td [] [ text id ]
      , td [ class "criteria-cell" ]
        [ details []
          [ summary [] [ text c.critere ]
          , h3 [ class "noprint" ] [ text "Objectif" ]
          , renderMarkdown c.objectif
          , h3 [ class "noprint" ] [ text "Mise en oeuvre" ]
          , renderMarkdown c.miseEnOeuvre
          , h3 [ class "noprint" ] [ text "Contrôle" ]
          , renderMarkdown c.controle
          , h3 [ class "noprint" ] [ text "En savoir plus" ]
          , a [ href c.url ] [ text c.url ]
          ]
        ]
      , td [ class "status", onClick (SetStatus (getID c) (rotateStatus status))] [ text (statusString status) ]
      ]


renderMarkdown : String -> Html Msg
renderMarkdown s =
      case s
        |> Markdown.parse
        |> Result.mapError (\_ -> "Erreur de décodage du markdown")
        |> Result.andThen (\ast -> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer ast)
      of
        Ok rendered -> div [ class "noprint" ] rendered
        Err errors -> div [ class "noprint" ] [ text s ]