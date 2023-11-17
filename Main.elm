port module Main exposing (main, printPage, storeData, exportData)

import Browser
import Dict
--import Html exposing Html, Attribute, div, input, text, table, tr, td, th, thead, summary, details)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode
import Json.Encode
import List.Extra
import Round
import Markdown.Parser as Markdown
import Markdown.Renderer
import Url

-- MAIN


main = Browser.element { init = init, update = update, view = view, subscriptions = (\_ -> Sub.none) }

port printPage : () -> Cmd msg

port storeData : Json.Encode.Value -> Cmd msg

port exportData : Json.Encode.Value -> Cmd msg

-- MODEL


type alias Model =
  {
    status : Status,
    referential : Referential,
    evaluations : Dict.Dict String Evaluation,
    textCandidate : String,
    isCandidateValidURL : Bool,
    selectedSite : Maybe String,
    sortOrder : Order,
    filter : Filter
  }

saveData : Dict.Dict String Evaluation -> Cmd msg
saveData evaluations = evaluations |> evaluationsEncoder |> storeData

pushData : Dict.Dict String Evaluation -> Cmd msg
pushData evaluations = evaluations |> evaluationsEncoder |> exportData

evaluationsEncoder : Dict.Dict String Evaluation -> Json.Encode.Value
evaluationsEncoder evaluations = Json.Encode.dict identity evaluationEncoder evaluations

evaluationEncoder : Evaluation -> Json.Encode.Value
evaluationEncoder evaluation = Json.Encode.dict identity criterionStatusEncoder evaluation

criterionStatusEncoder : CriterionStatus -> Json.Encode.Value
criterionStatusEncoder status = Json.Encode.string (statusCode status)

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

type Filter = All | OnlyConforme | OnlyNonApplicable | OnlyEnDeploiement

type alias FlagData =
  { referential : Referential
  , evaluations : Dict.Dict String Evaluation }

flagsDecoder: Json.Decode.Decoder FlagData
flagsDecoder = Json.Decode.map2 FlagData
  ( Json.Decode.field "ref" referentialDecoder)
  ( Json.Decode.field "eval" evaluationsDecoder)

evaluationsDecoder: Json.Decode.Decoder (Dict.Dict String Evaluation)
evaluationsDecoder = Json.Decode.dict evaluationDecoder

evaluationDecoder: Json.Decode.Decoder Evaluation
evaluationDecoder = Json.Decode.dict criterionStatusDecoder

criterionStatusDecoder: Json.Decode.Decoder CriterionStatus
criterionStatusDecoder = Json.Decode.string |> Json.Decode.andThen (\s -> Json.Decode.succeed (statusDecode s))

statusDecode : String -> CriterionStatus
statusDecode s = case s of
  "ko" -> NonConforme
  "na" -> NonApplicable
  "ok" -> Conforme
  "wip" -> EnDeploiement
  "tbd" -> AEvaluer
  _ -> AEvaluer

init: Json.Decode.Value -> (Model, Cmd Msg)
init jFlags =
    case Json.Decode.decodeValue flagsDecoder jFlags of

        Ok flags ->
          (
            { referential = flags.referential
            , status = OK
            , evaluations = flags.evaluations
            , selectedSite = Nothing
            , textCandidate = "https://www.esaip.org"
            , isCandidateValidURL = True
            , sortOrder = FunnyID
            , filter = All
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
            , filter = All
            }
          , Cmd.none
          )


-- UPDATE


type Msg =
  InputText String
  | LaunchEvaluation
  | Relaunch String
  | SetStatus String CriterionStatus
  | NukeSelectedSite
  | SetOrder Order
  | SetFilter Filter
  | PrintPage
  | ExportData


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =

  case msg of

    ExportData -> (model, pushData model.evaluations)

    PrintPage -> (model, printPage ())

    InputText text ->
      let
        valid = case (Url.fromString text) of
          Just url -> True
          Nothing -> False
      in
        ({ model | textCandidate = text, isCandidateValidURL = valid }, Cmd.none)

    LaunchEvaluation -> ({ model | selectedSite = Just model.textCandidate }, Cmd.none)

    Relaunch url -> ({ model | textCandidate = url, selectedSite = Just url }, Cmd.none)

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
            ( { model | evaluations = updatedEvaluations }, saveData updatedEvaluations )

    SetOrder order -> ({ model | sortOrder = order }, Cmd.none)

    SetFilter filter -> ({ model | filter = filter }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = case model.status of
  KO msg ->
    div [] [ text ( "Erreur : " ++ msg ) ]
  
  OK ->
    article []
      ( case model.selectedSite of
          Nothing ->
            [ h1 [] [ text "EcoChecker" ]
            , p [] [ text "Bienvenue sur EcoChecker, votre assistant pour évaluer l'éco-conception des sites web."]
            , p [] [ text "Le principe est simple : entrez l'URL du site que vous allez évaluer, puis vérifiez un à un les critères du ", a [ target "_blank", href "https://ecoresponsable.numerique.gouv.fr/publications/referentiel-general-ecoconception/" ] [ text "Référentiel général d'écoconception de services numériques (RGESN)"]]
            , aside [ id "launcher"]
              [ label [ for "currentSite" ] [ text "Site à évaluer" ]
              , input [ id "currentSite", type_ "url", value model.textCandidate, placeholder "https://www.esaip.org/", onInput InputText ] []
              , button (if model.isCandidateValidURL then [ onClick LaunchEvaluation ] else []) [ text "Go !" ]
              ]
            , if (Dict.isEmpty model.evaluations)
                then (div [] [])
                else ( div []
                  [ p [] [ text "ou reprenez une évaluation en cours : " ]
                  , section [] (List.map (\url -> aside [ onClick (Relaunch url) ] [ text url ]) (Dict.keys model.evaluations))
                  , p [] [ text "Vos données vous appartiennent ! EcoChecker le sait, et vous les tient à disposition :"]
                  , header [] [ button [class "light", onClick ExportData ] [ text "Télécharger mes données" ] ]
                  ]
                  )
            ]
          Just site ->
            [ nav [ id "topbar"]
              [ ul []
                [ li [] [ a [ target "_blank", href site ] [ text site ] ]
                , li [ class "noprint"] [ button [ class "light", onClick NukeSelectedSite ] [ text "Changer de site" ] ]
                ]
              ]
            , article []
              [ h1 [] [ text "Évaluation du site " ]
              , scoreCard model
              , header [] [ button [ class "noprint light", onClick PrintPage ] [ text "🖨 Exporter le bilan détaillé en PDF" ] ]
              , nav [id "filterbar", class "noprint"]
                [ ul []
                  [ li [] [text "Afficher :"]
                  , li []
                    [ button 
                      [id "filter-all"
                      , class (if model.filter == All then "filter-button active" else "filter-button inactive")
                      , onClick (SetFilter All)]
                      [ text "Tous les critères" ] ]
                  , li [] 
                    [ button 
                      [id "filter-ok"
                      , class (if model.filter == OnlyConforme then "filter-button active" else "filter-button inactive")
                      , onClick (SetFilter OnlyConforme)]
                      [ text "Les conformes" ] ]
                  , li [] 
                    [ button 
                      [id "filter-wip"
                      , class (if model.filter == OnlyEnDeploiement then "filter-button active" else "filter-button inactive")
                      , onClick (SetFilter OnlyEnDeploiement)]
                      [ text "Ceux en déploiement" ] ]
                  , li [] 
                    [ button 
                      [id "filter-na"
                      , class (if model.filter == OnlyNonApplicable then "filter-button active" else "filter-button inactive")
                      , onClick (SetFilter OnlyNonApplicable)]
                      [ text "Les non-applicables" ] ]
                  ]
                ]
              , div [] (List.indexedMap (categoryTable model) categories)
              ]
            ]
        )

getEvaluation : Model -> Evaluation
getEvaluation model =
  case model.selectedSite of
    Just url -> Dict.get url model.evaluations |> Maybe.withDefault Dict.empty
    Nothing -> Dict.empty

getScore : List CriterionStatus -> Maybe String
getScore statuses =
  let
    nbTotal = statuses |> List.length
    notApplicable = statuses |> List.filter (\v -> v == NonApplicable) |> List.length
    conforme = statuses |> List.filter (\v -> v == Conforme) |> List.length
  in
    if (notApplicable == nbTotal) then Nothing else
      Just (100.0 * toFloat conforme / (toFloat nbTotal - toFloat notApplicable) |> round |> String.fromInt |> (\s -> s++"%"))

viewScore : List CriterionStatus -> Html Msg
viewScore statuses =
  case getScore statuses of
    Just score -> div [ class "score" ] [ div [class "progressbar" ] [ div [ class "progress", style "width" score ] [] ], span [class "percents"] [text score] ]
    Nothing -> div [] [ text "Pas de critère applicable"]

scoreCard : Model -> Html Msg
scoreCard model =
  case getScore (model.referential.criteres |> Dict.keys |> List.map (statusFromId (getEvaluation model))) of
    Just score -> 
      aside []
      [
        details [ id "scorecard-details"]
        [ summary [ id "scorecard" ]
            [ span [] [ text "Score de conformité : " ]
            , div [ id "main-progressbar" ] [ div [ id "main-progress", style "width" score ] [] ]
            , span [class "percents"] [text score]
            ]
        , div [] (List.indexedMap (catHeader model) categories)
        ]
      ]
    Nothing -> div [] [ text "Pas de critère applicable"]

categoryTable : Model -> Int -> String -> Html Msg
categoryTable model index category =
  details [ class "category", attribute "open" "" ]
    [ catHeader model index category
    , if (getRowsForCat model (index+1) |> List.isEmpty)
      then p [] [ text "Aucun critère correspondant dans cette catégorie"]
      else table [] (tableHeader model.sortOrder :: (tableRows model (index+1)))
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
    [ th [ onClick (SetOrder FunnyID) ] [ text (if order == FunnyID then "# ↓" else "#") ]
    , th [ ] [ text "Critère" ]
    , th
      [ class "status"
      , onClick (SetOrder ( if order == StatusInc then StatusDec else StatusInc ))
      ]
      [ text (if order == StatusInc then "Statut ↓" else if order == StatusDec then "Statut ↑" else "Statut") ]
    ]

getRowsForCat : Model -> Int -> List (String, Criterion)
getRowsForCat model cat =
  Dict.toList model.referential.criteres
  |> List.filter (\(_, c) -> c.category == cat)
  |> List.filter (generalFilter model)

tableRows : Model -> Int -> List (Html Msg)
tableRows model cat =
  getRowsForCat model cat
  |> List.sortBy
    ( case model.sortOrder of
        FunnyID -> funnyID model
        StatusInc -> statusIncOrder model
        StatusDec -> statusDecOrder model
    )
  |> List.map ( viewCriterion (getEvaluation model) )

generalFilter : Model -> (String, Criterion) -> Bool
generalFilter model (id, _) =
  case model.filter of
    All -> True
    OnlyConforme -> (statusFromId (getEvaluation model) id) == Conforme
    OnlyNonApplicable -> (statusFromId (getEvaluation model) id) == NonApplicable
    OnlyEnDeploiement -> (statusFromId (getEvaluation model) id) == EnDeploiement

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
  NonConforme -> "Non conforme 😵"
  NonApplicable -> "Non applicable ☁️"
  Conforme -> "Conforme 🔥"
  EnDeploiement -> "En déploiement 🚀"

statusCode : CriterionStatus -> String
statusCode s = case s of
  AEvaluer -> "tbd"
  NonConforme -> "ko"
  NonApplicable -> "na"
  Conforme -> "ok"
  EnDeploiement -> "wip"

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
    tr [ class ("criterion "++(statusClass status)) ]
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
      , td [] [ button [ class ("status " ++ (statusClass status)), onClick (SetStatus (getID c) (rotateStatus status))] [ text (statusString status) ]]
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