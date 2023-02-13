module Main exposing (..)

import Browser exposing ( Document, UrlRequest(..) )
import Browser.Navigation as Nav
import DateTime exposing ( DateTime )
import Dict
import Html exposing ( Html, div, text )
import Html.Attributes exposing ( id )
import Http
import Liturgie
import Task
import Time
import Url exposing ( Url )
import Url.Builder as UrlBuilder
import Url.Parser as UrlParser exposing ( (</>) )


-- MAIN


main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlRequest = ClickLink -- UrlRequest -> msg
    , onUrlChange = ChangeUrl -- Url -> msg
    }


-- MODEL


type alias Model =
  { nu : Maybe DateTime
  , liturgischeKalender : Liturgie.Model
  , navKey : Nav.Key
  , route : Maybe Kruimelpad
  , log : List String
  }


type alias Kruimelpad =
  ( Liturgie.Mode, Liturgie.Date )


kruimelParser : UrlParser.Parser ( Kruimelpad -> a ) a
kruimelParser =
  UrlParser.s "kerkkalender" </> 
    ( UrlParser.map 
        Tuple.pair 
        ( UrlParser.map stringToMode ( UrlParser.string ) 
          </> 
          UrlParser.map Liturgie.stringToDate ( UrlParser.string ) 
        )
    )


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
  let
    newLiturgischeKalender = Liturgie.initMaandkalender |> initUrl ( UrlParser.parse kruimelParser url )

  in
    ( { nu = Nothing
      , liturgischeKalender = newLiturgischeKalender
      , navKey = navKey
      , route = UrlParser.parse kruimelParser url
      , log = [ String.join " " [ "init:", buildLogString newLiturgischeKalender ] ]
      }
    , Cmd.batch 
      [ Task.perform GotNow Time.now
      , Liturgie.getCommand ( KalenderMsg, newLiturgischeKalender )
      ]
    )


initUrl : Maybe Kruimelpad -> Liturgie.Model -> Liturgie.Model
initUrl kruimelpad liturgischeKalender =
  case kruimelpad of
    Nothing -> liturgischeKalender

    Just ( mode, date ) ->
      Liturgie.update KalenderMsg ( Liturgie.ChangeMode mode ) liturgischeKalender
      |> Liturgie.update KalenderMsg ( Liturgie.SetModelDate date )


-- UPDATE


type Msg
  = NoOp
  | GotNow Time.Posix
  | KalenderMsg Liturgie.LiturgieMsg
  | ChangeUrl Url
  | ClickLink UrlRequest


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    ChangeUrl url ->
      let
        newRoute = UrlParser.parse kruimelParser url

        newLiturgischeKalender = 
          case newRoute of
          Nothing -> 
            -- 'resetten' naar beginwaarden
            Liturgie.update KalenderMsg Liturgie.SoftReset model.liturgischeKalender
            
          Just r ->
            Liturgie.update KalenderMsg ( Liturgie.ChangeMode ( Tuple.first r ) ) model.liturgischeKalender
            |> Liturgie.update KalenderMsg ( Liturgie.SetModelDate ( Tuple.second r ) )

      in
        ( { model 
          | route = newRoute
          , liturgischeKalender = newLiturgischeKalender
          , log = ( "ChangeUrl " ++ ( Url.toString url ) ++ " " ++ ( buildLogString newLiturgischeKalender ) ) :: model.log
          }
        , Liturgie.getCommand ( KalenderMsg, newLiturgischeKalender )
        )

    ClickLink urlRequest ->
      case urlRequest of
        Internal url ->
          ( { model | log = "ClickLink Internal" :: model.log }
          , Nav.pushUrl model.navKey <| Url.toString url )

        External url ->
          ( { model | log = "ClickLink External" :: model.log }
          , Nav.load url )

    GotNow nuPosix ->
      let
        nu = DateTime.fromPosix nuPosix

        newLiturgischeKalender = 
          Liturgie.update 
            KalenderMsg
            ( Liturgie.GotToday 
                ( DateTime.getYear nu )
                ( DateTime.getMonth nu )
                ( DateTime.getDay nu )
            )
            model.liturgischeKalender

      in
        ( { model 
          | nu = Just nu 
          , liturgischeKalender = newLiturgischeKalender
          , log = ( "GotNow " ++ ( buildLogString newLiturgischeKalender ) ) :: model.log
          }
        , Liturgie.getCommand ( KalenderMsg, newLiturgischeKalender )
        )

    KalenderMsg subMsg ->
      let
        newLiturgischeKalender = 
          case subMsg of
            Liturgie.ClickDate date -> 
              Liturgie.update KalenderMsg subMsg model.liturgischeKalender
              |> Liturgie.update KalenderMsg ( Liturgie.ChangeMode Liturgie.DayMode )

            Liturgie.ClickMonth ( month, year ) ->
              Liturgie.update KalenderMsg subMsg model.liturgischeKalender
              |> Liturgie.update KalenderMsg ( Liturgie.ChangeMode Liturgie.MonthMode )

            _ ->
              Liturgie.update KalenderMsg subMsg model.liturgischeKalender

        pushUrlCommand = 
          -- alleen een nieuwe url pushen als mode en date veranderd zijn:
          if model.liturgischeKalender.mode /= newLiturgischeKalender.mode 
            || model.liturgischeKalender.date /= newLiturgischeKalender.date 
            --|| model.route == Nothing
          then
            Nav.pushUrl model.navKey ( buildNavUrl newLiturgischeKalender.mode newLiturgischeKalender.date )
          else
            Cmd.none

      in
        ( { model 
          | liturgischeKalender = newLiturgischeKalender 
          , log = ( "KalenderMsg " ++ ( Liturgie.liturgieMsgToString subMsg ) ++ " " ++ ( buildLogString newLiturgischeKalender ) ) :: model.log
          }
        , Cmd.batch
          [ Liturgie.getCommand ( KalenderMsg, newLiturgischeKalender )
          , pushUrlCommand
          ]
        )


buildNavUrl : Liturgie.Mode -> Maybe Liturgie.Date -> String
buildNavUrl mode date = 
  let
    modus = 
      case mode of
        Liturgie.YearMode -> "jaar"
        Liturgie.ListMode -> "lijst"
        Liturgie.MonthMode -> "maand"
        Liturgie.WeekMode -> "week"
        Liturgie.DayMode -> "dag"
    
    datum = 
      case date of
        Nothing -> "vandaag"
        Just d -> 
          Liturgie.dateToIsoString d

  in
    UrlBuilder.absolute [ "kerkkalender", modus, datum ] []


buildLogString : Liturgie.Model -> String
buildLogString liturgischeKalender =
  let
    datum = 
      case liturgischeKalender.date of
        Nothing -> "zonder datum"
        Just d -> Liturgie.dateToIsoString d

    modus = 
      modeToString liturgischeKalender.mode

  in
    String.join " " [ datum, modus ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Document Msg
view model =
  { title = "Kerkkalender" ++ ( liturgieModelToHistoryString model.liturgischeKalender )
  , body = 
    [ div
      [ id "kalender" ]
      [ Liturgie.view KalenderMsg model.liturgischeKalender ]
    --, viewKruimelpad model.route
    --, viewLog model.log
    ]
  }


viewKruimelpad : Maybe Kruimelpad -> Html Msg
viewKruimelpad mssKruimelpad =
  case mssKruimelpad of
    Nothing -> div [ id "kruimelpad" ] [ text "Geen kruimelpad" ]
    Just kruimelpad ->
      div
        [ id "kruimelpad" ]
        [ text ( String.join " " 
                  [ modeToString ( Tuple.first kruimelpad )
                  , ">"
                  , Tuple.second kruimelpad |> Liturgie.dateToIsoString
                  ]
                )
        ]


viewLog : List String -> Html Msg
viewLog log = 
  let
    viewItem string = 
      div [] [ text string ]
  in
    div
      []
      ( List.map viewItem log )


liturgieModelToHistoryString : Liturgie.Model -> String
liturgieModelToHistoryString liturgischeKalender = 
  case liturgischeKalender.date of
    Nothing -> 
      ""

    Just date ->
      case liturgischeKalender.mode of
        
        Liturgie.YearMode -> 
          String.concat
            [ ": "
            , Liturgie.dateToYearInt date |> String.fromInt
            ]

        Liturgie.ListMode -> 
          String.concat
            [ ": "
            , Liturgie.dateToYearInt date |> String.fromInt
            , " (lijst)"
            ]

        Liturgie.MonthMode -> 
          String.concat
            [ ": "
            , Liturgie.dateToMonthYearString date
            ]

        Liturgie.WeekMode -> 
          String.concat
            [ ": week "
            , Liturgie.dateToWeeknumber date |> String.fromInt
            , " ("
            , Liturgie.dateToYearInt date |> String.fromInt
            , ")"
            ]

        Liturgie.DayMode -> 
          String.concat
            [ ": "
            , Liturgie.dateToString date
            ]


modeToString : Liturgie.Mode -> String
modeToString mode = 
  case mode of
    Liturgie.YearMode -> "jaarweergave"
    Liturgie.ListMode -> "lijstweergave"
    Liturgie.MonthMode -> "maandweergave"
    Liturgie.WeekMode -> "weekweergave"
    Liturgie.DayMode -> "dagweergave"


stringToMode : String -> Liturgie.Mode
stringToMode string =
  case string of
    "jaar" -> Liturgie.YearMode
    "lijst" -> Liturgie.ListMode
    "maand" -> Liturgie.MonthMode
    "week" -> Liturgie.WeekMode
    "dag" -> Liturgie.DayMode
    _ -> Liturgie.MonthMode


