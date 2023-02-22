module Liturgie exposing 
  ( Model
  , LiturgieMsg(..)
  , Mode(..)
  , Date
  , initMaandkalender
  , initDagkalender
  , initMaandkalenderNL
  , initDagkalenderNL
  , update
  , getCommand
  , view
  , createDate
  , dateToString -- d-m-yyyy
  , dateToIsoString -- yyyy-mm-dd
  , stringToDate
  , dateToYearInt
  , dateToMonthYearString
  , dateToWeeknumber
  , isPast
  , liturgieMsgToString
  )

import Browser
import Date
import Dict exposing (Dict)
import Html exposing (Html, div, text, span, button, label, input, a)
import Html.Attributes exposing (id, class, title, disabled, checked, type_, target, href, classList)
import Html.Events exposing (onClick)
import Http
import List.Extra as Listx
import Time
import Task
import Json.Decode as Dec exposing (Decoder)


-- MODEL


type alias Model = 
  { today : Maybe Date.Date
  , date : Maybe Date.Date
  , selected : Maybe Date.Date
  , mode : Mode
  , calenderData : Calender
  , showKalender : Bool
  , filter : Filter
  , showSundaysOnly : Bool
  }


type Mode
  = YearMode
  | MonthMode
  | WeekMode
  | DayMode
  | ListMode


type Calender
  = Failure String
  | Loading Days
  | Success Days


type alias Days = 
  Dict String DayInfo


type alias DayInfo = 
  { weekTitle : String
  , weekISO : Int
  , weekISOCorrected : Int
  , weekDay : Int
  , weekDayCorrected : Int
  , yearABC : String
  , year12 : Int
  , season : Season
  , items : List Item 
  }


type Season
  = DoorHetJaar
  | Advent
  | Kerstnoveen
  | KersttijdVoorOpenbaring
  | KersttijdNaOpenbaring
  | Veertigdagentijd
  | Paastijd


type alias Item =
  { priority : Int
  , typeShort : String
  , titleLong : String
  , titleShort : String
  , titleCode : String
  , color : LiturgischeKleur
  , codeProper : String
  , codeCommon : String
  , codeDay : String
  , codeExtra : String
  }


type Priority
  = VrijeGedachtenis
  | Gedachtenis
  | Feest
  | Hoogfeest
  | DagKerstnoveen
  | KerstmisExtramissen
  | Paastriduum


type LiturgischeKleur
  = Groen
  | Rood
  | Wit
  | Paars
  | Roze


type alias Filter =
  { romeinseKalender : Bool
  , nederlandseKalender : Bool
  , vlaamseKalender : Bool
  }


type alias Date = Date.Date
  

initMaandkalender : Model
initMaandkalender = 
  { today = Nothing
  , date = Nothing
  , selected = Nothing
  , mode = MonthMode
  , calenderData = Loading Dict.empty
  , showKalender = True
  , filter = Filter True True True
  , showSundaysOnly = False
  }


initDagkalender : Model
initDagkalender = 
  { today = Nothing
  , date = Nothing
  , selected = Nothing
  , mode = DayMode
  , calenderData = Loading Dict.empty
  , showKalender = True
  , filter = Filter True True True
  , showSundaysOnly = False
  }


initMaandkalenderNL : Model
initMaandkalenderNL = 
  { today = Nothing
  , date = Nothing
  , selected = Nothing
  , mode = MonthMode
  , calenderData = Loading Dict.empty
  , showKalender = True
  , filter = Filter True True False
  , showSundaysOnly = False
  }


initDagkalenderNL : Model
initDagkalenderNL = 
  { today = Nothing
  , date = Nothing
  , selected = Nothing
  , mode = DayMode
  , calenderData = Loading Dict.empty
  , showKalender = True
  , filter = Filter True True False
  , showSundaysOnly = False
  }


-- HTTP & DECODING


minimumDate = Date.fromCalendarDate 1970 Time.Jan 1


resultToMsg : ( LiturgieMsg -> msg ) -> ( a -> LiturgieMsg ) -> Result Http.Error a -> msg
resultToMsg toParentMsg dataToMsg result =
  case result of 
    Ok a -> toParentMsg <| dataToMsg a
    Err e -> toParentMsg <| GotLiturgieHttpError e


getCalender : ( LiturgieMsg -> msg ) -> Mode -> Filter -> Maybe Date.Date -> Cmd msg
getCalender toParentMsg mode filter date = 
  case date of
    Nothing -> Cmd.none
    Just d -> 
      Http.get
        { url = "https://tiltenberg.org/kerkkalender/get_kerkkalender.php" ++ ( buildParameters mode filter d )
        , expect = Http.expectJson ( resultToMsg toParentMsg GotCalender ) calenderDecoder
        }


buildParameters : Mode -> Filter -> Date.Date -> String
buildParameters mode filter date =
  let
    startDate = 
      case mode of
        DayMode -> date
        WeekMode -> 
          let
            sd = Date.floor Date.Sunday date
          in
            case ( Date.compare sd minimumDate ) of
              LT -> minimumDate
              EQ -> sd
              GT -> sd
        MonthMode -> Date.floor Date.Month date
        YearMode -> Date.floor Date.Year date
        ListMode -> Date.floor Date.Year date

    start = 
      dateToIsoString startDate
      |> String.replace "-" ""

    endDate = 
      case mode of
        DayMode -> date
        WeekMode -> Date.ceiling Date.Saturday date
        MonthMode -> 
          Date.add Date.Days 1 date
          |> Date.ceiling Date.Month
          |> Date.add Date.Days -1
        YearMode -> 
          Date.add Date.Days 1 date
          |> Date.ceiling Date.Year
          |> Date.add Date.Days -1
        ListMode ->
          Date.add Date.Days 1 date
          |> Date.ceiling Date.Year
          |> Date.add Date.Days -1
    
    end = 
      dateToIsoString endDate
      |> String.replace "-" ""

    filterString = 
      if ( filter.vlaamseKalender == False ) then
        "&filter=1"
      else
        ""
  in
    "?start=" ++ start ++ "&end=" ++ end ++ filterString


andMap = Dec.map2 (|>)


calenderDecoder : Decoder Days
calenderDecoder = 
  Dec.keyValuePairs dayInfoDecoder
  |> Dec.map Dict.fromList


dayInfoDecoder : Decoder DayInfo
dayInfoDecoder = 
  Dec.succeed DayInfo
  |> andMap ( Dec.field "weekTitle" Dec.string )
  |> andMap ( Dec.field "weekISO" Dec.int )
  |> andMap ( Dec.field "weekISOCorrected" Dec.int )
  |> andMap ( Dec.field "weekDay" Dec.int )
  |> andMap ( Dec.field "weekDayCorrected" Dec.int )
  |> andMap ( Dec.field "yearABC" Dec.string )
  |> andMap ( Dec.field "year12" Dec.int )
  |> andMap ( Dec.field "season" ( Dec.string |> Dec.andThen seasonsDecoder ) )
  |> andMap ( Dec.field "items" ( Dec.list itemDecoder ) )


seasonsDecoder : String -> Decoder Season
seasonsDecoder seasonString =
  case seasonString of
    "d" -> Dec.succeed DoorHetJaar
    "a" -> Dec.succeed Advent
    "b" -> Dec.succeed Kerstnoveen
    "k" -> Dec.succeed KersttijdVoorOpenbaring
    "l" -> Dec.succeed KersttijdNaOpenbaring
    "v" -> Dec.succeed Veertigdagentijd
    "p" -> Dec.succeed Paastijd
    _ -> Dec.fail "Fout bij het decoden van de liturgische tijd!"


itemDecoder : Decoder Item
itemDecoder =
  Dec.succeed Item
  |> andMap ( Dec.field "priority" Dec.int )
  |> andMap ( Dec.field "type" Dec.string )
  |> andMap ( Dec.field "titleLong" Dec.string )
  |> andMap ( Dec.field "titleShort" Dec.string )
  |> andMap ( Dec.field "titleCode" Dec.string )
  |> andMap ( Dec.field "color" ( Dec.string |> Dec.andThen liturgischeKleurDecoder ) )
  |> andMap ( Dec.field "codeProper" Dec.string )
  |> andMap ( Dec.field "codeCommon" Dec.string )
  |> andMap ( Dec.field "codeDay" Dec.string )
  |> andMap ( Dec.field "codeExtra" Dec.string )


priorityDecoder : Int -> Decoder Priority
priorityDecoder priorityInt =
  case priorityInt of
    10 -> Dec.succeed Paastriduum
    19 -> Dec.succeed KerstmisExtramissen
    20 -> Dec.succeed Gedachtenis
    4 -> Dec.succeed VrijeGedachtenis
    5 -> Dec.succeed DagKerstnoveen
    _ -> Dec.fail ( "Fout bij het decoden van de priority: " ++ ( String.fromInt priorityInt ) ++ "!" )


liturgischeKleurDecoder : String -> Decoder LiturgischeKleur
liturgischeKleurDecoder kleurString =
  case kleurString of
    "g" -> Dec.succeed Groen
    "r" -> Dec.succeed Rood
    "p" -> Dec.succeed Paars
    "o" -> Dec.succeed Roze
    "w" -> Dec.succeed Wit
    _ -> Dec.fail ( "Fout bij het decoden van de liturgische kleur: " ++ kleurString ++ "!" )


stringToDate : String -> Date.Date
stringToDate string =
  case Date.fromIsoString string of
    Ok date -> date
    Err _ -> 
      Date.fromOrdinalDate 1984 1  -- Dit is natuurlijk onzin!


dateToWeekdayString : Date.Date -> String
dateToWeekdayString date = 
  case Date.weekday date of
    Time.Mon -> "maandag"
    Time.Tue -> "dinsdag"
    Time.Wed -> "woensdag"
    Time.Thu -> "donderdag"
    Time.Fri -> "vrijdag"
    Time.Sat -> "zaterdag"
    Time.Sun -> "zondag"


monthToFullString : Date.Month -> String
monthToFullString month =
  case month of
    Time.Jan -> "januari"
    Time.Feb -> "februari"
    Time.Mar -> "maart"
    Time.Apr -> "april"
    Time.May -> "mei"
    Time.Jun -> "juni"
    Time.Jul -> "juli"
    Time.Aug -> "augustus"
    Time.Sep -> "september"
    Time.Oct -> "oktober"
    Time.Nov -> "november"
    Time.Dec -> "december"


monthToShortString : Date.Month -> String
monthToShortString month =
  case month of
    Time.Jan -> "jan"
    Time.Feb -> "feb"
    Time.Mar -> "mrt"
    Time.Apr -> "apr"
    Time.May -> "mei"
    Time.Jun -> "jun"
    Time.Jul -> "jul"
    Time.Aug -> "aug"
    Time.Sep -> "sep"
    Time.Oct -> "okt"
    Time.Nov -> "nov"
    Time.Dec -> "dec"


seasonToString : Season -> String
seasonToString season =
  case season of
    DoorHetJaar -> "tijd door het jaar"
    Advent -> "advent"
    Kerstnoveen -> "advent (kerstnoveen)"
    KersttijdVoorOpenbaring -> "kersttijd (vóór Openbaring)"
    KersttijdNaOpenbaring -> "kersttijd (na Openbaring)"
    Veertigdagentijd -> "veertigdagentijd"
    Paastijd -> "paastijd"


priorityToString : Priority -> String
priorityToString priority = 
  case priority of
    VrijeGedachtenis -> "vrije gedachtenis"
    Gedachtenis -> "gedachtenis"
    Feest -> "feest"
    Hoogfeest -> "hoogfeest"
    DagKerstnoveen -> ""
    KerstmisExtramissen -> "hoogfeest"
    Paastriduum -> "paastriduum"


colorToString : LiturgischeKleur -> String
colorToString kleur =
  case kleur of 
    Groen -> "groen"
    Rood -> "rood"
    Wit -> "wit"
    Paars -> "paars"
    Roze -> "roze"


-- UPDATE


type LiturgieMsg
  = NoOp
  | GotToday Int Date.Month Int
  | GotCalender Days
  | GotLiturgieHttpError Http.Error
  | ChangeMode Mode
  | Next
  | Previous
  | ToToday
  | ToggleShowKalender
  | SetSelected ( Maybe Date.Date )
  | SetModelDate Date.Date
  | ClickDate Date.Date
  | ClickMonth ( Date.Month, Int )
  | ToggleVlaamseKalender
  | ToggleSundaysOnly
  | SoftReset


-- na het aanroepen van de update-functie, altijd controleren of er een command nodig is met getCommand.
update : ( LiturgieMsg -> msg ) -> LiturgieMsg -> Model -> Model
update toParentMsg msg model =
  case msg of
    NoOp ->
      model

    GotCalender calendar ->
      { model | calenderData = Success calendar }

    GotLiturgieHttpError error ->
      { model | calenderData = Failure ( httpError error ) }

    GotToday year month day ->
      let
        today = Date.fromCalendarDate year month day

        newDate = 
          case model.date of
            Nothing -> today
            Just d -> d

        newCalenderData = setLoadingCalenderData model.calenderData newDate

      in
        { model | date = Just newDate, today = Just today, calenderData = newCalenderData }

    ChangeMode newMode ->
      if model.mode == newMode then
        model
      else if ( model.mode == YearMode && newMode == ListMode ) || ( model.mode == ListMode && newMode == YearMode ) then
        { model | mode = newMode }
      else
        { model | mode = newMode, calenderData = Loading ( getCalenderLijst model.calenderData ) }

    Next ->
      { model | date = ( changeDate model 1 ), calenderData = Loading ( getCalenderLijst model.calenderData ) }

    Previous ->
      { model | date = ( changeDate model -1 ), calenderData = Loading ( getCalenderLijst model.calenderData ) }

    ToToday -> 
      { model | date = model.today, calenderData = Loading ( getCalenderLijst model.calenderData ) }

    ToggleShowKalender ->
      { model | showKalender = not model.showKalender }

    SetSelected mssDate ->
      { model | selected = mssDate }

    SetModelDate date -> 
      { model | date = Just date, calenderData = setLoadingCalenderData model.calenderData date }

    ClickDate date ->
      { model | selected = Just date, date = Just date }

    ClickMonth ( month, year ) ->
      { model 
      | selected = Just ( Date.fromCalendarDate year month 1 )
      , date = Just ( Date.fromCalendarDate year month 1 )
      }

    ToggleVlaamseKalender ->
      { model | filter = toggleVlaamseKalender model.filter, calenderData = Loading ( getCalenderLijst model.calenderData ) }

    ToggleSundaysOnly ->
      { model | showSundaysOnly = not model.showSundaysOnly }

    SoftReset ->
      -- gaat er vanuit dat today inmiddels bekend is
      { model 
      | date = model.today
      , mode = MonthMode
      , calenderData = Loading Dict.empty
      }


toggleVlaamseKalender : Filter -> Filter
toggleVlaamseKalender filter = 
  { filter | vlaamseKalender = ( not filter.vlaamseKalender ) }


setLoadingCalenderData : Calender -> Date.Date -> Calender
setLoadingCalenderData calender newDate =
  case calender of
    Loading dict -> Loading dict

    Success dict -> 
      case Dict.get ( dateToIsoString newDate ) dict of
        Nothing -> Loading dict
        Just d -> Success dict

    Failure _ -> Loading Dict.empty


getCommand : ( ( LiturgieMsg -> msg ), Model ) -> Cmd msg
getCommand ( toParentMsg, model ) =
  case model.calenderData of
    Success _ -> Cmd.none
    Failure _ -> Cmd.none
    Loading _ -> 
      getCalender toParentMsg model.mode model.filter model.date


getCalenderLijst : Calender -> Days
getCalenderLijst calenderData = 
  case calenderData of
    Failure _ -> Dict.empty
    Success lijst -> lijst
    Loading lijst -> lijst


-- onderstaande helpers worden vooral aangeroepen vanuit andere modules, die geen gebruik maken van Date.Date


createDate : Int -> Time.Month -> Int -> Date.Date
createDate year month day = 
  Date.fromCalendarDate year month day


dateToString : Date.Date -> String
dateToString date =
  Date.format "d-M-y" date


dateToIsoString : Date.Date -> String
dateToIsoString date = 
  Date.format "y-MM-dd" date


isPast : Date.Date -> Date.Date -> Bool
isPast date today = 
  case ( Date.compare date today ) of
    LT -> True
    EQ -> False
    GT -> False


dateToYearInt : Date.Date -> Int
dateToYearInt date =
  Date.year date


dateToMonthYearString : Date.Date -> String
dateToMonthYearString date =
  String.concat
    [ Date.month date |> monthToFullString
    , " "
    , Date.year date |> String.fromInt
    ]


dateToWeeknumber : Date.Date -> Int
dateToWeeknumber date =
  -- gecorrigeerde weeknummer: zondag is de eerste dag
  -- dus: het ISO-weeknummer van de volgende dag
  Date.add Date.Days 1 date
  |> Date.weekNumber


liturgieMsgToString : LiturgieMsg -> String
liturgieMsgToString msg =
  case msg of
    NoOp ->
      "NoOp"

    GotToday year month day ->
      "GotToday " ++ dateToString ( createDate year month day )

    GotCalender days ->
      "GotCalender"

    GotLiturgieHttpError error ->
      "GotLiturgieHttpError " ++ ( httpError error )

    ChangeMode mode ->
      "ChangeMode"

    Next -> 
      "Next"

    Previous ->
      "Previous"

    ToToday -> 
      "ToToday"

    ToggleShowKalender ->
      "ToggleShowKalender"

    SetSelected mssDate ->
      "SetSelected"

    SetModelDate date ->
      "SetModelDate " ++ ( dateToString date )

    ClickDate date ->
      "ClickDate " ++ ( dateToString date )

    ClickMonth ( maand, jaar ) ->
      "ClickMonth"

    ToggleVlaamseKalender ->
      "ToggleVlaamseKalender"

    ToggleSundaysOnly -> 
      "ToggleSundaysOnly"

    SoftReset ->
      "SoftReset"


--


changeDate : Model -> Int -> Maybe Date.Date
changeDate model direction =
  let
    unit = 
      case model.mode of
        YearMode -> Date.Years
        MonthMode -> Date.Months
        WeekMode -> Date.Weeks
        DayMode -> Date.Days
        ListMode -> Date.Years

  in
    case model.date of
      Nothing -> Nothing
      Just d -> 
        let
          newDate = 
            Date.add unit direction d

          newDateCorrected = 
            case ( Date.compare newDate minimumDate ) of
              LT -> minimumDate
              EQ -> newDate
              GT -> newDate

        in
          Just newDateCorrected


httpError : Http.Error -> String
httpError error =
  case error of
    Http.BadUrl string -> string
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "Netwerk fout"
    Http.BadStatus int -> "Foutcode" ++ ( String.fromInt int )
    Http.BadBody string -> string
    

-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions model =
  Sub.none


-- VIEW


view : ( LiturgieMsg -> msg ) -> Model -> Html msg
view toParentMsg model =
  case model.showKalender of
    True ->
      div 
        [ class "liturgie" ]
        [ viewControls toParentMsg model.date model.today
        , viewModeTabs toParentMsg model.mode
        , viewSettings toParentMsg model.filter model.mode model.showSundaysOnly
        , viewCalender toParentMsg model
        , viewVandaag model.today
        ]
    False ->
      div [] []


viewMaandJaar : ( LiturgieMsg -> msg ) -> Maybe Date.Date -> Html msg
viewMaandJaar toParentMsg modelDate =
  case modelDate of
    Nothing -> 
      div [ class "monthname" ] [ text "---" ]
    Just date -> 
      let
        maandjaar = 
          String.join " "
            [ ( monthToFullString ( Date.month date ) )
            , ( String.fromInt ( Date.year date ) )
            ]
      in
        div [ class "monthName" ] [ text maandjaar ]


viewDate : Maybe Date.Date -> Html msg
viewDate mssDate =
  case mssDate of
    Nothing -> div [] [ text "Geen modelDate" ]
    Just date ->
      div 
        [] 
        [ text ( dateToString date ) ]


viewVandaag : Maybe Date.Date -> Html msg
viewVandaag today =
  let
    htmlText = 
      case today of
        Nothing -> "Datum van vandaag nog onbekend. Even geduld."
        Just d -> 
          String.join " "
            [ "Vandaag is het "
            , String.fromInt ( Date.day d )
            , monthToFullString ( Date.month d )
            , String.fromInt ( Date.year d )
            ]
  in
    div [ class "vandaag" ] [ text htmlText ]


viewControls : ( LiturgieMsg -> msg ) -> Maybe Date.Date -> Maybe Date.Date -> Html msg
viewControls toParentMsg modelDate today =
  let
    isDisabled = 
      case modelDate of
        Nothing -> True
        Just d -> False

    ( todayIsDisabled, todayMsg ) = 
      case today of
        Nothing -> ( True, NoOp )
        Just td ->
          if isDisabled then
            ( True, NoOp )
          else
            ( False, SetModelDate td )

  in
    div 
      [ class "controls" ] 
      [ button [ disabled isDisabled, onClick ( toParentMsg Previous ) ] [ text "<<" ]
      --, button [ disabled isDisabled, onClick ( toParentMsg ToToday ) ] [ text "Vandaag" ]
      , button [ disabled todayIsDisabled, onClick ( toParentMsg todayMsg ) ] [ text "Vandaag" ]
      , button [ disabled isDisabled, onClick ( toParentMsg Next ) ] [ text ">>" ]
      ]


viewModeTabs : ( LiturgieMsg -> msg ) -> Mode -> Html msg
viewModeTabs toParentMsg mode =
  let
    className modelMode button = 
      if modelMode == button then
        "active"
      else
        ""

  in
    div
      [ class "tabs" ]
      [ button [ class ( className mode YearMode ), onClick ( toParentMsg ( ChangeMode YearMode ) ) ] [ text "Jaar" ]
      , button [ class ( className mode ListMode ), onClick ( toParentMsg ( ChangeMode ListMode ) ) ] [ text "Lijst" ]
      , button [ class ( className mode MonthMode ), onClick ( toParentMsg ( ChangeMode MonthMode ) ) ] [ text "Maand" ]
      , button [ class ( className mode WeekMode ), onClick ( toParentMsg ( ChangeMode WeekMode ) ) ] [ text "Week" ]
      , button [ class ( className mode DayMode ), onClick ( toParentMsg ( ChangeMode DayMode ) ) ] [ text "Dag" ]
      ]


viewSettings : ( LiturgieMsg -> msg ) -> Filter -> Mode -> Bool -> Html msg
viewSettings toParentMsg filter mode showSundaysOnly =
  let
    viewShowSundaysCheckbox = 
      case mode of
        ListMode ->
          [ viewCheckbox showSundaysOnly False ( toParentMsg ToggleSundaysOnly ) "Alleen zondagen" ]

        _ -> 
          []
  in
    div
      [ class "settings" ]
      ( List.concat
        [ [ viewCheckbox filter.vlaamseKalender False ( toParentMsg ToggleVlaamseKalender ) "Inclusief Vlaamse kalender"
          ]
        , viewShowSundaysCheckbox
        ]
      )


viewCalender : ( LiturgieMsg -> msg ) -> Model -> Html msg
viewCalender toParentMsg model =
  case model.calenderData of
    Failure error -> 
      div [ class "error" ] [ text ( "Fout bij het laden van de kalender. " ++ error ) ]
    Loading cal -> 
      if Dict.isEmpty cal then 
        div [ class "loading" ] [ text "Bezig met laden..." ]
      else
        div [ class "loading" ]
          [ viewCalenderData toParentMsg model cal ]

    Success cal -> 
      div [ class "success" ]
        [ viewCalenderData toParentMsg model cal ]


viewCalenderData : ( LiturgieMsg -> msg ) -> Model -> Days -> Html msg
viewCalenderData toParentMsg model cal =
  let
    aanwezigeDatums = 
      Dict.keys cal
      |> List.map stringToDate
      |> List.sortWith Date.compare

    aanwezigeWeeknummers = 
      --List.map ( \date -> ( Date.add Date.Days 1 date ) |> Date.weekNumber ) aanwezigeDatums
      List.map dateToWeeknumber aanwezigeDatums
      |> Listx.unique

    aanwezigeJaarMaanden = 
      List.map ( \date -> ( Date.year date, Date.month date ) ) aanwezigeDatums
      |> Listx.unique

  in
    case model.mode of
      YearMode -> 
        div 
        [ class "yearMode" ] 
        ( viewMonths toParentMsg model.today model.date model.selected model.mode model.showSundaysOnly cal )

      MonthMode -> 
        div
        [ class "monthMode" ]
        ( List.concat
          [ List.map ( viewMonthheader toParentMsg ) aanwezigeJaarMaanden
          , viewWeekheaders
          , List.indexedMap viewWeeknumbers aanwezigeWeeknummers
          , List.map ( viewDays toParentMsg model.today model.date model.selected cal ) aanwezigeDatums 
          ]
        )

      WeekMode -> 
        div 
        [ class "weekMode" ] 
        ( List.concat
          [ List.map ( viewDays toParentMsg model.today model.date model.selected cal ) aanwezigeDatums
          , viewWeekInfo ( Dict.values cal )
          ]
        )

      DayMode -> 
        div 
        [ class "dayMode" ] 
        ( List.concat
          [ List.map viewDateheader aanwezigeDatums
          , List.map ( viewDays toParentMsg model.today model.date model.selected cal ) aanwezigeDatums
          ]
        )

      ListMode -> 
        div 
        [ class "listMode" ]
        ( viewMonths toParentMsg model.today model.date model.selected model.mode model.showSundaysOnly cal )
        --( List.map ( viewDays toParentMsg model.today model.date model.selected cal ) aanwezigeDatums )


viewDateheader : Date.Date -> Html msg
viewDateheader date = 
  viewLabelTextDiv 
    "datum" 
    "Datum:" 
    ( String.join " " 
      [ dateToWeekdayString date
      , String.fromInt ( Date.day date )
      , monthToFullString ( Date.month date )
      , String.fromInt ( Date.year date )
      ]
    )


viewMonthheader : ( LiturgieMsg -> msg ) -> ( Int, Date.Month ) -> Html msg
viewMonthheader toParentMsg ( year, month ) =
  div
  [ class "monthName", onClick ( toParentMsg ( ClickMonth ( month, year ) ) ) ]
  [ span [ class "label" ] [ text "Maand:" ]
  , span [] [ text ( String.join " " [ ( monthToFullString month ), ( String.fromInt year ) ] ) ]
  ]


viewWeekheaders : List ( Html msg )
viewWeekheaders =
  let
    headers = 
      [ "nummer"
      , "zondag"
      , "maandag"
      , "dinsdag"
      , "woensdag"
      , "donderdag"
      , "vrijdag"
      , "zaterdag"
      ]
  in
    List.map viewWeekheader headers


viewWeekheader : String -> Html msg
viewWeekheader headerString = 
  div [ class ( "header " ++ headerString ) ] [ text headerString ]


viewWeeknumbers : Int -> Int -> Html msg
viewWeeknumbers index weeknumber =
  div [ class ( "weeknummer nr" ++ ( String.fromInt ( index + 1 ) ) ) ] [ text ( String.fromInt weeknumber ) ]


viewWeekInfo : List DayInfo -> List ( Html msg )
viewWeekInfo dayInfoList =
  let
    weektitel = 
      List.map .weekTitle dayInfoList
      |> Listx.unique
      |> String.join " & "
    
    weeknummer =
      List.map .weekISOCorrected dayInfoList
      |> Listx.unique
      |> List.map String.fromInt
      |> String.join "-"

    jaarABC = 
      List.map .yearABC dayInfoList
      |> Listx.unique
      |> List.map String.toUpper
      |> String.join "-"

    jaar12 = 
      List.map .year12 dayInfoList
      |> Listx.unique
      |> List.map String.fromInt
      |> String.join "-"

  in
    [ div
      [ class "weekinfo" ]
      [ div [] [ span [] [ text weektitel ] ]
      , viewLabelTextDiv "" "Weeknummer:" ( "week " ++ weeknummer )
      , viewLabelTextDiv "" "Driejarige cyclus:" ( "jaar " ++ jaarABC )
      , viewLabelTextDiv "" "Tweejarige cyclus:" ( "jaar " ++ jaar12 )
      ]
    ]


viewDays : ( LiturgieMsg -> msg ) -> Maybe Date.Date -> Maybe Date.Date -> Maybe Date.Date -> Days -> Date.Date -> Html msg
viewDays toParentMsg today modelDate selectedDate cal date = 
  let
    mssDayInfo = Dict.get ( dateToIsoString date ) cal
  in
    case mssDayInfo of
      Nothing -> div [] [ text "Datum niet gevonden!" ] -- kan niet voorkomen: code moet beter!
      Just dayInfo ->
        viewDay toParentMsg today modelDate selectedDate date dayInfo


viewDay : ( LiturgieMsg -> msg ) -> Maybe Date.Date -> Maybe Date.Date -> Maybe Date.Date -> Date.Date -> DayInfo -> Html msg
viewDay toParentMsg today modelDate selectedDate date dayInfo = 
  let
    modelDateClass = 
      if modelDate == Just date then 
        "modeldate"
      else 
        ""

    selectedClass = 
      if selectedDate == Just date then
        "selected"
      else
        ""

    timelineClass = 
      case today of
        Nothing -> ""
        Just td ->
          case ( Date.compare date td ) of
            LT -> "past"
            EQ -> "today"
            GT -> "future"

    positieMaandkalender =
      ( Date.day date ) + ( remainderBy 7 ( Date.weekdayNumber ( Date.floor Date.Month date ) ) )

    className = 
      List.singleton "day"
      |> List.append [ dateToWeekdayString date ]
      |> List.append [ "pos" ++ ( String.fromInt positieMaandkalender ) ]
      |> List.append [ modelDateClass ]
      |> List.append [ selectedClass ]
      |> List.append [ timelineClass ]
      |> List.filter ( \s -> s /= "" )
      |> String.join " "
      
  in
    div
      [ class className, onClick ( toParentMsg ( ClickDate date ) ) ]
      ( List.concat
        [
          [ viewLabelTextDiv "dayNumber" "Dagnummer:" ( String.fromInt ( Date.day date ) )
          , viewLabelTextDiv "weekTitle" "Week titel:" dayInfo.weekTitle
          , viewLabelTextDiv "weekISO" "ISO week:" ( String.fromInt dayInfo.weekISO )
          , viewLabelTextDiv "weekISOCorrected" "ISO week (gecorrigeerd):" ( String.fromInt dayInfo.weekISOCorrected )
          , viewLabelTextDiv "weekDay" "Weekdag (1-7):" ( String.fromInt dayInfo.weekDay )
          , viewLabelTextDiv "weekDayCorrected" "Weekdag (1-7) (gecorrigeerd):" ( String.fromInt dayInfo.weekDayCorrected )
          , viewLabelTextDiv "weekDayName" "Dag in de week:" ( dateToWeekdayString date )
          , viewLabelTextDiv "yearABC" "Driejarige cyclus:" ( "jaar " ++ ( String.toUpper dayInfo.yearABC ) )
          , viewLabelTextDiv "year12" "Tweejarige cyclus:" ( "jaar " ++ ( String.fromInt dayInfo.year12 ) )
          , viewLabelTextDiv "season" "Liturgische tijd:" ( seasonToString dayInfo.season )
          , a [ href ( buildMissaalUrl date ), target "_blank", class "missaalUrl" ] [ text "Teksten Eucharistie" ]
          ]
        , List.map ( viewItem date dayInfo.season dayInfo.weekTitle dayInfo.weekDayCorrected ) dayInfo.items
        ]
      )


viewItem : Date.Date -> Season -> String -> Int -> Item -> Html msg
viewItem date season weekTitle weekDayCorrected item = 
  let
    classNames = 
      [ ( "item " ++ ( colorToString item.color ), True )
      , ( "feest", ( item.typeShort == "f" ) )
      , ( "hoogfeest", ( item.typeShort == "h" ) )
      ]
    titleShort = 
      case item.titleShort of
        "" -> 
          --"Van de dag"
          --item.titleLong
          let
            weekTitles = String.split " / " weekTitle
            title = 
              if weekDayCorrected < 4 then
                List.head weekTitles
              else
                List.reverse weekTitles |> List.head
          in
            case title of
              Nothing -> "Van de dag"
              Just t -> t

        _ -> item.titleShort
  in
    div
      [ classList classNames, title item.titleLong ]
      [ viewLabelTextDiv "priority" "Prioriteit:" ( String.fromInt item.priority )
      , viewLabelTextDiv "typeShort" "Korte weergave type:" ( getTypeShort item.typeShort )
      , viewLabelTextDiv "typeLong" "Lange weergave type:" ( getTypeLong item.codeProper item.priority item.typeShort )
      , viewLabelTextDiv "titleLong" "Lange titel:" item.titleLong
      , viewLabelTextDiv "titleShort" "Korte titel:" titleShort
      , viewLabelTextDiv "titleCode" "Titelcode:" item.titleCode
      , viewLabelTextDiv "color" "Liturgische kleur:" ( colorToString item.color )
      , viewLabelTextDiv "codeProper" "Code eigen:" item.codeProper
      , viewLabelTextDiv "codeCommon" "Code gemeenschappelijk:" item.codeCommon
      , viewLabelTextDiv "codeDay" "Code van de dag:" item.codeDay
      , viewLabelTextDiv "codeExtra" "Code aanvullende psalmodie:" item.codeExtra
      , viewEucharistie date season item
      , viewGetijdengebed item
      --, a [ href ( buildMissaalUrl date ), target "_blank", class "missaalUrl" ] [ text "Teksten Eucharistie" ]
      ]


viewLabelTextDiv : String -> String -> String -> Html msg
viewLabelTextDiv className labelText textString = 
  div
  [ class className ]
  [ span [ class "label" ] [ text labelText ]
  , span [] [ text textString ]
  ]


viewEucharistie : Date.Date -> Season -> Item -> Html msg
viewEucharistie date season item =
  -- TODO: Chrismamis / Witte Donderdag en andere afwijkende dagen
  let
    beginGloria = "Het Eer aan God (Gloria) "
    welGloria = [ div [ class className ] [ text ( beginGloria ++ "wordt gebeden." ) ] ]
    nietGloria = [ div [ class className ] [ text ( beginGloria ++ "blijft achterwege." ) ] ]

    beginCredo = "De Geloofsbelijdenis (Credo) "
    welCredo = [ div [ class className ] [ text ( beginCredo ++ "wordt gebeden." ) ] ]
    nietCredo = [ div [ class className ] [ text ( beginCredo ++ "blijft achterwege." ) ] ]

    className = "gloria"

    -- Gloria:
    -- zondagen, behalve Advent en Veertigdagentijd
    -- feesten en hoogfeesten (behalve Palmzondag, maar die valt onder het eerste punt)
    -- Kerstoctaaf
    -- Witte Donderdag en Paasoctaaf

    -- Credo:
    -- idem als gloria, behalve: ook op zondagen in Advent en Veertigdagentijd

    ( gloria, credo ) = 
      -- zondagen
      if Date.weekday date == Time.Sun then
        case season of
          DoorHetJaar ->  ( welGloria, welCredo )
          Advent -> ( nietGloria, welCredo )
          Kerstnoveen -> ( nietGloria, welCredo )
          KersttijdVoorOpenbaring -> ( welGloria, welCredo )
          KersttijdNaOpenbaring -> ( welGloria, welCredo )
          Veertigdagentijd -> ( nietGloria, welCredo )
          Paastijd -> ( welGloria, welCredo )

      -- feesten en hoogfeesten
      else if List.member item.typeShort [ "f" ] then
        ( welGloria, [] )
      else if List.member item.typeShort [ "h" ] then
        ( welGloria, welCredo)

      -- Kerstoctaaf
      else if 
        Date.isBetween 
          ( Date.fromCalendarDate ( Date.year date ) Time.Dec 26 ) 
          ( Date.fromCalendarDate ( Date.year date ) Time.Dec 31 ) 
          date
      then
        ( welGloria, welCredo )

      -- Paasoctaaf ( Witte Donderdag moet apart geregeld worden )
      else if List.member item.codeProper [ "100", "101", "102", "103", "104", "105" ] then
        ( welGloria, welCredo )

      else
        ( [], [] )

    inhoud = List.concat [ gloria, credo ]
    
    kopje = 
      if inhoud == [] then
        []
      else
        [ div [ class "kopje" ] [ text "Eucharistieviering" ] ]
        
  in
    div
      [ class "eucharistie" ]
      ( List.concat [ kopje, inhoud ] )


viewGetijdengebed : Item -> Html msg
viewGetijdengebed item =
  let
    psalmboek = [ div [ class "psalmboek" ] [ text ( getPsalmboek item ) ] ]

    inhoud = List.concat [ psalmboek ]

    kopje = 
      if inhoud == [] then
        []
      else
        [ div [ class "kopje" ] [ text "Getijdengebed" ] ]
  in
    div
      [ class "getijdengebed" ]
      ( List.concat [ kopje, inhoud ] )


viewMonths : ( LiturgieMsg -> msg ) -> Maybe Date.Date -> Maybe Date.Date -> Maybe Date.Date -> Mode -> Bool -> Days -> List ( Html msg )
viewMonths toParentMsg today modelDate selectedDate mode showSundaysOnly cal =
  let
    months = List.range 1 12
  in
    List.map ( viewMonth toParentMsg today modelDate selectedDate mode showSundaysOnly cal ) months


viewMonth : ( LiturgieMsg -> msg ) -> Maybe Date.Date -> Maybe Date.Date -> Maybe Date.Date -> Mode -> Bool -> Days -> Int -> Html msg
viewMonth toParentMsg today modelDate selectedDate mode showSundaysOnly cal monthInt =
  let
    month = Date.numberToMonth monthInt

    maandCal = 
      Dict.filter ( \date info -> Date.month ( stringToDate date ) == month ) cal

    datumsInMaand = 
      Dict.keys maandCal
      |> List.map stringToDate
      |> List.sortWith Date.compare
      |> List.filter ( filterByShowSundaysOnly mode showSundaysOnly )

    aanwezigeWeeknummers = 
      datumsInMaand
      |> List.map dateToWeeknumber
      |> Listx.unique

    aanwezigeJaarMaanden = 
      datumsInMaand
      |> List.map ( \date -> ( Date.year date, Date.month date ) )
      |> Listx.unique

  in
    div
      [ class "month" ]
      ( List.concat
        [ List.map ( viewMonthheader toParentMsg ) aanwezigeJaarMaanden
        , viewWeekheaders
        , List.indexedMap viewWeeknumbers aanwezigeWeeknummers
        , List.map ( viewDays toParentMsg today modelDate selectedDate maandCal ) datumsInMaand
        ]
      )


filterByShowSundaysOnly : Mode -> Bool -> Date.Date -> Bool
filterByShowSundaysOnly mode showSundaysOnly date =
  case mode of
    ListMode ->
      if showSundaysOnly then
        if Date.weekday date == Time.Sun then
          True
        else
          False

      else
        True

    _ ->
      True


viewCheckbox : Bool -> Bool -> msg -> String -> Html msg
viewCheckbox isChecked isDisabled msg name =
  let
    labelClass = 
      case isDisabled of
        True -> "checkbox disabled"
        False -> "checkbox"
  in
    div [ class labelClass, onClick msg ]
      [ input [ type_ "checkbox", checked isChecked, disabled isDisabled ] []
      , label [] [ text name ]
      ]


buildMissaalUrl : Date.Date -> String
buildMissaalUrl date =
  "https://www.tiltenberg.org/missaal/0/?date=" ++ ( dateToIsoString date )


getTypeShort : String -> String
getTypeShort typeShort = 
  -- alleen om "a" om te zetten in ""
  case typeShort of
    "a" -> ""
    _ -> typeShort


getTypeLong : String -> Int -> String -> String
getTypeLong codeProper priority typeShort =
  {-
    Als typeShort niet leeg is:
      - a: noveen vóór Kerstmis           ""
      - f: feest                          "feest"
      - g: verplichte gedachtenis         "gedachtenis"
      - h: hoogfeest                      "hoogfeest"
      - anders: fout                      "fout"
    Anders:
      - kerstoctaaf: 26-12 t/m 31 december            "" -> niet behandelen
      - paasoctaaf: code eigen 100 t/m 105 (ma-za)    "" -> niet behandelen
      - Witte Donderdag: code eigen 598               "paastriduum" (zal nooit voorkomen, omdat het alleen in het missaal een rol speelt)
      - Goede Vrijdag: code eigen 097                 "paastriduum"
      - Stille zaterdag: code eigen 098               "paastriduum"
      - Vierde zondag van de Advent: code eigen 990   "" -> niet behandelen
      - vrije gedachtenis:                            "vrije gedachtenis"
                priority: 120, 90
      - Anders:
          ""
  -}
  case typeShort of 
    "a" -> ""
    "f" -> "feest"
    "g" -> "gedachtenis"
    "h" -> "hoogfeest"
    
    "" ->
      if List.member codeProper [ "598", "097", "098" ] then
        "paastriduum"
      else if List.member priority [ 90, 120, 85 ] then
        "vrije gedachtenis"
      else
        ""

    _ -> "fout"


getPsalmboek : Item -> String
getPsalmboek item =
  -- deze functie is nog niet af!
  let
    codeDayInt = String.toInt item.codeDay
    intro = "Psalmboek week "
  in
    case codeDayInt of

      Just day ->

        --ADVENT EN KERSTTIJD
        --a. eerste deel: code dag 001 t/m 020
        if day >= 1 && day <= 20 then
          intro
          ++ String.fromInt ( ( modBy 4 ( ( ( day - 1 ) // 7 ) ) ) + 1 )
  
        --b. vierde zondag van de advent: code dag 021
        else if day == 21 then
          intro
          ++ String.fromInt ( 4 )
    
        --c. tweede deel: code dag 159 t/m 187
        else if day >= 159 && day <= 187 then
          intro
          ++ String.fromInt ( ( modBy 4 ( ( ( ( day - 160 ) // 7 ) ) + 3 ) ) + 1 )
  
        --VEERTIGDAGENTIJD: 
        --a. Aswoensdag
        else if day == 645 then
          intro ++ "4 (in het morgengebed kan men de psalmen en antifonen van vrijdag van de derde week nemen)"

        --donderdag na aswoensdag t/m zaterdag voor palmzondag
        --code dag 054 t/m 091
        else if day >= 54 && day <= 91 then
          -- de extra + 4 is om -1 in de berekening te voorkomen
          intro
          ++ String.fromInt ( ( modBy 4 ( ( ( ( day - 57 ) // 7 ) ) + 4 ) ) + 1 )
    
        --TIJD DOOR HET JAAR: code dag 600 t/m 837
        else if day >= 600 && day <= 837 then
          intro
          ++ String.fromInt ( ( modBy 4 ( ( ( day - 600 ) // 7 ) ) ) + 1 )

        else
          "Psalmboek week onbekend"

      _ -> 
        "Psalmboek week onbekend"