module Pocketwatch.Internal
    exposing
        ( Props, fromPosix
        , defaultMultiplier
        , daysInMonth, isLeapYear, monthToInt, padDigits
        , Formatter, Options, Part(..), DateOption, TimeOption
        , write, writeHelp, formatToString
        )

import Time



-- DATE/TIME PROPERTY


type Props =
    Props
        { year : Int
        , month : Time.Month
        , day : Int
        , weekday : Time.Weekday
        , hour : Int
        , minute : Int
        , second : Int
        , millis : Int
        , posix_ : Time.Posix
        }


fromPosix : Time.Zone -> Time.Posix -> Props
fromPosix zone posix =
    Props
        { year = Time.toYear zone posix
        , month = Time.toMonth zone posix
        , day = Time.toDay zone posix
        , weekday = Time.toWeekday zone posix
        , hour = Time.toHour zone posix
        , minute = Time.toMinute zone posix
        , second = Time.toSecond zone posix
        , millis = Time.toMillis zone posix
        , posix_ = posix
        }


toPosix : Props -> Time.Posix
toPosix (Props { posix_ }) =
    posix_


type alias Multiplier =
    { second : Int
    , minute : Int
    , hour : Int
    , day : Int
    , year : Int
    }


defaultMultiplier : Multiplier
defaultMultiplier =
    { second    = 1000
    , minute    = 60000         -- 60 * 1000
    , hour      = 3600000       -- 60 * 60 * 1000
    , day       = 86400000      -- 24 * 60 * 60 * 1000
    , year      = 31536000000   -- 365 * 24 * 60 * 60 * 1000
    }


epochYear : Int
epochYear =
    1970



-- FORMATTER


formatToString : Options String -> List Part -> Formatter String
formatToString options parts =
    \zone posix ->
        String.join
            ""
            (write (fromPosix zone posix) options parts)


write : Props -> Options s -> List Part -> List s
write props render parts =
    List.reverse (writeHelp props render parts [])


writeHelp : Props -> Options s -> List Part -> List s -> List s
writeHelp (Props props) render parts buffer =
    case parts of
        [] ->
            buffer

        part :: rest ->
            let
                value =
                    case part of
                        Year ->
                            render.date.year props.year

                        Month ->
                            render.date.month props.month

                        Day ->
                            render.date.day props.day

                        Weekday ->
                            render.date.weekday props.weekday

                        Hour ->
                            render.time.hour props.hour

                        Minute ->
                            render.time.minute props.minute

                        Second ->
                            render.time.second props.second

                        Millis ->
                            render.time.millis props.millis

                        Segment s ->
                            render.segment s
            in
            writeHelp
                (Props props)
                render
                rest
                (value :: buffer)


type alias Formatter s =
    Time.Zone -> Time.Posix -> s


type Part
    = Year
    | Month
    | Day
    | Weekday
    | Hour
    | Minute
    | Second
    | Millis
    | Segment String


type alias Options s =
    { date : DateOption s
    , time : TimeOption s
    , segment : String -> s
    }


type alias DateOption s =
    { year : Int -> s
    , month : Time.Month -> s
    , day : Int -> s
    , weekday : Time.Weekday -> s
    }


type alias TimeOption s =
    { hour : Int -> s
    , minute : Int -> s
    , second : Int -> s
    , millis : Int -> s
    }



-- UTILITY


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


daysInMonth : Int -> Time.Month -> Int
daysInMonth year month =
    case month of
        Time.Jan ->
            31

        Time.Feb ->
            if isLeapYear year then
                29
            else
                28

        Time.Mar ->
            31

        Time.Apr ->
            30

        Time.May ->
            31

        Time.Jun ->
            30

        Time.Jul ->
            31

        Time.Aug ->
            31

        Time.Sep ->
            30

        Time.Oct ->
            31

        Time.Nov ->
            30

        Time.Dec ->
            31


isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 4 year == 0) && ((modBy 100 year /= 0) || (modBy 400 year == 0))


leapYearsBefore : Int -> Int
leapYearsBefore y1 =
    let
        y =
            y1 - 1
    in
    (y // 4) - (y // 100) + (y // 400)


padDigits : Int -> Int -> String
padDigits =
    padDigitsHelp String.fromInt


padDigitsHelp : (m -> String) -> Int -> m -> String
padDigitsHelp writer n value =
    String.padLeft
        n
        '0'
        (writer value)
