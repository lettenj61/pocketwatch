module Pocketwatch.Iso8601 exposing
    ( dateOption, timeOption, options, format
    )

import Time
import Pocketwatch.Internal as Internal exposing (..)



format : Formatter String
format =
    formatToString options iso8601Parts


options : Options String
options =
    { date = dateOption
    , time = timeOption
    , segment = identity
    }


dateOption : DateOption String
dateOption =
    { year = String.fromInt
    , month = Internal.monthToInt >> Internal.padDigits 2
    , day = Internal.padDigits 2
    , weekday = always ""
    }


timeOption : TimeOption String
timeOption =
    { hour = Internal.padDigits 2
    , minute = Internal.padDigits 2
    , second = Internal.padDigits 2
    , millis = Internal.padDigits 3
    }


iso8601Parts : List Part
iso8601Parts =
    [ Year, Segment "-", Month, Segment "-", Day
    , Segment "T"
    , Hour, Segment ":", Minute, Segment ":", Second, Segment ".", Millis
    , Segment "Z"
    ]
