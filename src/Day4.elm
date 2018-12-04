module Day4 exposing (puzzle1, puzzle2)

import Dict exposing (Dict)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import SantasLittleHelper exposing (..)


puzzle1 : String -> String
puzzle1 input =
    Parser.run (parseList logParser) input
        |> Result.toMaybe
        |> Maybe.andThen (getGuardSleepTime >> getSleepiestGuardAndTheirSnooziestMinute)
        |> Debug.log "b"
        |> Maybe.map (\a -> a.guardId * a.snooziestMinute |> String.fromInt)
        |> Maybe.withDefault ""


sortLogs : List Log -> List Log
sortLogs logs =
    List.sortBy
        (\a ->
            let
                dt =
                    a.dateTime
            in
            dt.month * 100000 + dt.day * minutesInDay + dt.time
        )
        logs


minutesInDay =
    24 * 60


getGuardSleepTime : List Log -> List Guard
getGuardSleepTime logs =
    logs
        |> sortLogs
        |> List.foldl
            updateGuards
            { currentGuard = Nothing, guards = [] }
        |> .guards


type alias GuardState =
    { currentGuard : Maybe Int, guards : List Guard }


type alias SleepSpan =
    { fellAsleepAt : Time, duration : Time }


type alias Guard =
    { id : Int, sleepSchedule : List SleepSpan }


type alias Time =
    Int


minutePart : Time -> Int
minutePart time =
    Basics.modBy time 60


minuteInSleepSpan : Int -> SleepSpan -> Bool
minuteInSleepSpan minute sleepSpan =
    let
        fellAsleepAt =
            sleepSpan.fellAsleepAt

        wokeUpAt =
            sleepSpan.fellAsleepAt + sleepSpan.duration
    in
    minute >= fellAsleepAt && minute < wokeUpAt


snooziestMinute : List SleepSpan -> Int
snooziestMinute sleepSpan =
    List.range 0 60
        |> List.maximumBy (\a -> List.count (minuteInSleepSpan a) sleepSpan)
        |> Maybe.withDefault 0


getSleepiestGuardAndTheirSnooziestMinute : List Guard -> Maybe { guardId : Int, snooziestMinute : Time }
getSleepiestGuardAndTheirSnooziestMinute guards =
    guards
        |> List.maximumBy guardTotalSleep
        |> Maybe.map (\guard -> { guardId = guard.id, snooziestMinute = snooziestMinute guard.sleepSchedule })


guardTotalSleep : Guard -> Time
guardTotalSleep guard =
    guard.sleepSchedule |> List.map .duration |> List.sum


updateGuards : Log -> GuardState -> GuardState
updateGuards newLog state =
    let
        updateGuard : (Guard -> Guard) -> GuardState
        updateGuard updateFunc =
            { state
                | guards =
                    state.guards
                        |> List.updateIf (\a -> Just a.id == state.currentGuard) updateFunc
            }

        wakesUpCase : GuardState
        wakesUpCase =
            updateGuard
                (\guard ->
                    { guard
                        | sleepSchedule =
                            guard.sleepSchedule
                                |> List.updateAt 0 (\a -> { a | duration = timeDifference newLog.dateTime.time a.fellAsleepAt })
                    }
                )
    in
    case newLog.status of
        BeginsShift id ->
            let
                newGuard =
                    Guard id []

                -- Account for how much time the previous guard slept if they don't wake up before the next shift.
                -- newState =
                --     wakesUpCase
            in
            { currentGuard = Just id, guards = newGuard :: state.guards }

        FellAsleep ->
            updateGuard (\a -> { a | sleepSchedule = SleepSpan newLog.dateTime.time 0 :: a.sleepSchedule })

        WakesUp ->
            wakesUpCase


timeDifference : Time -> Time -> Time
timeDifference future past =
    if future < past then
        future + minutesInDay - past

    else
        future - past


type alias DateTime =
    { month : Int
    , day : Int
    , time : Time --Minutes since the start of the day
    }


type Status
    = BeginsShift Int
    | FellAsleep
    | WakesUp


type alias Log =
    { dateTime : DateTime
    , status : Status
    }


dateParser : Parser DateTime
dateParser =
    Parser.succeed (\a b c d -> DateTime a b (c * 60 + d))
        |. Parser.symbol "["
        |. leadingZeroesIntParser
        |. Parser.symbol "-"
        |= leadingZeroesIntParser
        |. Parser.symbol "-"
        |= leadingZeroesIntParser
        |. Parser.spaces
        |= leadingZeroesIntParser
        |. Parser.symbol ":"
        |= leadingZeroesIntParser
        |. Parser.symbol "]"


leadingZeroesIntParser : Parser Int
leadingZeroesIntParser =
    Parser.chompWhile Char.isDigit |> Parser.getChompedString |> Parser.map (String.toInt >> Maybe.withDefault 0)


statusParser : Parser Status
statusParser =
    Parser.oneOf
        [ Parser.succeed BeginsShift
            |. Parser.symbol "Guard #"
            |= leadingZeroesIntParser
            |. Parser.symbol " begins shift"
        , Parser.succeed FellAsleep
            |. Parser.symbol "falls asleep"
        , Parser.succeed WakesUp
            |. Parser.symbol "wakes up"
        ]


logParser : Parser Log
logParser =
    Parser.succeed Log
        |= dateParser
        |. Parser.spaces
        |= statusParser


puzzle2 : String -> String
puzzle2 input =
    input
