module Toml exposing
    ( parse
    , Error(..)
    , Key, Toml, Value(..)
    )

{-| Parse a [TOML](https://toml.io/en/v1.0.0) file to a queryable Elm structure.

@docs BetterToml
@docs ParsedKey, ParsedValue

@docs parse

@docs Error

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Parser.Advanced exposing ((|.), (|=))
import Parser.Advanced.Workaround
import Rfc3339
import Set exposing (Set)
import Time
import Util.List
import Util.Result


{-| -}
type alias Toml =
    Dict Key Value


type alias Key =
    String


type alias ParsedToml =
    List Expression


{-| -}
type Value
    = String String
    | Integer Int
    | Float Float
    | Boolean Bool
    | DateTime Rfc3339.DateTime
    | Array (Array Value)
    | Table (Dict Key Value)


type ParsedValue
    = PVString String
    | PVInteger Int
    | PVFloat Float
    | PVBoolean Bool
    | PVDateTime Rfc3339.DateTime
    | PVArray (Array ParsedValue)
    | PVInlineTable (List ( ParsedKey, ParsedValue ))


{-| -}
type alias ParsedKey =
    ( String, List String )


{-| -}
type Error
    = InvalidComment String
    | DuplicateKey String
    | TriedToChangeValue { from : String, to : String }
    | Other (Parser.Advanced.DeadEnd Context Problem)


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type Context
    = Context_TODO
    | ParsingDate
    | ParsingTime
    | ParsingOffset


type Problem
    = ExpectingSpace
    | ExpectingNewLine
    | ExpectingCarriageReturn
    | ExpectingCarriageReturnNewline
    | ExpectingCommentStart
    | InvaliadCommentCharacters { start : ( Int, Int ), end : ( Int, Int ), indicies : List Int }
    | ExpectingKeyValueSeparator
    | ExpectingDottedKeySeparator
    | ExpectingKeyChar
    | ExpectingBasicStringStart
    | ExpectingBasicStringEnd
    | ExpectingMultilineBasicStringStart
    | ExpectingMultilineBasicDoubleQuote
    | ExpectingMultilineBasicStringEnd
    | ExpectingLiteralStringStart
    | ExpectingLiteralStringEnd
    | ExpectingMultilineLiteralStringStart
    | ExpectingMultilineLiteralSingleQuote
    | ExpectingMultilineLiteralStringEnd
    | ExpectingStringEscapeChar
    | ExpectingUnicodeDigit
    | ExpectingEscapedString String
    | ExpectingPlusSymbol
    | ExpectingMinusSymbol
    | ExpectingDigit
    | ExpectingInt
    | ExpectingFloat
    | ExpectingHexStart
    | ExpectingOctalStart
    | ExpectingBinaryStart
    | ExpectingFloatDecimal
    | ExpectingExponentStart
    | ExpectingFloatInfinity
    | ExpectingFloatNan
    | ExpectingTrue
    | ExpectingFalse
    | ExpectingArrayStart
    | ExpectingArraySeparator
    | ExpectingArrayEnd
    | ExpectingArrayTableStart
    | ExpectingArrayTableEnd
    | ExpectingStandardTableStart
    | ExpectingStandardTableEnd
    | ExpectingInlineTableStart
    | ExpectingInlineTableSeparator
    | ExpectingInlineTableEnd
    | UnknownValue { start : ( Int, Int ), end : ( Int, Int ) }
    | ExpectingEndOfFile
      --
    | ExpectedDateSeparator
    | ExpectedDateTimeSeparator
    | ExpectedTimeSeparator
    | ExpectedOffsetSeparator
    | InvalidMonth
    | DayTooLarge Int
    | ExpectedZuluOffset
    | ExpectedOffsetSign
    | ExpectedFractionalSecondSeparator
    | ExpectedDigit
    | ExpectedAFloat
    | ExpectedAnInt
    | InvalidNegativeDigits
    | InvalidHour
    | InvalidMinute
    | InvalidSecond
    | InvalidDay


{-| -}
parse : String -> Result (List Error) Toml
parse source =
    Parser.Advanced.run tomlParser source
        |> Result.mapError (List.map (problemToError source))
        |> Result.andThen
            (Util.List.foldlResult
                (\expr acc ->
                    case expr of
                        CommentExpr _ ->
                            Ok acc

                        KeyValueExpr keyValue _ ->
                            insertKeyValuePair keyValue acc

                        TableExpr table _ ->
                            case table of
                                StandardTable key keyValues ->
                                    insertTable key keyValues acc

                                ArrayTable ( key, restKeys ) keyValues ->
                                    List.foldl
                                        (\tableKeyValue accum ->
                                            case ( insertKeyValuePair tableKeyValue Dict.empty, accum ) of
                                                ( Ok tbl, Ok a ) ->
                                                    updateOrInsertOrError
                                                        ( key
                                                        , restKeys
                                                        , Array (Array.fromList [ Table tbl ])
                                                        )
                                                        a
                                                        |> Result.mapError List.singleton

                                                ( Ok _, Err _ ) ->
                                                    accum

                                                ( Err e, Ok _ ) ->
                                                    Err e

                                                ( Err e, Err errs ) ->
                                                    Err (errs ++ e)
                                        )
                                        (Ok acc)
                                        keyValues
                )
                (Ok Dict.empty)
            )


insertKeyValuePair : ( ParsedKey, ParsedValue ) -> Toml -> Result (List Error) Toml
insertKeyValuePair ( key, value ) toml =
    case mapValue value of
        Err errs ->
            Err errs

        Ok val ->
            case key of
                ( first, [] ) ->
                    insertOrError ( first, val ) toml

                ( first, rest ) ->
                    case List.reverse rest of
                        [] ->
                            insertOrError ( first, val ) toml

                        deepKey :: restKeys ->
                            let
                                nestedTable =
                                    List.foldr
                                        (\k t ->
                                            Table (Dict.singleton k t)
                                        )
                                        (Table (Dict.singleton deepKey val))
                                        restKeys
                            in
                            insertOrError ( first, nestedTable ) toml


insertTable : ParsedKey -> List ( ParsedKey, ParsedValue ) -> Toml -> Result (List Error) Toml
insertTable ( key, nestedKeys ) values toml =
    case Dict.get key toml of
        Nothing ->
            case nestedKeys of
                [] ->
                    let
                        tableRes : Result (List Error) Toml
                        tableRes =
                            List.foldl
                                (\kvPair ->
                                    Result.andThen (insertKeyValuePair kvPair)
                                )
                                (Ok Dict.empty)
                                values
                    in
                    tableRes
                        |> Result.map
                            (\table ->
                                Dict.insert key (Table table) toml
                            )

                deepKey :: restKeys ->
                    let
                        tableRes : Result (List Error) Toml
                        tableRes =
                            List.foldl
                                (\kvPair ->
                                    Result.andThen (insertKeyValuePair kvPair)
                                )
                                (Ok Dict.empty)
                                values
                    in
                    tableRes
                        |> Result.map
                            (\table ->
                                let
                                    nestedTable =
                                        List.foldr
                                            (\k t ->
                                                Table (Dict.singleton k t)
                                            )
                                            (Table (Dict.singleton deepKey (Table table)))
                                            restKeys
                                in
                                Dict.insert key nestedTable toml
                            )

        Just table ->
            Debug.todo ""


mapValue : ParsedValue -> Result (List Error) Value
mapValue pv =
    case pv of
        PVString str ->
            Ok (String str)

        PVInteger i ->
            Ok (Integer i)

        PVFloat f ->
            Ok (Float f)

        PVBoolean b ->
            Ok (Boolean b)

        PVDateTime dateTime ->
            Ok (DateTime dateTime)

        PVArray arr ->
            let
                ( mappedVals, errs ) =
                    arr
                        |> Array.toList
                        |> List.map mapValue
                        |> Util.Result.combine
            in
            case errs of
                [] ->
                    Ok (Array (Array.fromList mappedVals))

                _ ->
                    Err (List.concat errs)

        PVInlineTable keyVals ->
            case mapTableVal keyVals of
                Ok mappedVals ->
                    Ok (Table (Dict.fromList mappedVals))

                Err errs ->
                    Err errs


mapTableVal : List ( ParsedKey, ParsedValue ) -> Result (List Error) (List ( Key, Value ))
mapTableVal parsedKeyValues =
    mapTableValHelper parsedKeyValues ( [], [] )


mapTableValHelper : List ( ParsedKey, ParsedValue ) -> ( List Error, List ( Key, Value ) ) -> Result (List Error) (List ( Key, Value ))
mapTableValHelper parsedKeyValues ( errs, oks ) =
    case parsedKeyValues of
        [] ->
            case errs of
                [] ->
                    Ok (List.reverse oks)

                _ ->
                    Err errs

        ( ( key, remKeys ), value ) :: restKeyValues ->
            case mapValue value of
                Err err ->
                    mapTableValHelper restKeyValues ( err ++ errs, oks )

                Ok mpdVal ->
                    case List.reverse remKeys of
                        [] ->
                            mapTableValHelper restKeyValues
                                ( errs
                                , ( key, mpdVal ) :: oks
                                )

                        deepKey :: restKeys ->
                            let
                                nestedTable =
                                    List.foldr
                                        (\k t ->
                                            Table (Dict.singleton k t)
                                        )
                                        (Table (Dict.singleton deepKey mpdVal))
                                        restKeys
                            in
                            mapTableValHelper restKeyValues
                                ( errs
                                , ( key, nestedTable ) :: oks
                                )


insertOrError : ( Key, Value ) -> Toml -> Result (List Error) Toml
insertOrError ( key, value ) toml =
    case Dict.get key toml of
        Just _ ->
            Err [ DuplicateKey key ]

        Nothing ->
            Ok (Dict.insert key value toml)


updateOrInsertOrError : ( Key, List Key, Value ) -> Toml -> Result Error Toml
updateOrInsertOrError ( key, restKeys, value ) toml =
    case Dict.get key toml of
        Nothing ->
            case restKeys of
                [] ->
                    Ok (Dict.insert key value toml)

                nextKey :: remKeys ->
                    case updateOrInsertOrError ( nextKey, remKeys, value ) Dict.empty of
                        Ok v ->
                            Ok (Dict.insert key (Table v) toml)

                        Err err ->
                            Err err

        Just (String _) ->
            Err (TriedToChangeValue { from = "String", to = "???" })

        Just (Integer _) ->
            Err (TriedToChangeValue { from = "Integer", to = "???" })

        Just (Float _) ->
            Err (TriedToChangeValue { from = "Float", to = "???" })

        Just (Boolean _) ->
            Err (TriedToChangeValue { from = "Boolean", to = "???" })

        Just (DateTime _) ->
            Err (TriedToChangeValue { from = "DateTime", to = "???" })

        Just (Array _) ->
            Err (TriedToChangeValue { from = "Array", to = "???" })

        Just (Table table) ->
            case restKeys of
                [] ->
                    Err (TriedToChangeValue { from = "Array", to = "???" })

                nextKey :: [] ->
                    Ok (Dict.insert key (Table (Dict.insert nextKey value table)) toml)

                nextKey :: remKeys ->
                    Debug.todo ""


problemToError : String -> Parser.Advanced.DeadEnd Context Problem -> Error
problemToError source deadEnd =
    case deadEnd.problem of
        InvaliadCommentCharacters details ->
            let
                invalidText =
                    source
                        |> String.split "\n"
                        |> List.drop (Tuple.first details.start - 1)
                        |> List.take 1
                        |> String.join "\n"

                errorNote =
                    List.foldl
                        (\index str ->
                            String.left index str ++ "^" ++ String.dropLeft (index + 1) str
                        )
                        (String.repeat (String.length invalidText) " ")
                        details.indicies
            in
            InvalidComment (String.join "\n" [ invalidText, errorNote ])

        _ ->
            Other deadEnd


tomlParser : Parser ParsedToml
tomlParser =
    Parser.Advanced.succeed List.reverse
        |. spacesParser
        |= Parser.Advanced.loop [] tomlBodyParser


tomlBodyParser : List Expression -> Parser (Parser.Advanced.Step (List Expression) (List Expression))
tomlBodyParser revExpressions =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed
            (Parser.Advanced.Done revExpressions)
            |. Parser.Advanced.end ExpectingEndOfFile
        , Parser.Advanced.succeed (\expr -> Parser.Advanced.Loop (expr :: revExpressions))
            |= expressionParser
        , Parser.Advanced.succeed
            (Parser.Advanced.Loop revExpressions)
            |. whiteSpaceParser
        ]


type Expression
    = CommentExpr (Maybe String)
    | KeyValueExpr ( ParsedKey, ParsedValue ) (Maybe String)
    | TableExpr PVTable (Maybe String)


expressionParser : Parser Expression
expressionParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed KeyValueExpr
            |. spacesParser
            |= keyValueParser
            |. spacesParser
            |= Parser.Advanced.oneOf
                [ optionalCommentParser
                , Parser.Advanced.succeed Nothing
                    |. newLineParser
                ]
            |. whiteSpaceParser
        , Parser.Advanced.succeed TableExpr
            |. spacesParser
            |= tableParser
            |. spacesParser
            |= optionalCommentParser
            |. whiteSpaceParser
        , Parser.Advanced.succeed CommentExpr
            |. spacesParser
            |= optionalCommentParser
            |. whiteSpaceParser
        ]


spaceParser : Parser ()
spaceParser =
    Parser.Advanced.chompIf (\char -> char == '\t' || char == ' ') ExpectingSpace


isWhiteSpaceChar : Char -> Bool
isWhiteSpaceChar char =
    Set.member char whiteSpaceChars


whiteSpaceChars : Set Char
whiteSpaceChars =
    Set.fromList
        [ '\t'
        , ' '
        , '\u{0000}'
        , '\u{0001}'
        , '\u{0002}'
        , '\u{0003}'
        , '\u{0004}'
        , '\u{0005}'
        , '\u{0006}'
        , '\u{0007}'
        , '\u{0008}'
        , '\n'
        , '\u{000B}'
        , '\u{000C}'
        , '\u{000D}'
        , '\u{000E}'
        , '\u{000F}'
        , '\u{0010}'
        , '\u{0011}'
        , '\u{0012}'
        , '\u{0013}'
        , '\u{0014}'
        , '\u{0015}'
        , '\u{0016}'
        , '\u{0017}'
        , '\u{0018}'
        , '\u{0019}'
        , '\u{001A}'
        , '\u{001B}'
        , '\u{001C}'
        , '\u{001D}'
        , '\u{001E}'
        , '\u{001F}'
        , '\u{007F}'
        ]


spacesParser : Parser ()
spacesParser =
    Parser.Advanced.loop () spacesParserHelper


spacesParserHelper : () -> Parser (Parser.Advanced.Step () ())
spacesParserHelper () =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Loop ())
            |. spaceParser
        , Parser.Advanced.succeed (Parser.Advanced.Done ())
        ]


newLineParser : Parser ()
newLineParser =
    Parser.Advanced.succeed ()
        |. Parser.Advanced.oneOf
            [ Parser.Advanced.chompIf (\char -> char == '\u{000D}')
                ExpectingCarriageReturn
            , Parser.Advanced.succeed ()
            ]
        |. Parser.Advanced.chompIf (\char -> char == '\n')
            ExpectingNewLine


whiteSpaceParser : Parser ()
whiteSpaceParser =
    Parser.Advanced.loop () whiteSpaceParserHelper


whiteSpaceParserHelper : () -> Parser (Parser.Advanced.Step () ())
whiteSpaceParserHelper () =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Loop ())
            |. spaceParser
        , Parser.Advanced.succeed (Parser.Advanced.Loop ())
            |. newLineParser
        , Parser.Advanced.succeed (Parser.Advanced.Done ())
        ]


commentParser : Parser String
commentParser =
    Parser.Advanced.succeed (\start comment end -> ( start, comment, end ))
        |= Parser.Advanced.getPosition
        |= (Parser.Advanced.succeed ()
                |. Parser.Advanced.chompIf (\char -> char == '#') ExpectingCommentStart
                |. Parser.Advanced.Workaround.chompUntilEndOrAfter (Parser.Advanced.Token "\n" ExpectingNewLine)
                |> Parser.Advanced.getChompedString
                |> Parser.Advanced.map
                    (\str ->
                        if String.endsWith "\u{000D}\n" str then
                            String.dropRight 2 str

                        else if String.endsWith "\n" str then
                            String.dropRight 1 str

                        else
                            str
                    )
           )
        |= Parser.Advanced.getPosition
        |> Parser.Advanced.andThen
            (\( start, comment, end ) ->
                let
                    invalid =
                        List.concatMap (\char -> String.indices char comment)
                            disallowedCommentChars
                in
                case invalid of
                    [] ->
                        Parser.Advanced.succeed comment

                    _ ->
                        Parser.Advanced.problem
                            (InvaliadCommentCharacters { start = start, end = end, indicies = invalid })
            )


optionalCommentParser : Parser (Maybe String)
optionalCommentParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.map Just
            commentParser
        , Parser.Advanced.succeed Nothing
        ]


{-| These control characters aren't allowed in comments:

  - U+0000 to U+0008
  - U+000A to U+001F
  - U+007F

-}
disallowedCommentChars : List String
disallowedCommentChars =
    [ "\u{0000}"
    , "\u{0001}"
    , "\u{0002}"
    , "\u{0003}"
    , "\u{0004}"
    , "\u{0005}"
    , "\u{0006}"
    , "\u{0007}"
    , "\u{0008}"
    , "\n"
    , "\u{000B}"
    , "\u{000C}"
    , "\u{000D}"
    , "\u{000E}"
    , "\u{000F}"
    , "\u{0010}"
    , "\u{0011}"
    , "\u{0012}"
    , "\u{0013}"
    , "\u{0014}"
    , "\u{0015}"
    , "\u{0016}"
    , "\u{0017}"
    , "\u{0018}"
    , "\u{0019}"
    , "\u{001A}"
    , "\u{001B}"
    , "\u{001C}"
    , "\u{001D}"
    , "\u{001E}"
    , "\u{001F}"
    , "\u{007F}"
    ]


keyValueParser : Parser ( ParsedKey, ParsedValue )
keyValueParser =
    Parser.Advanced.succeed Tuple.pair
        |. spacesParser
        |= dottedkeyParser
        |. spacesParser
        |. Parser.Advanced.symbol (Parser.Advanced.Token "=" ExpectingKeyValueSeparator)
        |. spacesParser
        |= valueParser
        |. Parser.Advanced.oneOf
            [ newLineParser
            , Parser.Advanced.end ExpectingEndOfFile
            ]


dottedkeyParser : Parser ParsedKey
dottedkeyParser =
    Parser.Advanced.succeed Tuple.pair
        |= keyParser
        |= Parser.Advanced.loop [] dottedkeyParserHelper


dottedkeyParserHelper : List String -> Parser (Parser.Advanced.Step (List String) (List String))
dottedkeyParserHelper reverseKeys =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (\key -> Parser.Advanced.Loop (key :: reverseKeys))
            |. spacesParser
            |. Parser.Advanced.symbol (Parser.Advanced.Token "." ExpectingDottedKeySeparator)
            |. spacesParser
            |= keyParser
            |> Parser.Advanced.backtrackable
        , Parser.Advanced.succeed (Parser.Advanced.Done (List.reverse reverseKeys))
        ]


keyParser : Parser String
keyParser =
    Parser.Advanced.oneOf
        [ bareKeyParser
        , quotedKeyParser
        ]


bareKeyParser : Parser String
bareKeyParser =
    Parser.Advanced.succeed ()
        |. Parser.Advanced.chompIf (\char -> Char.isAlphaNum char || char == '-' || char == '_') ExpectingKeyChar
        |. Parser.Advanced.chompWhile (\char -> Char.isAlphaNum char || char == '-' || char == '_')
        |> Parser.Advanced.getChompedString


quotedKeyParser : Parser String
quotedKeyParser =
    Parser.Advanced.oneOf
        [ basicStringParser
        , literalStringParser
        ]



-- VALUES


valueParser : Parser ParsedValue
valueParser =
    Parser.Advanced.succeed identity
        |= Parser.Advanced.oneOf valueParsers
        |. spacesParser


valueParsers : List (Parser ParsedValue)
valueParsers =
    [ dateTimeParser
    , numberParser
    , booleanParser
    , stringParser
    , arrayParser
    , inlineTableParser
    ]



-- STRINGS


stringParser : Parser ParsedValue
stringParser =
    Parser.Advanced.succeed PVString
        |= Parser.Advanced.oneOf
            [ multilineBasicStringParser
                |> Parser.Advanced.backtrackable
            , basicStringParser
            , multilineLiteralStringParser
                |> Parser.Advanced.backtrackable
            , literalStringParser
            ]


basicStringParser : Parser String
basicStringParser =
    Parser.Advanced.succeed (String.join "")
        |. Parser.Advanced.token (Parser.Advanced.Token "\"" ExpectingBasicStringStart)
        |= Parser.Advanced.loop [] basicStringParserHelper


basicStringParserHelper : List String -> Parser (Parser.Advanced.Step (List String) (List String))
basicStringParserHelper revChunks =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (\chunk -> Parser.Advanced.Loop (chunk :: revChunks))
            |. Parser.Advanced.token (Parser.Advanced.Token "\\" ExpectingStringEscapeChar)
            |= Parser.Advanced.oneOf
                [ Parser.Advanced.token (escapedToken "b")
                    |> Parser.Advanced.map (\() -> "\u{0008}")
                , Parser.Advanced.token (escapedToken "t")
                    |> Parser.Advanced.map (\() -> "\t")
                , Parser.Advanced.token (escapedToken "n")
                    |> Parser.Advanced.map (\() -> "\n")
                , Parser.Advanced.token (escapedToken "f")
                    |> Parser.Advanced.map (\() -> "\u{000C}")
                , Parser.Advanced.token (escapedToken "r")
                    |> Parser.Advanced.map (\() -> "\u{000D}")
                , Parser.Advanced.token (escapedToken "\\")
                    |> Parser.Advanced.map (\() -> "\\")
                ]
        , Parser.Advanced.succeed (\chunk -> Parser.Advanced.Loop (("\\u" ++ chunk) :: revChunks))
            |. Parser.Advanced.token (Parser.Advanced.Token "\\" ExpectingStringEscapeChar)
            |. Parser.Advanced.token (escapedToken "u")
            |= (Parser.Advanced.succeed ()
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |> Parser.Advanced.getChompedString
                    |> Parser.Advanced.backtrackable
               )
        , Parser.Advanced.succeed (\chunk -> Parser.Advanced.Loop (("\\U" ++ chunk) :: revChunks))
            |. Parser.Advanced.token (Parser.Advanced.Token "\\" ExpectingStringEscapeChar)
            |. Parser.Advanced.token (escapedToken "U")
            |= (Parser.Advanced.succeed ()
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |> Parser.Advanced.getChompedString
                    |> Parser.Advanced.backtrackable
               )
        , Parser.Advanced.token (Parser.Advanced.Token "\"" ExpectingBasicStringEnd)
            |> Parser.Advanced.map (\_ -> Parser.Advanced.Done (List.reverse revChunks))
        , Parser.Advanced.chompWhile isUninterestingForAString
            |> Parser.Advanced.getChompedString
            |> Parser.Advanced.map (\chunk -> Parser.Advanced.Loop (chunk :: revChunks))
        ]


isUninterestingForAString : Char -> Bool
isUninterestingForAString char =
    char /= '\\' && char /= '"'


multilineBasicStringParser : Parser String
multilineBasicStringParser =
    Parser.Advanced.succeed
        (\str ->
            if String.startsWith "\n" str then
                String.dropLeft 1 str

            else if String.startsWith "\u{000D}\n" str then
                String.dropLeft 2 str

            else
                str
        )
        |. Parser.Advanced.token (Parser.Advanced.Token "\"\"\"" ExpectingMultilineBasicStringStart)
        |= Parser.Advanced.loop [] multilineBasicStringParserHelper


multilineBasicStringParserHelper : List String -> Parser (Parser.Advanced.Step (List String) String)
multilineBasicStringParserHelper revChunks =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Loop revChunks)
            |. Parser.Advanced.token (Parser.Advanced.Token "\\" ExpectingStringEscapeChar)
            |. Parser.Advanced.oneOf
                [ Parser.Advanced.token (Parser.Advanced.Token "\n" ExpectingNewLine)
                , Parser.Advanced.token (Parser.Advanced.Token "\u{000D}\n" ExpectingCarriageReturnNewline)
                ]
            |. Parser.Advanced.chompWhile isWhiteSpaceChar
        , Parser.Advanced.succeed (\chunk -> Parser.Advanced.Loop (chunk :: revChunks))
            |. Parser.Advanced.token (Parser.Advanced.Token "\\" ExpectingStringEscapeChar)
            |= Parser.Advanced.oneOf
                [ Parser.Advanced.token (escapedToken "b")
                    |> Parser.Advanced.map (\() -> "\u{0008}")
                , Parser.Advanced.token (escapedToken "t")
                    |> Parser.Advanced.map (\() -> "\t")
                , Parser.Advanced.token (escapedToken "n")
                    |> Parser.Advanced.map (\() -> "\n")
                , Parser.Advanced.token (escapedToken "f")
                    |> Parser.Advanced.map (\() -> "\u{000C}")
                , Parser.Advanced.token (escapedToken "r")
                    |> Parser.Advanced.map (\() -> "\u{000D}")
                , Parser.Advanced.token (escapedToken "\\")
                    |> Parser.Advanced.map (\() -> "\\")
                ]
        , Parser.Advanced.succeed (\chunk -> Parser.Advanced.Loop (("\\u" ++ chunk) :: revChunks))
            |. Parser.Advanced.token (Parser.Advanced.Token "\\" ExpectingStringEscapeChar)
            |. Parser.Advanced.token (escapedToken "u")
            |= (Parser.Advanced.succeed ()
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |> Parser.Advanced.getChompedString
                    |> Parser.Advanced.backtrackable
               )
        , Parser.Advanced.succeed (\chunk -> Parser.Advanced.Loop (("\\U" ++ chunk) :: revChunks))
            |. Parser.Advanced.token (Parser.Advanced.Token "\\" ExpectingStringEscapeChar)
            |. Parser.Advanced.token (escapedToken "U")
            |= (Parser.Advanced.succeed ()
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |. Parser.Advanced.chompIf Char.isDigit ExpectingUnicodeDigit
                    |> Parser.Advanced.getChompedString
                    |> Parser.Advanced.backtrackable
               )
        , Parser.Advanced.token (Parser.Advanced.Token "\"\"\"\"\"" ExpectingMultilineBasicStringEnd)
            |> Parser.Advanced.map (\_ -> Parser.Advanced.Done (String.join "" (List.reverse ("\"\"" :: revChunks))))
        , Parser.Advanced.token (Parser.Advanced.Token "\"\"\"\"" ExpectingMultilineBasicStringEnd)
            |> Parser.Advanced.map (\_ -> Parser.Advanced.Done (String.join "" (List.reverse ("\"" :: revChunks))))
        , Parser.Advanced.token (Parser.Advanced.Token "\"\"\"" ExpectingMultilineBasicStringEnd)
            |> Parser.Advanced.map (\_ -> Parser.Advanced.Done (String.join "" (List.reverse revChunks)))
        , Parser.Advanced.succeed (Parser.Advanced.Loop ("\"" :: revChunks))
            |. Parser.Advanced.token (Parser.Advanced.Token "\"" ExpectingMultilineBasicDoubleQuote)
        , Parser.Advanced.chompWhile isUninterestingForAString
            |> Parser.Advanced.getChompedString
            |> Parser.Advanced.map (\chunk -> Parser.Advanced.Loop (chunk :: revChunks))
        ]


escapedToken : String -> Parser.Advanced.Token Problem
escapedToken str =
    Parser.Advanced.Token str (ExpectingEscapedString str)


literalStringParser : Parser String
literalStringParser =
    Parser.Advanced.succeed identity
        |. Parser.Advanced.token (Parser.Advanced.Token "'" ExpectingLiteralStringStart)
        |= Parser.Advanced.loop [] literalStringParserHelper


literalStringParserHelper : List String -> Parser (Parser.Advanced.Step (List String) String)
literalStringParserHelper revChunks =
    Parser.Advanced.oneOf
        [ Parser.Advanced.token (Parser.Advanced.Token "'" ExpectingLiteralStringEnd)
            |> Parser.Advanced.map (\_ -> Parser.Advanced.Done (String.join "" (List.reverse revChunks)))
        , Parser.Advanced.chompWhile isUninterestingForAStringLiteral
            |> Parser.Advanced.getChompedString
            |> Parser.Advanced.map (\chunk -> Parser.Advanced.Loop (chunk :: revChunks))
        ]


isUninterestingForAStringLiteral : Char -> Bool
isUninterestingForAStringLiteral char =
    char /= '\''


multilineLiteralStringParser : Parser String
multilineLiteralStringParser =
    Parser.Advanced.succeed
        (\strList ->
            let
                str =
                    String.join "" strList
            in
            if String.startsWith "\n" str then
                String.dropLeft 1 str

            else if String.startsWith "\u{000D}\n" str then
                String.dropLeft 2 str

            else
                str
        )
        |. Parser.Advanced.token (Parser.Advanced.Token "'''" ExpectingMultilineLiteralStringStart)
        |= Parser.Advanced.loop [] multilineLiteralStringParserHelper


multilineLiteralStringParserHelper : List String -> Parser (Parser.Advanced.Step (List String) (List String))
multilineLiteralStringParserHelper revChunks =
    Parser.Advanced.oneOf
        [ Parser.Advanced.token (Parser.Advanced.Token "'''''" ExpectingMultilineLiteralStringEnd)
            |> Parser.Advanced.map (\_ -> Parser.Advanced.Done (List.reverse ("''" :: revChunks)))
        , Parser.Advanced.token (Parser.Advanced.Token "''''" ExpectingMultilineLiteralStringEnd)
            |> Parser.Advanced.map (\_ -> Parser.Advanced.Done (List.reverse ("'" :: revChunks)))
        , Parser.Advanced.token (Parser.Advanced.Token "'''" ExpectingMultilineLiteralStringEnd)
            |> Parser.Advanced.map (\_ -> Parser.Advanced.Done (List.reverse revChunks))
        , Parser.Advanced.succeed (Parser.Advanced.Loop ("'" :: revChunks))
            |. Parser.Advanced.token (Parser.Advanced.Token "'" ExpectingMultilineLiteralSingleQuote)
        , Parser.Advanced.chompWhile isUninterestingForAStringLiteral
            |> Parser.Advanced.getChompedString
            |> Parser.Advanced.map (\chunk -> Parser.Advanced.Loop (chunk :: revChunks))
        ]



-- NUMBERS


numberParser : Parser ParsedValue
numberParser =
    Parser.Advanced.oneOf
        [ floatParser
        , integerParser
        ]


integerParser : Parser ParsedValue
integerParser =
    Parser.Advanced.succeed
        (\shouldNegate value ->
            PVInteger
                (if shouldNegate then
                    negate value

                 else
                    value
                )
        )
        |= negateParser
        |= Parser.Advanced.oneOf
            [ hexIntParser
                |> Parser.Advanced.backtrackable
            , octalIntParser
                |> Parser.Advanced.backtrackable
            , binaryIntParser
                |> Parser.Advanced.backtrackable
            , decimalIntParser
            ]


decimalIntParser : Parser Int
decimalIntParser =
    Parser.Advanced.succeed ()
        |. Parser.Advanced.chompIf Char.isDigit ExpectingDigit
        |. Parser.Advanced.chompWhile (\char -> Char.isDigit char || char == '_')
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen
            (\intStr ->
                case String.toInt (String.replace "_" "" intStr) of
                    Nothing ->
                        Parser.Advanced.problem ExpectingInt

                    Just i ->
                        Parser.Advanced.succeed i
            )


hexIntParser : Parser Int
hexIntParser =
    Parser.Advanced.succeed ()
        |. Parser.Advanced.token (Parser.Advanced.Token "0x" ExpectingHexStart)
        |. Parser.Advanced.chompWhile (\char -> char == '_' || Set.member (Char.toUpper char) hexChars)
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.map
            (\intStr ->
                intStr
                    |> String.replace "_" ""
                    |> String.foldr
                        (\char ( total, index ) ->
                            ( case String.toInt (String.fromChar char) of
                                Nothing ->
                                    total

                                Just i ->
                                    total + (i * 16 ^ index)
                            , index + 1
                            )
                        )
                        ( 0, 0 )
                    |> Tuple.first
            )


hexChars : Set Char
hexChars =
    Set.fromList
        [ '0'
        , '1'
        , '2'
        , '3'
        , '4'
        , '5'
        , '6'
        , '7'
        , '8'
        , '9'
        , 'A'
        , 'B'
        , 'C'
        , 'D'
        , 'E'
        , 'F'
        ]


octalIntParser : Parser Int
octalIntParser =
    Parser.Advanced.succeed ()
        |. Parser.Advanced.token (Parser.Advanced.Token "0o" ExpectingHexStart)
        |. Parser.Advanced.chompWhile (\char -> char == '_' || Set.member (Char.toUpper char) octalChars)
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.map
            (\intStr ->
                intStr
                    |> String.replace "_" ""
                    |> String.foldr
                        (\char ( total, index ) ->
                            ( case String.toInt (String.fromChar char) of
                                Nothing ->
                                    total

                                Just i ->
                                    total + (i * 8 ^ index)
                            , index + 1
                            )
                        )
                        ( 0, 0 )
                    |> Tuple.first
            )


octalChars : Set Char
octalChars =
    Set.fromList
        [ '0'
        , '1'
        , '2'
        , '3'
        , '4'
        , '5'
        , '6'
        , '7'
        , '8'
        ]


binaryIntParser : Parser Int
binaryIntParser =
    Parser.Advanced.succeed ()
        |. Parser.Advanced.token (Parser.Advanced.Token "0b" ExpectingHexStart)
        |. Parser.Advanced.chompWhile (\char -> char == '_' || char == '0' || char == '1')
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.map
            (\intStr ->
                intStr
                    |> String.replace "_" ""
                    |> String.foldr
                        (\char ( total, index ) ->
                            ( case String.toInt (String.fromChar char) of
                                Nothing ->
                                    total

                                Just i ->
                                    total + (i * 2 ^ index)
                            , index + 1
                            )
                        )
                        ( 0, 0 )
                    |> Tuple.first
            )


floatParser : Parser ParsedValue
floatParser =
    Parser.Advanced.succeed
        (\shouldNegate value ->
            case value of
                Err i ->
                    PVInteger
                        (if shouldNegate then
                            negate i

                         else
                            i
                        )

                Ok f ->
                    PVFloat
                        (if shouldNegate then
                            negate f

                         else
                            f
                        )
        )
        |= negateParser
        |= Parser.Advanced.oneOf
            [ floatLiteralParser
            , Parser.Advanced.map Ok
                floatNamedParser
            ]


floatLiteralParser : Parser (Result Int Float)
floatLiteralParser =
    Parser.Advanced.succeed
        (\integerPortion decimalPortion exponent ->
            { integerPortion = integerPortion
            , decimalPortion = decimalPortion
            , exponent = exponent
            }
        )
        |= decimalIntParser
        |= Parser.Advanced.oneOf
            [ floatDecimalParser
                |> Parser.Advanced.map Just
            , Parser.Advanced.succeed Nothing
            ]
        |= Parser.Advanced.oneOf
            [ floatExponentParser
                |> Parser.Advanced.map Just
            , Parser.Advanced.succeed Nothing
            ]
        |> Parser.Advanced.andThen
            (\parts ->
                case ( parts.decimalPortion, parts.exponent ) of
                    ( Nothing, Nothing ) ->
                        parts.integerPortion
                            |> toFloat
                            |> round
                            |> Err
                            |> Parser.Advanced.succeed

                    _ ->
                        let
                            decimal : String
                            decimal =
                                Maybe.withDefault ".0" parts.decimalPortion
                        in
                        case String.toFloat (String.fromInt parts.integerPortion ++ decimal) of
                            Nothing ->
                                Debug.todo (String.fromInt parts.integerPortion ++ decimal)

                            Just f ->
                                let
                                    exponent : Float
                                    exponent =
                                        toFloat (Maybe.withDefault 0 parts.exponent)
                                in
                                Parser.Advanced.succeed (Ok (f * 10.0 ^ exponent))
            )


floatDecimalParser : Parser String
floatDecimalParser =
    Parser.Advanced.succeed ()
        |. Parser.Advanced.token (Parser.Advanced.Token "." ExpectingFloatDecimal)
        |. Parser.Advanced.chompIf Char.isDigit ExpectingDigit
        |. Parser.Advanced.chompWhile (\char -> Char.isDigit char || char == '_')
        |> Parser.Advanced.getChompedString


floatExponentParser : Parser Int
floatExponentParser =
    Parser.Advanced.succeed
        (\shouldNegate i ->
            if shouldNegate then
                negate i

            else
                i
        )
        |. Parser.Advanced.oneOf
            [ Parser.Advanced.token (Parser.Advanced.Token "e" ExpectingExponentStart)
            , Parser.Advanced.token (Parser.Advanced.Token "E" ExpectingExponentStart)
            ]
        |= negateParser
        |= decimalIntParser


floatNamedParser : Parser Float
floatNamedParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (1 / 0)
            |. Parser.Advanced.token (Parser.Advanced.Token "inf" ExpectingFloatInfinity)
        , Parser.Advanced.succeed (0 / 0)
            |. Parser.Advanced.token (Parser.Advanced.Token "nan" ExpectingFloatNan)
        ]


negateParser : Parser Bool
negateParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed False
            |. Parser.Advanced.token (Parser.Advanced.Token "+" ExpectingPlusSymbol)
        , Parser.Advanced.succeed True
            |. Parser.Advanced.token (Parser.Advanced.Token "-" ExpectingMinusSymbol)
        , Parser.Advanced.succeed False
        ]



-- BOOLEAN


booleanParser : Parser ParsedValue
booleanParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (PVBoolean True)
            |. Parser.Advanced.token (Parser.Advanced.Token "true" ExpectingTrue)
        , Parser.Advanced.succeed (PVBoolean False)
            |. Parser.Advanced.token (Parser.Advanced.Token "false" ExpectingFalse)
        ]



-- TIME
-- ARRAY


arrayParser : Parser ParsedValue
arrayParser =
    Parser.Advanced.succeed identity
        |. Parser.Advanced.token (Parser.Advanced.Token "[" ExpectingArrayStart)
        |= Parser.Advanced.loop (LookingForEndOrValue []) arrayParserHelper
        |> Parser.Advanced.andThen
            (\builder ->
                case builder of
                    Complete revValues ->
                        revValues
                            |> List.reverse
                            |> Array.fromList
                            |> PVArray
                            |> Parser.Advanced.succeed

                    _ ->
                        Parser.Advanced.problem (Debug.todo "")
            )


arrayParserHelper : ArrayBuilder -> Parser (Parser.Advanced.Step ArrayBuilder ArrayBuilder)
arrayParserHelper builder =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed
            (case builder of
                LookingForEndOrSeprator revValues ->
                    Parser.Advanced.Done (Complete revValues)

                LookingForEndOrValue revValues ->
                    Parser.Advanced.Done (Complete revValues)

                Complete revValues ->
                    Parser.Advanced.Done (Complete revValues)

                ArrayError ->
                    Parser.Advanced.Done ArrayError
            )
            |. whiteSpaceParser
            |. Parser.Advanced.token (Parser.Advanced.Token "]" ExpectingArrayEnd)
            |> Parser.Advanced.backtrackable
        , Parser.Advanced.succeed ()
            |. whiteSpaceParser
            |. Parser.Advanced.token (Parser.Advanced.Token "," ExpectingArraySeparator)
            |. whiteSpaceParser
            |. optionalCommentParser
            |. whiteSpaceParser
            |> Parser.Advanced.map
                (\() ->
                    case builder of
                        LookingForEndOrSeprator revValues ->
                            Parser.Advanced.Loop (LookingForEndOrValue revValues)

                        LookingForEndOrValue _ ->
                            Parser.Advanced.Done ArrayError

                        Complete _ ->
                            Parser.Advanced.Done ArrayError

                        ArrayError ->
                            Parser.Advanced.Done ArrayError
                )
        , Parser.Advanced.succeed
            (\value ->
                case builder of
                    LookingForEndOrSeprator _ ->
                        Parser.Advanced.Done ArrayError

                    LookingForEndOrValue revValues ->
                        Parser.Advanced.Loop (LookingForEndOrSeprator (value :: revValues))

                    Complete _ ->
                        Parser.Advanced.Done ArrayError

                    ArrayError ->
                        Parser.Advanced.Done ArrayError
            )
            |= Parser.Advanced.lazy
                (\() ->
                    Parser.Advanced.oneOf valueParsers
                )
        ]


type ArrayBuilder
    = LookingForEndOrValue (List ParsedValue)
    | LookingForEndOrSeprator (List ParsedValue)
    | Complete (List ParsedValue)
    | ArrayError



-- INLINE TABLE


inlineTableParser : Parser ParsedValue
inlineTableParser =
    Parser.Advanced.succeed PVInlineTable
        |= Parser.Advanced.sequence
            { start = Parser.Advanced.Token "{" ExpectingInlineTableStart
            , item =
                Parser.Advanced.succeed Tuple.pair
                    |= dottedkeyParser
                    |. spacesParser
                    |. Parser.Advanced.symbol (Parser.Advanced.Token "=" ExpectingKeyValueSeparator)
                    |. spacesParser
                    |= Parser.Advanced.lazy
                        (\() -> Parser.Advanced.oneOf valueParsers)
            , separator = Parser.Advanced.Token "," ExpectingInlineTableSeparator
            , end = Parser.Advanced.Token "}" ExpectingInlineTableEnd
            , spaces = spacesParser
            , trailing = Parser.Advanced.Forbidden
            }



-- TABLE


type PVTable
    = StandardTable ParsedKey (List ( ParsedKey, ParsedValue ))
    | ArrayTable ParsedKey (List ( ParsedKey, ParsedValue ))


tableParser : Parser PVTable
tableParser =
    Parser.Advanced.oneOf
        [ arrayTableParser
        , standardTableParser
        ]


arrayTableParser : Parser PVTable
arrayTableParser =
    Parser.Advanced.succeed ArrayTable
        |. Parser.Advanced.token (Parser.Advanced.Token "[[" ExpectingArrayTableStart)
        |= dottedkeyParser
        |. Parser.Advanced.token (Parser.Advanced.Token "]]" ExpectingArrayTableEnd)
        |. whiteSpaceParser
        |= Parser.Advanced.loop [] tableValuesParser



-- |> Parser.Advanced.backtrackable


standardTableParser : Parser PVTable
standardTableParser =
    Parser.Advanced.succeed StandardTable
        |. Parser.Advanced.token (Parser.Advanced.Token "[" ExpectingStandardTableStart)
        |= dottedkeyParser
        |. Parser.Advanced.token (Parser.Advanced.Token "]" ExpectingStandardTableEnd)
        |. whiteSpaceParser
        |= Parser.Advanced.loop [] tableValuesParser


tableValuesParser : List ( ParsedKey, ParsedValue ) -> Parser (Parser.Advanced.Step (List ( ParsedKey, ParsedValue )) (List ( ParsedKey, ParsedValue )))
tableValuesParser revValues =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (\value -> Parser.Advanced.Loop (value :: revValues))
            |= keyValueParser
            |> Parser.Advanced.backtrackable
        , Parser.Advanced.succeed (Parser.Advanced.Done (List.reverse revValues))
        ]



-- DATE TIME
--
-- The below is borrowed from wolfadex/elm-rfc3339


dateTimeParser : Parser ParsedValue
dateTimeParser =
    Parser.Advanced.succeed PVDateTime
        |= Parser.Advanced.oneOf
            [ rfc3339DateTimeParser
                |> Parser.Advanced.backtrackable
            , Parser.Advanced.map Rfc3339.TimeLocal timeLocalParser
                |> Parser.Advanced.backtrackable
            ]


rfc3339DateTimeParser : Parser Rfc3339.DateTime
rfc3339DateTimeParser =
    Parser.Advanced.succeed
        (\date maybeTimeOffset ->
            case maybeTimeOffset of
                Nothing ->
                    Rfc3339.DateLocal date

                Just ( time, maybeOffset ) ->
                    case maybeOffset of
                        Nothing ->
                            Rfc3339.DateTimeLocal
                                { year = date.year
                                , month = date.month
                                , day = date.day
                                , hour = time.hour
                                , minute = time.minute
                                , second = time.second
                                }

                        Just offset ->
                            Rfc3339.DateTimeOffset
                                { year = date.year
                                , month = date.month
                                , day = date.day
                                , hour = time.hour
                                , minute = time.minute
                                , second = time.second
                                , offset = offset
                                }
        )
        |= dateParser
        |= Parser.Advanced.oneOf
            [ Parser.Advanced.succeed (\time maybeOffset -> Just ( time, maybeOffset ))
                |. Parser.Advanced.oneOf
                    [ Parser.Advanced.token (Parser.Advanced.Token "T" ExpectedDateTimeSeparator)
                    , Parser.Advanced.token (Parser.Advanced.Token "t" ExpectedDateTimeSeparator)
                    , Parser.Advanced.token (Parser.Advanced.Token " " ExpectedDateTimeSeparator)
                        |> Parser.Advanced.backtrackable
                    ]
                |= timeLocalParser
                |= Parser.Advanced.oneOf
                    [ Parser.Advanced.map Just offsetParser
                    , Parser.Advanced.succeed Nothing
                    ]
            , Parser.Advanced.succeed Nothing
            ]
        |> Parser.Advanced.andThen
            (\dateTime ->
                case dateTime of
                    Rfc3339.TimeLocal _ ->
                        Parser.Advanced.succeed dateTime

                    Rfc3339.DateLocal date ->
                        let
                            maxDays : Int
                            maxDays =
                                daysInMonth date
                        in
                        if date.day > maxDays then
                            Parser.Advanced.problem (DayTooLarge maxDays)

                        else
                            Parser.Advanced.succeed dateTime

                    Rfc3339.DateTimeLocal date ->
                        let
                            maxDays : Int
                            maxDays =
                                daysInMonth date
                        in
                        if date.day > maxDays then
                            Parser.Advanced.problem (DayTooLarge maxDays)

                        else
                            Parser.Advanced.succeed dateTime

                    Rfc3339.DateTimeOffset date ->
                        let
                            maxDays : Int
                            maxDays =
                                daysInMonth date
                        in
                        if date.day > maxDays then
                            Parser.Advanced.problem (DayTooLarge maxDays)

                        else
                            Parser.Advanced.succeed dateTime
            )


dateParser : Parser { year : Int, month : Time.Month, day : Int }
dateParser =
    Parser.Advanced.succeed
        (\year month day ->
            { year = year
            , month = month
            , day = day
            }
        )
        |= parseDigits 4
        |. Parser.Advanced.token (Parser.Advanced.Token "-" ExpectedDateSeparator)
        |= (parseDigits 2
                |> Parser.Advanced.andThen
                    (\int ->
                        case intToMonth int of
                            Nothing ->
                                Parser.Advanced.problem InvalidMonth

                            Just month ->
                                Parser.Advanced.succeed month
                    )
           )
        |. Parser.Advanced.token (Parser.Advanced.Token "-" ExpectedDateSeparator)
        |= parseDigitsInRange 2 { min = 1, max = 31 } InvalidDay
        |> Parser.Advanced.inContext ParsingDate


offsetParser : Parser { hour : Int, minute : Int }
offsetParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed { hour = 0, minute = 0 }
            |. Parser.Advanced.token (Parser.Advanced.Token "Z" ExpectedZuluOffset)
        , Parser.Advanced.succeed (\sign hour minute -> { hour = sign hour, minute = minute })
            |= Parser.Advanced.oneOf
                [ Parser.Advanced.succeed identity
                    |. Parser.Advanced.token (Parser.Advanced.Token "+" ExpectedOffsetSign)
                , Parser.Advanced.succeed negate
                    |. Parser.Advanced.token (Parser.Advanced.Token "-" ExpectedOffsetSign)
                ]
            |= hourParser
            |. Parser.Advanced.token (Parser.Advanced.Token ":" ExpectedOffsetSeparator)
            |= minuteParser
        ]
        |> Parser.Advanced.inContext ParsingOffset


timeLocalParser :
    Parser
        { hour : Int
        , minute : Int
        , second : Float
        }
timeLocalParser =
    Parser.Advanced.succeed
        (\hour minute second ->
            { hour = hour
            , minute = minute
            , second = second
            }
        )
        |= hourParser
        |. Parser.Advanced.token (Parser.Advanced.Token ":" ExpectedTimeSeparator)
        |= minuteParser
        |. Parser.Advanced.token (Parser.Advanced.Token ":" ExpectedTimeSeparator)
        |= (Parser.Advanced.succeed Tuple.pair
                |= parseDigitsInRange 2 { min = 0, max = 59 } InvalidSecond
                |= Parser.Advanced.oneOf
                    [ Parser.Advanced.succeed Just
                        |. Parser.Advanced.token (Parser.Advanced.Token "." ExpectedFractionalSecondSeparator)
                        |= (Parser.Advanced.succeed ()
                                |. Parser.Advanced.chompIf Char.isDigit ExpectedDigit
                                |. Parser.Advanced.chompWhile Char.isDigit
                                |> Parser.Advanced.getChompedString
                           )
                    , Parser.Advanced.succeed Nothing
                    ]
                |> Parser.Advanced.andThen
                    (\( second, fracSeconds ) ->
                        case fracSeconds of
                            Nothing ->
                                Parser.Advanced.succeed (toFloat second)

                            Just frac ->
                                case String.toFloat (String.fromInt second ++ "." ++ frac) of
                                    Nothing ->
                                        Parser.Advanced.problem ExpectedAFloat

                                    Just f ->
                                        Parser.Advanced.succeed f
                    )
           )
        |> Parser.Advanced.inContext ParsingTime


parseDigits : Int -> Parser Int
parseDigits size =
    Parser.Advanced.loop ( size, [] ) parseDigitsHelper
        |> Parser.Advanced.andThen
            (\digits ->
                case String.toInt digits of
                    Nothing ->
                        Parser.Advanced.problem ExpectedAnInt

                    Just i ->
                        Parser.Advanced.succeed i
            )


parseDigitsInRange : Int -> { min : Int, max : Int } -> Problem -> Parser Int
parseDigitsInRange size limits limitProblem =
    Parser.Advanced.loop ( size, [] ) parseDigitsHelper
        |> Parser.Advanced.andThen
            (\digits ->
                case String.toInt digits of
                    Nothing ->
                        Parser.Advanced.problem ExpectedAnInt

                    Just i ->
                        if i < limits.min then
                            Parser.Advanced.problem limitProblem

                        else if i > limits.max then
                            Parser.Advanced.problem limitProblem

                        else
                            Parser.Advanced.succeed i
            )


parseDigitsHelper : ( Int, List String ) -> Parser (Parser.Advanced.Step ( Int, List String ) String)
parseDigitsHelper ( leftToChomp, revDigits ) =
    if leftToChomp < 0 then
        Parser.Advanced.problem InvalidNegativeDigits

    else if leftToChomp > 0 then
        Parser.Advanced.succeed ()
            |. Parser.Advanced.chompIf Char.isDigit ExpectedDigit
            |> Parser.Advanced.getChompedString
            |> Parser.Advanced.map (\digit -> Parser.Advanced.Loop ( leftToChomp - 1, digit :: revDigits ))

    else
        Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse revDigits)))


intToMonth : Int -> Maybe Time.Month
intToMonth i =
    case i of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


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


hourParser : Parser Int
hourParser =
    parseDigitsInRange 2 { min = 0, max = 23 } InvalidHour


minuteParser : Parser Int
minuteParser =
    parseDigitsInRange 2 { min = 0, max = 59 } InvalidMinute


isLeapYear : Int -> Bool
isLeapYear year =
    modBy 400 year == 0 || (modBy 4 year == 0 && (modBy 100 year /= 0))


daysInMonth : { a | year : Int, month : Time.Month } -> Int
daysInMonth date =
    case date.month of
        Time.Jan ->
            31

        Time.Feb ->
            if isLeapYear date.year then
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


padInt2 : Int -> String
padInt2 i =
    String.padLeft 2 '0' (String.fromInt i)
