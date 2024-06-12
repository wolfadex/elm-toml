module Toml exposing (..)

{-| <https://toml.io/en/v1.0.0>
-}

import Array exposing (Array)
import Parser
import Parser.Advanced exposing ((|.), (|=))
import Parser.Advanced.Workaround
import Set exposing (Set)


type alias Toml =
    List Expression


type Value
    = String String
    | Integer Int
    | Float Float
    | Boolean Bool
    | Offset -- Date-Time
    | Local -- Date-Time
      -- | Local -- Date
      -- | Local -- Time
    | Array (Array Value)
    | InlineTable ( Key, Value )


type alias Key =
    ( String, List String )


type Error
    = InvalidComment String
    | Other (Parser.Advanced.DeadEnd Context Problem)


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type Context
    = Context_TODO


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
    | UnknownValue { start : ( Int, Int ), end : ( Int, Int ) }
    | ExpectingEndOfFile


parse : String -> Result (List Error) Toml
parse source =
    Parser.Advanced.run tomlParser source
        |> Result.mapError (List.map (problemToError source))


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


tomlParser : Parser Toml
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
    | KeyValueExpr ( Key, Value ) (Maybe String)
    | TableExpr Table (Maybe String)


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


keyValueParser : Parser ( Key, Value )
keyValueParser =
    Parser.Advanced.succeed Tuple.pair
        |. spacesParser
        |= dottedkeyParser
        |. spacesParser
        |. Parser.Advanced.symbol (Parser.Advanced.Token "=" ExpectingKeyValueSeparator)
        |. spacesParser
        |= valueParser


dottedkeyParser : Parser Key
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
            |> Parser.Advanced.map (\key -> "\"" ++ key ++ "\"")
        , literalStringParser
            |> Parser.Advanced.map (\key -> "'" ++ key ++ "'")
        ]



-- VALUES


valueParser : Parser Value
valueParser =
    Parser.Advanced.succeed identity
        |= Parser.Advanced.oneOf valueParsers
        |. spacesParser


valueParsers : List (Parser Value)
valueParsers =
    [ numberParser
    , booleanParser
    , stringParser
    , arrayParser
    ]



-- STRINGS


stringParser : Parser Value
stringParser =
    Parser.Advanced.succeed String
        |= Parser.Advanced.oneOf
            [ basicStringParser
            , multilineBasicStringParser
            , literalStringParser
            , multilineLiteralStringParser
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


numberParser : Parser Value
numberParser =
    Parser.Advanced.oneOf
        [ floatParser
        , integerParser
        ]


integerParser : Parser Value
integerParser =
    Parser.Advanced.succeed
        (\shouldNegate value ->
            Integer
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


floatParser : Parser Value
floatParser =
    Parser.Advanced.succeed
        (\shouldNegate value ->
            case value of
                Err i ->
                    Integer
                        (if shouldNegate then
                            negate i

                         else
                            i
                        )

                Ok f ->
                    Float
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


booleanParser : Parser Value
booleanParser =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Boolean True)
            |. Parser.Advanced.token (Parser.Advanced.Token "true" ExpectingTrue)
        , Parser.Advanced.succeed (Boolean False)
            |. Parser.Advanced.token (Parser.Advanced.Token "false" ExpectingFalse)
        ]



-- TIME
-- ARRAY


arrayParser : Parser Value
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
                            |> Array
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
    = LookingForEndOrValue (List Value)
    | LookingForEndOrSeprator (List Value)
    | Complete (List Value)
    | ArrayError



-- TABLE


type Table
    = StandardTable Key (List ( Key, Value ))
    | ArrayTable Key (List ( Key, Value ))


tableParser : Parser Table
tableParser =
    Parser.Advanced.oneOf
        [ arrayTableParser
        , standardTableParser
        ]


arrayTableParser : Parser Table
arrayTableParser =
    Parser.Advanced.succeed ArrayTable
        |. Parser.Advanced.token (Parser.Advanced.Token "[[" ExpectingArrayTableStart)
        |= dottedkeyParser
        |. Parser.Advanced.token (Parser.Advanced.Token "]]" ExpectingArrayTableEnd)
        |. spacesParser
        |. newLineParser
        |= Parser.Advanced.loop [] tableValuesParser


standardTableParser : Parser Table
standardTableParser =
    Parser.Advanced.succeed StandardTable
        |. Parser.Advanced.token (Parser.Advanced.Token "[" ExpectingStandardTableStart)
        |= dottedkeyParser
        |. Parser.Advanced.token (Parser.Advanced.Token "]" ExpectingStandardTableEnd)
        |. spacesParser
        |. newLineParser
        |= Parser.Advanced.loop [] tableValuesParser


tableValuesParser : List ( Key, Value ) -> Parser (Parser.Advanced.Step (List ( Key, Value )) (List ( Key, Value )))
tableValuesParser revValues =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (\value -> Parser.Advanced.Loop (value :: revValues))
            |= keyValueParser
        , Parser.Advanced.succeed (Parser.Advanced.Done (List.reverse revValues))
        ]
