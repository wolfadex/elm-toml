module Test.Toml exposing (suite)

import Array exposing (Array)
import Dict
import Expect
import Fuzz exposing (oneOf)
import Parser.Advanced
import Test exposing (..)
import Toml


suite : Test
suite =
    describe "basics"
        [ test "a comment" <|
            \() ->
                "# This is a full-line comment"
                    |> Toml.parse
                    |> Expect.equal (Ok Dict.empty)
        , keyBasics
        , valueBasics
        , tableBasics
        ]


keyBasics : Test
keyBasics =
    describe "key basics"
        [ test "bare key" <|
            \() ->
                "carl = 5"
                    |> Toml.parse
                    |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Integer 5)))
        , test "dotted key" <|
            \() ->
                "carl.steve = 5"
                    |> Toml.parse
                    |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Table (Dict.singleton "steve" (Toml.Integer 5)))))
        , test "quoted key" <|
            \() ->
                "\"carl\" = 5"
                    |> Toml.parse
                    |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Integer 5)))
        , test "dotted mixed with spaces key" <|
            \() ->
                "\"carl.steve\" .    jerry . THOMAS-_12345678dfghjklzxcvbnm = 5"
                    |> Toml.parse
                    |> Expect.equal
                        (Ok
                            (Dict.singleton "carl.steve"
                                (Toml.Table
                                    (Dict.singleton "jerry"
                                        (Toml.Table
                                            (Dict.singleton "THOMAS-_12345678dfghjklzxcvbnm" (Toml.Integer 5))
                                        )
                                    )
                                )
                            )
                        )
        , test "all valid chars key" <|
            \() ->
                "0123456789-_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ = 5"
                    |> Toml.parse
                    |> Expect.equal (Ok (Dict.singleton "0123456789-_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" (Toml.Integer 5)))
        ]


valueBasics : Test
valueBasics =
    describe "value basics"
        [ -- Integer
          describe "integers"
            [ fuzz Fuzz.int "any" <|
                \i ->
                    ("carl = " ++ String.fromInt i)
                        |> Toml.parse
                        |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Integer i)))
            , fuzz (Fuzz.intAtLeast 0) "positive" <|
                \i ->
                    ("carl = +" ++ String.fromInt i)
                        |> Toml.parse
                        |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Integer i)))
            , fuzz (Fuzz.intAtMost -1) "negative" <|
                \i ->
                    ("carl = " ++ String.fromInt i)
                        |> Toml.parse
                        |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Integer i)))
            ]

        -- Float
        , describe "floats"
            [ skip <|
                fuzz Fuzz.float "any" <|
                    \f ->
                        let
                            parsed =
                                "carl = "
                                    ++ formatGeneratedFloat f
                                    |> Toml.parse
                        in
                        if isNaN f then
                            case parsed of
                                Ok toml ->
                                    case Dict.get "carl" toml of
                                        Just (Toml.Float parsedF) ->
                                            if isNaN parsedF then
                                                Expect.pass

                                            else
                                                Expect.fail ("Should have parsed as NaN but parsed as " ++ String.fromFloat parsedF)

                                        Just _ ->
                                            Expect.fail "Expected to parse as a Float"

                                        Nothing ->
                                            Expect.fail "Expected to have a Dict with a key 'carl'"

                                Err err ->
                                    Expect.fail "Failed to parse"

                        else
                            parsed
                                -- Failures:
                                --
                                -- --seed 81502190583721
                                -- --seed 163497739302199
                                |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Float f)))
            , skip <|
                fuzz (Fuzz.floatAtLeast 0) "positive" <|
                    \f ->
                        ("carl = "
                            ++ (if f >= 0 then
                                    "+"

                                else
                                    ""
                               )
                            ++ formatGeneratedFloat f
                        )
                            |> Toml.parse
                            |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Float f)))
            , skip <|
                fuzz (Fuzz.floatAtMost 0) "negative" <|
                    \f ->
                        ("carl = " ++ formatGeneratedFloat f)
                            |> Toml.parse
                            |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Float f)))
            ]

        -- Boolean
        , fuzz Fuzz.bool "boolean" <|
            \b ->
                ("carl = " ++ formatGeneratedBool b)
                    |> Toml.parse
                    |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Boolean b)))

        -- Date Times
        , rfc3339DateTimes

        -- String
        , describe "strings"
            [ test "basic" <|
                \() ->
                    "carl = \"Some werds go here!\""
                        |> Toml.parse
                        |> Expect.equal (Ok (Dict.singleton "carl" (Toml.String "Some werds go here!")))
            , test "basic multi-line" <|
                \() ->
                    """carl = \"\"\"


Some werds        \\


go here!\"\"\""""
                        |> Toml.parse
                        |> Expect.equal (Ok (Dict.singleton "carl" (Toml.String """

Some werds        go here!""")))
            , test "literal" <|
                \() ->
                    "carl = 'Some werds go here!'"
                        |> Toml.parse
                        |> Expect.equal (Ok (Dict.singleton "carl" (Toml.String "Some werds go here!")))
            , test "literal multi-line" <|
                \() ->
                    """carl = '''

Some werds   \\
    go here!


'''
"""
                        |> Toml.parse
                        |> Expect.equal (Ok (Dict.singleton "carl" (Toml.String """
Some werds   \\
    go here!


""")))
            ]
        , describe "arrays"
            [ test "empty" <|
                \() ->
                    "carl = []"
                        |> Toml.parse
                        |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Array Array.empty)))
            , test "single value" <|
                \() ->
                    "carl = [5]"
                        |> Toml.parse
                        |> Expect.equal (Ok (Dict.singleton "carl" (Toml.Array (Array.fromList [ Toml.Integer 5 ]))))
            , test "multi-value" <|
                \() ->
                    "carl = [5, \t\t\t'carl', true]"
                        |> Toml.parse
                        |> Expect.equal
                            (Ok
                                (Dict.singleton "carl"
                                    (Toml.Array
                                        (Array.fromList
                                            [ Toml.Integer 5
                                            , Toml.String "carl"
                                            , Toml.Boolean True
                                            ]
                                        )
                                    )
                                )
                            )
            , test "trailing separator" <|
                \() ->
                    "carl = [5   \n,\n 'carl', \n]"
                        |> Toml.parse
                        |> Expect.equal
                            (Ok
                                (Dict.singleton "carl"
                                    (Toml.Array
                                        (Array.fromList
                                            [ Toml.Integer 5
                                            , Toml.String "carl"
                                            ]
                                        )
                                    )
                                )
                            )
            ]
        , describe "inline tables"
            [ test "empty" <|
                \() ->
                    "carl = {}"
                        |> Toml.parse
                        |> Expect.equal
                            (Ok
                                (Dict.singleton "carl"
                                    (Toml.Table
                                        Dict.empty
                                    )
                                )
                            )
            , test "single key/value" <|
                \() ->
                    "carl = { steve = 5 }"
                        |> Toml.parse
                        |> Expect.equal
                            (Ok
                                (Dict.singleton "carl"
                                    (Toml.Table
                                        (Dict.singleton "steve" (Toml.Integer 5))
                                    )
                                )
                            )
            , test "multi-key/value" <|
                \() ->
                    "carl = { steve = 5, \t\t\t'carl' = 'jenny', yes.no = true }"
                        |> Toml.parse
                        |> Expect.equal
                            (Ok
                                (Dict.singleton "carl"
                                    (Toml.Table
                                        (Dict.fromList
                                            [ ( "steve", Toml.Integer 5 )
                                            , ( "carl", Toml.String "jenny" )
                                            , ( "yes", Toml.Table (Dict.singleton "no" (Toml.Boolean True)) )
                                            ]
                                        )
                                    )
                                )
                            )
            ]
        ]


tableBasics : Test
tableBasics =
    describe "table basics"
        [ describe "standard table"
            [ test "minimal" <|
                \() ->
                    "[minimal]"
                        |> Toml.parse
                        |> Expect.equal (Ok (Dict.singleton "minimal" (Toml.Table Dict.empty)))
            , test "empty" <|
                \() ->
                    "[empty.table]"
                        |> Toml.parse
                        |> Expect.equal
                            (Ok
                                (Dict.singleton "empty"
                                    (Toml.Table
                                        (Dict.singleton "table" (Toml.Table Dict.empty))
                                    )
                                )
                            )
            , test "non-empty" <|
                \() ->
                    """[a.'table']
carl = 5
steve = false
"""
                        |> Toml.parse
                        |> Expect.equal
                            (Ok
                                (Dict.singleton "a"
                                    (Toml.Table
                                        (Dict.singleton "table"
                                            (Toml.Table
                                                (Dict.fromList
                                                    [ ( "carl", Toml.Integer 5 )
                                                    , ( "steve", Toml.Boolean False )
                                                    ]
                                                )
                                            )
                                        )
                                    )
                                )
                            )
            ]
        , describe "array of tables"
            [ only <|
                test "empty" <|
                    \() ->
                        "[[empty]]"
                            |> Toml.parse
                            |> Expect.equal
                                (Ok
                                    (Dict.singleton "empty"
                                        (Toml.Table
                                            Dict.empty
                                        )
                                    )
                                )
            , test "non-empty" <|
                \() ->
                    """[[a.'table']]
carl = 5
steve = false
"""
                        |> Toml.parse
                        |> Expect.ok
            , test "multi-entry" <|
                \() ->
                    """[[arrTable.first]]
value = 5

[[arrTable.second]]

value = false
"""
                        |> Toml.parse
                        |> Expect.ok
            ]
        ]



--


formatGeneratedFloat : Float -> String
formatGeneratedFloat f =
    if f == (1 / 0) then
        "inf"

    else if f == (-1 / 0) then
        "-inf"

    else if isNaN f then
        "nan"

    else
        let
            fStr =
                String.fromFloat f
        in
        if String.contains "." fStr || String.contains "e" fStr then
            fStr

        else
            fStr ++ ".0"


formatGeneratedBool : Bool -> String
formatGeneratedBool b =
    if b then
        "true"

    else
        "false"


rfc3339DateTimes : Test
rfc3339DateTimes =
    describe "dates and times"
        [ localTimes
        , localDates
        , localDateTimes
        , offsetDateTimes
        ]


localTimes : Test
localTimes =
    describe "local times"
        [ test "midnight" <|
            \() ->
                "carl = 00:00:00"
                    |> Toml.parse
                    |> Expect.ok
        , test "right after midnight" <|
            \() ->
                "carl = 00:00:00.00001"
                    |> Toml.parse
                    |> Expect.ok
        ]


localDates : Test
localDates =
    describe "local dates"
        [ test "the beginning of 'time'" <|
            \() ->
                "carl = 1970-01-01"
                    |> Toml.parse
                    |> Expect.ok
        , test "leap  day" <|
            \() ->
                "carl = 1988-02-29"
                    |> Toml.parse
                    |> Expect.ok
        ]


localDateTimes : Test
localDateTimes =
    describe "local date times"
        [ test "the beginning of 'time'" <|
            \() ->
                "carl = 1970-01-01T00:00:00"
                    |> Toml.parse
                    |> Expect.ok
        , test "lower 't' seprator" <|
            \() ->
                "carl = 1970-01-01t00:00:00"
                    |> Toml.parse
                    |> Expect.ok
        , test "space separator" <|
            \() ->
                "carl = 1970-01-01 00:00:00"
                    |> Toml.parse
                    |> Expect.ok
        ]


offsetDateTimes : Test
offsetDateTimes =
    describe "offset date times"
        [ test "the beginning of 'time'" <|
            \() ->
                "carl = 1970-01-01T00:00:00+00:00"
                    |> Toml.parse
                    |> Expect.ok
        , test "also the beginning" <|
            \() ->
                "carl = 1970-01-01T00:00:00Z"
                    |> Toml.parse
                    |> Expect.ok
        , test "negative offset" <|
            \() ->
                "carl = 1970-01-01T00:00:00-12:34"
                    |> Toml.parse
                    |> Expect.ok
        ]
