module Test.Toml exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
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
                    |> Expect.ok
        , keyBasics
        , valueBasics
        ]


keyBasics : Test
keyBasics =
    describe "key basics"
        [ test "bare key" <|
            \() ->
                "carl = 5"
                    |> Toml.parse
                    |> Expect.ok
        , test "dotted key" <|
            \() ->
                "carl.steve = 5"
                    |> Toml.parse
                    |> Expect.ok
        , test "quoted key" <|
            \() ->
                "\"carl\" = 5"
                    |> Toml.parse
                    |> Expect.ok
        , test "dotted mixed with spaces key" <|
            \() ->
                "\"carl.steve\" .    jerry . THOMAS-_12345678dfghjklzxcvbnm = 5"
                    |> Toml.parse
                    |> Expect.ok
        , test "all valid chars key" <|
            \() ->
                "0123456789-_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ = 5"
                    |> Toml.parse
                    |> Expect.ok
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
                        |> Expect.ok
            , fuzz (Fuzz.intAtLeast 0) "positive" <|
                \i ->
                    ("carl = +" ++ String.fromInt i)
                        |> Toml.parse
                        |> Expect.ok
            , fuzz (Fuzz.intAtMost -1) "negative" <|
                \i ->
                    ("carl = " ++ String.fromInt i)
                        |> Toml.parse
                        |> Expect.ok
            ]

        -- Float
        , describe "floats"
            [ fuzz Fuzz.float "any" <|
                \f ->
                    ("carl = " ++ formatGeneratedFloat f)
                        |> Toml.parse
                        |> Expect.ok
            , fuzz (Fuzz.floatAtLeast 0) "positive" <|
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
                        |> Expect.ok
            , fuzz (Fuzz.floatAtMost 0) "negative" <|
                \f ->
                    ("carl = " ++ formatGeneratedFloat f)
                        |> Toml.parse
                        |> Expect.ok
            ]

        -- Boolean
        , fuzz Fuzz.bool "boolean" <|
            \b ->
                ("carl = " ++ formatGeneratedBool b)
                    |> Toml.parse
                    |> Expect.ok

        -- String
        , describe "strings"
            [ test "basic" <|
                \() ->
                    "carl = \"Some werds go here!\""
                        |> Toml.parse
                        |> Expect.ok
            , test "basic multi-line" <|
                \() ->
                    """carl = \"\"\"


Some werds        \\


go here!\"\"\""""
                        |> Toml.parse
                        |> Expect.ok
            , test "literal" <|
                \() ->
                    "carl = 'Some werds go here!'"
                        |> Toml.parse
                        |> Expect.ok
            , test "literal multi-line" <|
                \() ->
                    """carl = '''

Some werds   \\
    go here!


'''
"""
                        |> Toml.parse
                        |> Expect.ok
            ]
        , describe "arrays"
            [ test "empty" <|
                \() ->
                    "carl = []"
                        |> Toml.parse
                        |> Expect.ok
            , test "single value" <|
                \() ->
                    "carl = [5]"
                        |> Toml.parse
                        |> Expect.ok
            , test "multi-value" <|
                \() ->
                    "carl = [5, \t\t\t'carl', true]"
                        |> Toml.parse
                        |> Expect.ok
            , test "trailing separator" <|
                \() ->
                    "carl = [5   \n,\n 'carl', \n]"
                        |> Toml.parse
                        |> Expect.ok
            ]
        ]


formatGeneratedFloat : Float -> String
formatGeneratedFloat f =
    if f == (1 / 0) then
        "inf"

    else if f == (-1 / 0) then
        "-inf"

    else if isNaN f then
        "nan"

    else
        String.fromFloat f


formatGeneratedBool : Bool -> String
formatGeneratedBool b =
    if b then
        "true"

    else
        "false"
