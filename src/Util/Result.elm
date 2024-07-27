module Util.Result exposing (combine)


combine : List (Result e a) -> ( List a, List e )
combine =
    List.foldr
        (\next ( oks, errs ) ->
            case next of
                Ok a ->
                    ( a :: oks, errs )

                Err e ->
                    ( oks, e :: errs )
        )
        ( [], [] )
