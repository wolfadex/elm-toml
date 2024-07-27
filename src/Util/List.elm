module Util.List exposing
    ( UpdateOrContinue(..)
    , combineErrs
    , foldlResult
    , updateOrPush
    )


foldlResult : (a -> b -> Result e b) -> Result e b -> List a -> Result e b
foldlResult fn init list =
    case init of
        Err e ->
            init

        Ok b ->
            case list of
                [] ->
                    init

                a :: rest ->
                    foldlResult fn (fn a b) rest


combineErrs : List (Result e a) -> Result (List e) (List a)
combineErrs =
    List.foldr
        (\next acc ->
            case ( next, acc ) of
                ( Ok a, Ok ls ) ->
                    Ok (a :: ls)

                ( Ok _, Err _ ) ->
                    acc

                ( Err e, Ok _ ) ->
                    Err [ e ]

                ( Err e, Err errs ) ->
                    Err (e :: errs)
        )
        (Ok [])


type UpdateOrContinue a
    = Continue
    | Update a


updateOrPush : (a -> UpdateOrContinue a) -> a -> List a -> List a
updateOrPush fn default list =
    updateOrPushHelper fn default [] list


updateOrPushHelper : (a -> UpdateOrContinue a) -> a -> List a -> List a -> List a
updateOrPushHelper fn default acc list =
    case list of
        [] ->
            List.reverse (default :: acc)

        next :: rest ->
            case fn next of
                Update a ->
                    List.reverse acc ++ (a :: rest)

                Continue ->
                    updateOrPushHelper fn default (next :: acc) rest
