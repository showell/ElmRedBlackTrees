module ListUtil exposing (combineEqual)


combineEqual : (b -> b -> Bool) -> List ( a, b ) -> List ( List a, b )
combineEqual eq tups =
    {--
        For tups of form (a, b) combine a's into a list of
        a's such that every element in the list gets an equivalent
        b, and grab the first matching b.
    --}
    let
        accum : ( a, b ) -> List ( List a, b ) -> List ( List a, b )
        accum ( k, v ) acc =
            case acc of
                [] ->
                    [ ( [ k ], v ) ]

                head :: rest ->
                    let
                        ( keys, vHead ) =
                            head
                    in
                    if eq vHead v then
                        ( k :: keys, v ) :: rest

                    else
                        head :: accum ( k, v ) rest
    in
    List.foldl accum [] tups
