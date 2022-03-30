-module(raindrops).

-export([convert/1]).


convert(N) ->
    R = case factors(N) of
            [] ->
                integer_to_list(N);
            Factors ->
                [ case F of
                      3 -> "Pling";
                      5 -> "Plang";
                      7 -> "Plong"
                  end || F <- Factors ]
        end,
    lists:flatten(R).

factors(N) ->
    [ 3 || N rem 3 == 0 ] ++
    [ 5 || N rem 5 == 0 ] ++
    [ 7 || N rem 7 == 0 ].
