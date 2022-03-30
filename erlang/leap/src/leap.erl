-module(leap).

-export([leap_year/1]).


leap_year(Year) ->
    case {Year rem 4, Year rem 100, Year rem 400} of
        {0, _, 0} -> true;
        {0, 0, _} -> false;
        {0, _, _} -> true;
        _ -> false
    end.
