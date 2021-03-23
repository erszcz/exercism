-module(nucleotide_count).

-export([count/2, nucleotide_counts/1]).


count(Strand, Nucleotide) ->
    lists:member(Nucleotide, ["A", "C", "G", "T"])
        orelse erlang:error(invalid_nucleotide, [Strand, Nucleotide]),
    case lists:keyfind(Nucleotide, 1, nucleotide_counts(Strand)) of
        {Nucleotide, N} -> N;
        false -> 0
    end.

nucleotide_counts(Strand) ->
    Counts = lists:foldr(fun (N, Acc) ->
                                 maps:update_with([N], fun (C) -> C+1 end, Acc)
                         end, #{"A" => 0, "C" => 0, "G" => 0, "T" => 0}, Strand),
    maps:to_list(Counts).
