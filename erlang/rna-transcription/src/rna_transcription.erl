-module(rna_transcription).

-export([to_rna/1]).

to_rna([]) -> [];
to_rna([H | T]) -> [dna_to_rna(H) | to_rna(T)].

dna_to_rna($G) -> $C;
dna_to_rna($C) -> $G;
dna_to_rna($T) -> $A;
dna_to_rna($A) -> $U.
