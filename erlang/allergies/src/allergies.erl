-module(allergies).

-export([allergies/1, is_allergic_to/2]).

-type allergen() :: eggs
                  | peanuts
                  | shellfish
                  | strawberries
                  | tomatoes
                  | chocolate
                  | pollen
                  | cats.

-spec score(allergen()) -> integer().
score(Allergen) ->
    case Allergen of
        eggs          ->   1;
        peanuts       ->   2;
        shellfish     ->   4;
        strawberries  ->   8;
        tomatoes      ->  16;
        chocolate     ->  32;
        pollen        ->  64;
        cats          -> 128
    end.

%% This is an allergen() constructor, which guarantees "compile-time" safety in Erlang,
%% which is otherwise a dynamically typed language.
%% Check out https://github.com/josefs/Gradualizer to understand how to reap the benefits of this function.
-spec allergen(allergen()) -> allergen().
allergen(Al) ->
    case Al of
        eggs          -> Al;
        peanuts       -> Al;
        shellfish     -> Al;
        strawberries  -> Al;
        tomatoes      -> Al;
        chocolate     -> Al;
        pollen        -> Al;
        cats          -> Al
    end.

-spec allergies(_Score) -> [allergen()].
allergies(Score) ->
    %% By calling allergen/1 here we make sure - at "compile time" - our input allergen list is valid.
    Allergens = [ allergen(Al)
                  || Al <- [eggs, peanuts, shellfish, strawberries, tomatoes, chocolate, pollen, cats] ],
    [ Al || Al <- Allergens, is_allergic_to(Al, Score) ].
    

is_allergic_to(Substance, Score) ->
    Score band score(Substance) > 0.
