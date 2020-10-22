:- ['A1_metroDB.P'].
:- use_module(library(pairs)).
:- table hop/2.

neighbor(X, Y, [X, Y|_]).
neighbor(X, Y, [Y, X|_]).
neighbor(X, Y, [_|L]) :- neighbor(X, Y, L).

preprocess([], [], _). 
preprocess([X|L1], [(X, Z)|L2], Z) :-
    preprocess(L1, L2, Z).

validLine(L) :- member(L, ['blueLine', 'yellowLine','greenLine', 'greenbranchLine','magentaLine', 'pinkLine', 'orangeLine', 'bluebranchLine','greyLine', 'redLine', 'violetLine']).

list_of_stations(X, L) :- 
    call(X, Ls),
    preprocess(Ls, L, X).

hop(X, Y) :- 
    validLine(S),
    list_of_stations(S, L),
    neighbor(X, Y, L). 

hop((X, Y), (X, Z)) :- 
    validLine(Y),
    validLine(Z),
    Y \== Z, 
    list_of_stations(Y, L1),
    member((X, Y), L1),
    list_of_stations(Z, L2),
    member((X, Z), L2).

sort_paths(L, Lsorted):-
    map_list_to_pairs(length, L, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Lsorted).

path(X, X, [X], [X | _], N) :- N > 0.
path(X, Y, [X, Z|L], V, N):- 
    hop(X, Z),
    not(member(Z, V)),
    N > 0, 
    N1 is N - 1,
    path(Z, Y, [Z|L], [Z|V], N1).

paths_bounded(X, Y, L, N) :- 
    validLine(Line1), 
    validLine(Line2),
    list_of_stations(Line1, List1),
    member((X, Line1), List1),
    list_of_stations(Line2, List2),
    member((Y, Line2), List2), 
    % write((X, Line1)),
    % nl,
    % write((Y, Line2)),

    findall(T, path((X, Line1), (Y, Line2), T, [(X, Line1)], N), L).

expand([], []).
expand([[] | L1], L2) :-
    expand(L1, L2).
expand([[X|L1]|L2], [X|L3]) :- 
    % write([L1|L2]), 
    expand([L1|L2], L3).

paths_helper(X, Y, L, Num, N):-
    Num > 0, 
    findall(T, paths_bounded(X, Y, T, N), Ltemp),
    expand(Ltemp, L1),
    length(L1, Len),
    % write(Len),
    N2 is N + 1,
    N3 is Num - Len,
    % write(Len),
    (
        N3 > 0 -> 
        (   
            paths_helper(X, Y, L, Num, N2), 
            !
        );
        N3 =< 0 -> L = L1
    ).
        

take(0, _, []).
take(N, [X|L1], [X|L2]) :-
    N1 is N - 1,
    take(N1, L1, L2).

paths(X, Y, L):-
    paths_helper(X, Y, AllL, 5, 1),
    % write(AllL),
    sort_paths(AllL, L1),
    length(L1, Len),
    write(Len),
    nl,
    take(5, L1, L).
