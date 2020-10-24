:- ['A1_metroDB.P'].
:- use_module(library(pairs)).
:- table hop/2.

% Check if X and Y are neighbors in list(are they adjacent)
neighbor(X, Y, [X, Y|_]).
neighbor(X, Y, [Y, X|_]).
neighbor(X, Y, [_|L]) :- neighbor(X, Y, L).

% Returns colour of line
get_colour('blueLine', 'blue').
get_colour('yellowLine', 'yellow').
get_colour('redLine', 'red').
get_colour('magentaLine', 'magenta').
get_colour('pinkLine', 'pink').
get_colour('orangeLine', 'orange').
get_colour('greyLine', 'grey').
get_colour('greenLine', 'green').
get_colour('greenbranchLine', 'greenbranch').
get_colour('violetLine', 'violet').
get_colour('bluebranchLine', 'bluebranch').

% Preprocess the given data to form (colour, line) pairs
preprocess([], [], _). 
preprocess([X|L1], [(Z, X)|L2], Z) :-
    preprocess(L1, L2, Z).

% List of all the lines
validLine(L) :- member(L, ['blueLine', 'yellowLine','greenLine', 'greenbranchLine','magentaLine', 'pinkLine', 'orangeLine', 'bluebranchLine','greyLine', 'redLine', 'violetLine']).

% List of stations in a particular line L
list_of_stations(X, L) :- 
    call(X, Ls),
    get_colour(X, Y),
    preprocess(Ls, L, Y).

% If X and Y are 1 hop apart in graph
hop(X, Y) :- 
    % Adjecent in line
    validLine(S),
    list_of_stations(S, L),
    neighbor(X, Y, L). 

hop((Y, X), (Z, X)) :- 
    % Junction
    get_colour(Line1, Y),
    get_colour(Line2, Z),
    validLine(Line1),
    validLine(Line2),
    Y \== Z, 
    list_of_stations(Line1, L1),
    member((Y, X), L1),
    list_of_stations(Line2, L2),
    member((Z, X), L2).

% Sort the paths found on basis of length
sort_paths(L, Lsorted):-
    map_list_to_pairs(length, L, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Lsorted).

% Returns a path between X and Y of length < N
path(X, X, [X], [X | _], N) :- N > 0.
path(X, Y, [X, Z|L], V, N):- 
    hop(X, Z),
    not(member(Z, V)),
    N > 0, 
    N1 is N - 1,
    path(Z, Y, [Z|L], [Z|V], N1).

% Finds all possible combinations of src and dest stations
paths_bounded(X, Y, L, N) :- 
    validLine(Line1), 
    validLine(Line2),
    get_colour(Line1, C1),
    get_colour(Line2, C2),
    list_of_stations(Line1, List1),
    member((C1, X), List1),
    list_of_stations(Line2, List2),
    member((C2, Y), List2), 

    % write((C1, X)),
    % nl,
    % write((C2, Y)),

    findall(T, path((C1, X), (C2, Y), T, [(C1, X)], N), L).

% Converts a list of list to list
expand([], []).
expand([[] | L1], L2) :-
    expand(L1, L2).
expand([[X|L1]|L2], [X|L3]) :- 
    % write([L1|L2]), 
    expand([L1|L2], L3).

% Find smallest Num paths 
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
        
% Select N items from L1 
take(0, _, []).
take(N, [X|L1], [X|L2]) :-
    N1 is N - 1,
    take(N1, L1, L2).

get_length([], []).
get_length([X|L1], [N|L2]) :-
    length(X, N),
    get_length(L1, L2).

% Shortest 5 paths between X and Y
paths(X, Y, L):-
    paths_helper(X, Y, AllL, 5, 1),
    % write(AllL),
    sort_paths(AllL, L1),
    length(L1, Len),
    write(Len),
    nl,
    take(5, L1, L).
