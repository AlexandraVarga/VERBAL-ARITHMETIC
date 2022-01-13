%
%  to run this solution you need to run the sum predicate
%  example:
%       sum([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]).
%
%  press ';' to cicle through all possible solutions


%
%   digit: predicate that checks if X is a digit.
%   this is helpful to enumerate all possible digits and
%   eliminate Arguments are not sufficiently instantiated problem.
%
digit(X) :- member(X, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]).


%   all_digit: predicate that checks if a list contains only digits
%   final case: list with one element that is a digit
%   else all elements are digits
all_digit([X]) :- digit(X).
all_digit([X | L]) :- digit(X), all_digit(L).


%  list_to_int_reversed: intermidiate predicate that converts a list of digits to a number
%  the number is reversed
list_to_int_reversed([X], X) :- digit(X).
list_to_int_reversed([D | L], X) :- digit(D), list_to_int_reversed(L, XI), X is XI * 10 + D.


% list_to_int: predicate that converts a list of digits to a number
list_to_int(L, X) :- reverse(L, LR), list_to_int_reversed(LR, X).



% sum: main predicate that solves the equation L1 + L2 = L3
sum(L1, L2, L3) :- all_digit(L1), all_digit(L2), all_digit(L3), % initiate the given list of numbers with digits
    list_to_int(L1, X1), list_to_int(L2, X2), list_to_int(L3, X3), % covert those lists into integers
    X3 is X1 + X2, % check if L3 = L1 + L2
    X1 \=0, X2\=0, X3\=0. % remove trivial solutions

solve(L1, L2, L3) :- flatten([L1, L2, L3], V), list_to_set(V, Vars), write(Vars).

% goal: predicate that checks if the goal is achieved, All variables are replaced, and W1 + W2 = W3
% this predicate loops and replaces the values of the variable in W1, W2 and W3 in order to check the correctness of W1 + W2 = W3.
goal(L1, L2, L3, Vars, State) :-
    length(Vars, N), length(State, N), length(L1, N1), length(L2, N2), length(L3, N3), length(L1R, N1), length(L2R, N2), length(L3R, N3),
    goal(L1, L2, L3, Vars, State, [L1R, L2R, L3R], N, 0).
goal(_, _, _, _, _, [L1R, L2R, L3R], N, N) :- !, list_to_int(L1R, N1), list_to_int(L2R, N2), list_to_int(L3R, N3), N3 is N1 + N2, write(L1R), nl, write(L2R), nl, write(L3R), nl, nl, nl.
goal(L1, L2, L3, Vars, State, [L1R, L2R, L3R], N, Index) :-
    Index < N, !, nth0(Index, Vars, E), nth0(Index, State, R), NIndex is Index + 1,
    replace_all(E, L1, R, L1R), replace_all(E, L2, R, L2R), replace_all(E, L3, R, L3R),
    goal(L1, L2, L3, Vars, State, [L1R, L2R, L3R], N, NIndex).

% replace_all apearances of E in L with R, the result is in LR
% This predicate don't affect L even if it contains variables
%
replace_all(E, L, R, LR) :- length(L, N), length(LR, N), replace_all(E, L, R, LR, N, 0).
replace_all(_, _, _, _, N, N) :- !.
replace_all(E, L, R, LR, N, Index) :- NIndex is Index + 1, (nth0(Index, L, X), X == E, !, nth0(Index, LR, R); true), replace_all(E, L, R, LR, N, NIndex).


% dfs: DFS Main call
dfs(Root, L1, L2, L3) :-
    flatten([L1, L2, L3], V), list_to_set(V, Vars),
    dfs([Root], [], L1, L2, L3, Vars).
%   dfs(ToVisit, Visited, Global input)

% Done, Goal achieved
dfs([H|_],_, L1, L2, L3, Vars) :- goal(L1, L2, L3, Vars, H).

%% Skip elements that are already visited
dfs([H|T],Visited, L1, L2, L3, Vars) :-
    member(H,Visited),
    dfs(T,Visited, L1, L2, L3, Vars).

%% Add unvisited neigbors of H
dfs([H|T], Visited, L1, L2, L3, Vars) :-
    not(member(H,Visited)),
    findall(Nb,move(H,Nb, L1, L2, L3, Vars),Nbs),
    append(Nbs,T, ToVisit),
    dfs(ToVisit,[H|Visited], L1, L2, L3, Vars).

% move: predicate that gives the next elements to check in the dfs
move(State, _, _, _, _, Vars) :- length(State, N), length(Vars, N), !, false.
move(State, NewState, _, _, _, _) :- digit(X), append(State, [X], NewState).
