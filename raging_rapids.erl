% My solution for the Raging Rapids puzzle.
% Puzzle details can be found at http://www.jaapsch.net/puzzles/rapids.htm
-module(raging_rapids).
-export([get_solution_sets/0]).

get_solution_sets() ->
    % Pieces of the puzzle. The letter uniquely identifies the piece.
    Pieces = [{c, [1, -2, -1, 2]}, {k, [1, 2, -1, 2]}, {e, [1, 2, 1, -2]},
        {i, [1, 2, 1, 2]}, {g, [1, 2, -1, -2]}, {h, [-1, -2, 1, -2]},
        {b, [-1, -2, 1, 2]}, {j, [1, -2, 1, -2]}, {a, [-1, 2, 1, -2]},
        {f, [-1, -2, -1, -2]}, {l, [-1, 2, -1, 2]}, {d, [-1, 2, -1, -2]}],
    % Board slots. Zero indicates that a piece side doesn't need to match with the board slot side.
    Board = [[1, 0, 0, 2], [1, 0, 0, 0], [1, 2, 0, 0],
        [0, 0, 0, 2], [0, 0, 0, 0], [0, -2, 0, 0],
        [0, 0, 0, 2], [0, 0, 0, 0], [0, -2, 0, 0],
        [0, 0, -1, -2], [0, 0, -1, 0], [0, -2, -1, 0]],
    % Get a solution set    
    SS = get_solution_sets(Pieces, Board),
    % Solve the puzzle
    find_soln(SS).

% Try each solution set
find_soln([H|T]) ->
    case verify(H) of 
    	true  -> H;
    	false -> find_soln(T)
    end.
    
% See if solution set S if valid
% Note that the outer sides have already been validated
verify(S) ->
	  [{_,[_, A2, A3, _]}, {_, [_, B2, B3, B4]}, {_, [_, _, C3, C4]},
	  {_, [D1, D2, D3, _]}, {_, [E1, E2, E3, E4]}, {_, [F1, _, F3, F4]},
	  {_, [G1, G2, G3, _]}, {_, [H1, H2, H3, H4]}, {_, [I1, _, I3, I4]},
	  {_, [J1, J2, _, _]}, {_, [K1, K2, _, K4]}, {_, [L1, _, _, L4]}] = S,
	  sum_is_zero([{A3, D1}, {D3, G1}, {G3, J1}, {B3, E1}, {E3, H1}, {H3, K1}, {C3, F1}, {F3, I1}, {I3, L1}, {A2, B4}, {B2, C4}, {D2, E4}, {E2, F4}, {G2, H4}, {H2, I4}, {J2, K4}, {K2, L4}]).

% sum_is_zero makes sure that all side pairs fit together
sum_is_zero([]) ->
	true;
	
sum_is_zero([{A, B}|T]) ->
	(A + B =:= 0) and sum_is_zero(T).

% Given the pieces P, determines all plausible solution sets.
% B represents the board slots and is used to determine solution set plausibility.	
get_solution_sets([], B) ->
    [[]];
    
get_solution_sets(P, B) ->
    [BH|BT] = B,
    [[H|T] || H <- P, check_piece(H, BH) =:= true, T <- get_solution_sets(P--[H], BT)].
   
% Reduces the solution set from 12! to 3920
check_piece(P, B) ->
    {_, [P1, P2, P3, P4]} = P,
    [B1, B2, B3, B4] = B,
    X1 = (B1 =:= 0) or (P1 - B1 =:= 0),    
    X2 = (B2 =:= 0) or (P2 - B2 =:= 0),
    X3 = (B3 =:= 0) or (P3 - B3 =:= 0),    
    X4 = (B4 =:= 0) or (P4 - B4 =:= 0),
    X1 and X2 and X3 and X4.

