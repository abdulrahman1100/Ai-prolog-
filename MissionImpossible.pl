% initial situation 
:-include("KB.pl").

% Goal Test
goal_test(S):-
	ethanAxiom(location(X,Y),S,C,[]),submarine(X,Y),capacity(C).
	
% locations of imf members 	
locIMF1(X,Y):-members_loc([[X,Y],[_,_]]).
locIMF2(X,Y):-members_loc([[_,_],[X,Y]]).


% Ethan axioms 

ethanAxiom(location(X,Y),s0,C,Acc):-
    ethan_loc(X,Y),capacity(C),members_loc(Acc).

ethanAxiom(location(X,Y),result(A,S),C,Acc):-
	  ethanAxiom(location(X0,Y0),S,C0,Acc0)
	 ,((A=right,X is X0,Y is Y0+1,C is C0,Acc = Acc0)
	 ;(A=left,X is X0,Y is Y0-1,C is C0,Acc = Acc0)
	 ;(A=up,Y is Y0,X is X0-1,C is C0,Acc = Acc0)
	 ;(A=down,Y is Y0,X is X0+1,C is C0,Acc = Acc0)
	 ;(A=carry,X is X0 , Y is Y0, members_loc(L),member([X,Y],L),C is C0-1,C0 > 0,member([X,Y],Acc0),delete(Acc0,[X,Y],Acc))
	 ;(A=drop, X is X0, Y is Y0,is_contatin_submarine(location(X,Y)),capacity(C),Acc = Acc0))
	 ,in_grid(location(X,Y)).
	
	
% AXioms constraints 

% true if the next location is in the grid 
in_grid(location(X,Y)):- X<4,Y<4,X>=0,Y>=0. %true if the cell inside the grid 

% true if the cell is not contained by any IMF or a submarine.
cell_is_empty(location(X,Y)):-
	\+is_contatin_imf(location(X,Y)),\+is_contatin_submarine(location(X,Y)). 
	
% true if the cell contains any of the imf 	
is_contatin_imf(location(X,Y)):-	
	locIMF1(X,Y);locIMF2(X,Y).
	
	
% true if the cell contains the submarine 
is_contatin_submarine(location(X,Y)):-
	submarine(X,Y).	


	