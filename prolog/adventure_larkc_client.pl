larkc_asserta(X) :-
	writeln(asserta(X)),
	X =.. List,
	cycAssert(List,'BaseKB',Result),
	assert(X),
	writeln(Result).

larkc_retract(X) :-
	findall(X,X,Is),
	member(Item,Is),
	writeln(retract(Item)),
	Item =.. List,
	cycUnassert(List,'BaseKB',Result),
	retract(Item),
	writeln(Result).

:- consult('larkc_nanisearch_helper').