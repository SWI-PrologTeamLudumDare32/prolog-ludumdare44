getMt(Mt) :-
	Mt = 'LD44-user_43-Mt'.

larkc_asserta(X) :-
	getMt(Mt),
	writeln(asserta(X)),
	X =.. List,
	cycAssert(List,Mt,Result),
	assert(X),
	writeln(Result).

larkc_retract(X) :-
	getMt(Mt),
	findall(X,X,Is),
	member(Item,Is),
	writeln(retract(Item)),
	Item =.. List,
	cycUnassert(List,Mt,Result),
	retract(Item),
	writeln(Result).

:- consult('larkc_nanisearch_helper').