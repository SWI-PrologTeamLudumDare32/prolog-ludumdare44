:- use_module(library(http/http_session)).

getMt(Mt) :-
	Mt = 'LD44-user_43-Mt'.
getMt(Mt) :-
	http_in_session(Session),
	atomic_list_concat(['LD44-user_', Session, '-Mt'], Mt).
getMt(_) :-
	throw(error(resource_error(not_in_session), context(larkc_nanisearch_helper:getMt/1,
						      'must have an http session to assert/retract'))).

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
