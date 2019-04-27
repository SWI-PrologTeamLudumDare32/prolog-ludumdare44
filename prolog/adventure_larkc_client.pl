:- module(adventure_larkc_client, [larkc_asserta/1,
				   larkc_retract/1]).

:- use_module(library(http/http_session)).

:- use_module(larkc_client).
:- use_module(larkc_client_eval_wrappers).

:- consult('nanisearch_helper').

:- writeln('be sure to run `loadNaniSearchIntoLarKC(SessionID).` after larkc_server.pl is finished loading').

getMt(Mt) :-
	Mt = 'LD44-user_43-Mt'.
getMt(Mt) :-
	http_in_session(Session),
	atomic_list_concat(['LD44-user_', Session, '-Mt'], Mt).
getMt(_) :-
	throw(error(resource_error(not_in_session), context(larkc_nanisearch_helper:getMt/1,
						      'must have an http session to assert/retract'))).

/*
larkc_asserta(X) :-
	assert(adventure:X).

larkc_retract(X) :-
	retract(adventure:X).
*/

larkc_asserta(X) :-
	getMt(Mt),
	writeln(asserta(adventure:X)),
	X =.. List,
	cycAssert(List,Mt,Result),
	assert(X),
	writeln(Result).

larkc_retract(X) :-
	getMt(Mt),
	findall(X,adventure:X,Is),
	member(Item,Is),
	writeln(retract(adventure:Item)),
	Item =.. List,
	cycUnassert(List,Mt,Result),
	retract(Item),
	writeln(Result).
