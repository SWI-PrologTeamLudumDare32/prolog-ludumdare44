:- module(adventure_larkc_client, [larkc_asserta/1,
				   larkc_retract/1]).

:- use_module(library(http/http_session)).

:- use_module(larkc_client).
:- use_module(larkc_client_eval_wrappers).
:- use_module(library(pengines)).

:- consult('nanisearch_helper').

:- writeln('be sure to run `nanisearch_helper:loadNaniSearchIntoLarKC(user_43).` after larkc_server.pl is finished loading').

getMt(Mt) :-
	Mt = 'LD44-user_43-Mt'.
getMt(Mt) :-
	pengine_self(Session),
	atomic_list_concat(['LD44-user_', Session, '-Mt'], Mt).
getMt(_) :-
	throw(error(resource_error(not_in_session), context(larkc_nanisearch_helper:getMt/1,
						      'must have an http session to assert/retract'))).

larkc_asserta(X) :-
	getMt(Mt),
	writeln(asserta(X)),
	X =.. List,
	cycAssert(List,Mt,Result),
	assert(adventure:X),
	writeln(Result).

larkc_retract(X) :-
	getMt(Mt),
	findall(X,adventure:X,Is),
	member(Item,Is),
	writeln(retract(Item)),
	Item =.. List,
	cycUnassert(List,Mt,Result),
	retract(adventure:Item),
	writeln(Result).
