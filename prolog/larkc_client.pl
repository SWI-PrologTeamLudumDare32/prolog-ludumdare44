:- module(larkc_client, [
	      clEval/2,
	      larkcClCall/2]).

:- use_module(library(pengines)).
:- use_module(library(statistics)).
:- use_module(library(hostname)).

larkcCall(Callable) :-
	(   hostname(ai) -> pengine_rpc('http://127.0.0.1:9880',Callable) ; pengine_rpc('http://partyserver.rocks:9880',Callable)).


clEval(SubL,Result) :-
	writeln([subL,SubL]),
	larkcCall(larkc_api:my_cl_eval(SubL,Result)).

larkcClCall(SubL,Result) :-
	(   hostname(ai) -> pengine_rpc('http://127.0.0.1:9880',my_cl_eval(SubL,Result)) ; pengine_rpc('http://partyserver.rocks:9880',my_cl_eval(SubL,Result))).

/*
:- consult('larkc_client_code').
:- consult('larkc_client_eval_wrappers').

:- consult('t/larkc_client_tests').
:- consult('t/larkc_client_original_tests').
*/
