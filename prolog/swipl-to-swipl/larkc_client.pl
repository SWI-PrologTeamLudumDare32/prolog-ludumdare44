:- use_module(library(pengines)).
:- use_module(library(statistics)).

larkcCall(Callable) :-
	pengine_rpc('http://localhost:9880',Callable).

clEval(SubL,Result) :-
	larkcCall(larkc_api:my_cl_eval(SubL,Result)).

larkcClCall(SubL,Result) :-
	pengine_rpc('http://localhost:9880',my_cl_eval(SubL,Result)).

:- consult('larkc_client_code').
:- consult('larkc_client_eval_wrappers').

:- consult('t/larkc_client_tests').
:- consult('t/larkc_client_original_tests').
