:- module(adventure_larkc_client, [larkc_asserta/1,
				   larkc_retract/1]).

:- use_module(library(http/http_session)).

:- use_module(larkc_client).
:- use_module(larkc_client_eval_wrappers).
:- use_module(library(pengines)).

:- consult('nanisearch_helper').
