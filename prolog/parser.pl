:- module(parser, [parse/2]).
/** <module> Parse the user's input into a prolog term
  *
*/

:- use_module(library(tokenize)).
:- use_module(library(porter_stem)).
:- use_module(library(dcg/basics)).
:- ensure_loaded(adventure).

parse(Codes, Term) :-
	tokenize(Codes, Tokens, [case(false),spaces(false), cntrl(false), to(atoms), pack(false)] ),
	!, % tokenize leaves choice points
	normalize_tokens(Tokens, NormTokens),
	!, % so does normalize_tokens.
phrase(adventure_input(Term), NormTokens).

normalize_tokens([], []).
normalize_tokens([word(W)|T], NT) :-
	porter_stem_and_adjust(W, Stem),
	member(Stem, [a, an, the, of, for]),
	normalize_tokens(T, NT).
normalize_tokens([word(W)|T], [Stem|NT]) :-
	porter_stem_and_adjust(W, Stem),
	normalize_tokens(T, NT).
normalize_tokens([_|T], NT) :-
	normalize_tokens(T, NT).

porter_stem_and_adjust(W, Stem) :-
	porter_stem(W,TmpStem),
	(   substitute(TmpStem,Stem) -> true ; Stem = TmpStem).

substitute(offic,office).
substitute(diningroom,diningRoom).
substitute(washingmachin,washingMachine).
substitute(envelop,envelope).
substitute(cracker,crackers).
substitute(appl,apple).
substitute(brocoll,brocolli).
substitute(comput,computer).

adventure_input(X) -->
	... ,
	command(X),
	... ,
	!.
adventure_input(error_input) -->
	...,
	!.

... --> [].
... --> [_], ... .

command(look_in(X)) -->
	[look,in],
	thing(X).
command(look) --> [look].
command(goto(Place)) -->
        (   [g] |  [go] | [go, to] | [goto] | [visit] | [return, to]),
        place(Place).
command(move(Place)) -->
	place(Place).
command(take(Thing)) -->
	[take],
	thing(Thing).
command(put(Thing)) -->
	(   [put] | [drop] | [leave] ),
	thing(Thing).
command(turn_on(X)) -->
	(   [turn, on]
	|   [switch, on]
	),
	device(X).
command(turn_off(X)) -->
	(   [turn, off]
	|   [switch, off]
	),
	device(X).
command(inventory) -->
	[i] |
	[inv] |
	[invent] |
	[inventory].
command(util:assert(ld44Flag(debug))) -->
	[debug,on].
command(util:retractall(ld44Flag(debug))) -->
	[debug,off].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	     


place(X) -->
	[X],
	{ hold(isa(X, room))}.

thing(X) -->
	[X],
	{ hold(isa(X, naniObject)) }.

device(X) -->
	[X],
	{ hold(isa(X, device))}.
