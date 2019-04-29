:- module(game_interact,
          [
	   create_game/1,
	   game_turn/2
	  ]).
/** <module> handle interactions from the game web page to the server via pengine
 *
 * this module gets imported into the sandbox
 */
:- use_module(library(pengines)).
:- use_module(library(http/http_session)).
:- use_module(library(hostname)).

:- ensure_loaded(adventure).
:- use_module(parser).
:- use_module(util).

:- dynamic current_process/4, current_location/3.

board_size(10, 10).

create_game('THE RAIN MAKES MUD PUDDLES') :-
	pengine_self(PengineID),
	current_process(PengineID, _, _, _),
	!,
	debug(ld(redundant), 'game already created', []).
create_game(_) :-
	pengine_self(PengineID),
	init_game_state,
	thread_at_exit(kill_game(PengineID)).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(game_interact:create_game(_)).

kill_game(PengineID) :-
	current_process(PengineID, PID, _, _),
	process_kill(PID).

game_turn(URIRawRequest, Response) :-
	www_form_encode(RawRequest, URIRawRequest),
	viewIf(rawRequest(RawRequest)),
	game_turn_(RawRequest, Response).

game_turn_(Request, Response) :-
	string_codes(Request, Codes),
	parse(Codes, Term1),
	viewIf([term1,Term1]),
	with_output_to(atom(Got),
		       (   (   @(call(Term1),adventure),adventure:nanifound) ->
			   (   Response = Got) ; 
			   (   Response = Got))),
	nonvar(Response),
	!.
game_turn_(Request, Response) :-
	hostname(ai),
	atom_string(Atom,Request),
	read_term_from_atom(Atom, Term2, [character_escapes(true)]),
	with_output_to(atom(Got),
		       (   call(Term2) ->
			   (   Response = Got) ; 
			   (   Response = Got))),
	!.

sandbox:safe_primitive(game_interact:game_turn(_, _)).
	