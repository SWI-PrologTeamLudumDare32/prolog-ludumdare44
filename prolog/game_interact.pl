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
:- ensure_loaded(adventure).
:- use_module(parser).

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
	print_term(rawRequest(RawRequest),[]),
	game_turn_(RawRequest, Response).

game_turn_(Request, Response) :-
	string_codes(Request, Codes),
	parse(Codes, Term),
	with_output_to(atom(Response),
		       adventure:Term),
	!.

sandbox:safe_primitive(game_interact:game_turn(_, _)).
















