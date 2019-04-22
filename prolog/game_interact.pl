:- module(game_interact,
          [
	   create_game/1,
	   game_turn/2
	  ]).

:- use_module(library(pengines)).

:- dynamic current_process/4, current_location/3.

board_size(10, 10).

create_game('THE RAIN MAKES MUD PUDDLES') :-
	pengine_self(PengineID),
	current_process(PengineID, _, _, _),
	!,
	debug(ld(redundant), 'game already created', []).
create_game(Response) :-
	pengine_self(PengineID),
	thread_at_exit(kill_game(PengineID)).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(game_interact:create_game(_)).

kill_all_processes(PengineID) :-
	setof(PID, current_process(PengineID, PID, _, _), PIDS),
	maplist(process_kill, PIDS).

kill_game(PengineID) :-
	current_process(PengineID, PID, _, _),
	process_kill(PID).

game_turn(URIRawRequest, Response) :-
	www_form_encode(RawRequest, URIRawRequest),
	print_term(rawRequest(RawRequest),[]),
	game_turn_(RawRequest, Response).

game_turn_(Request, Response) :-
	atom_string(Atom,Request),
	read_term_from_atom(Atom, Term, [character_escapes(true)]),
	with_output_to(atom(Got),(call(Term) -> Response = Got ; Response = Got)).

sandbox:safe_primitive(game_interact:game_turn(_, _)).
