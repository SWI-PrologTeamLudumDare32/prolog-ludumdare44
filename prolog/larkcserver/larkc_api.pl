:- module(larkc_api, [do_larkc_server/2]).


:- set_setting(pengines:slave_limit, -1).
:- set_setting(pengines:allow_from, ['127.0.0.1', localhost]).

% simulated API
% if you change/make more, also change export and the
% safe_primitive definition
do_larkc_server(A, B) :-
    debug(larkc, 'saw larkc_server call ~w ~w', [A,B]),
    setup_call_cleanup(
        open('foo.txt', write, Stream),
        writeln(Stream, 'test output'),
        close(Stream)
    ).


:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(larkc_api:do_larkc_server(_, _)).
