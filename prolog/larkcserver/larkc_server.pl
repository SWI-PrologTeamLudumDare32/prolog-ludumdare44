:- module(larkc_server, [go/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_files)).
:- use_module(library(pengines)).
% :- ensure_loaded(larkc_api).

:- use_module(library(sandbox)).
:- use_module(pengine_sandbox:larkc_api).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(js, '/js', []).
http:location(css, '/css', []).
http:location(img, '/img', []).
user:file_search_path(css, './css').
user:file_search_path(js, './js').
user:file_search_path(icons, './icons').

:- html_resource(style, [virtual(true), requires([css('style.css')]), mime_type(text/css)]).
:- html_resource(script, [virtual(true), requires([js('interact.js')]), mime_type(text/javascript)]).
:- html_resource(jquery, [virtual(true), requires([js('jquery.js')]), mime_type(text/javascript)]).
:- html_resource(pengines_script, [virtual(true), requires([root('pengine/pengines.js')]), mime_type(text/javascript)]).

go :-
    http_set_session_options([timeout(60)]),
    http_server(http_dispatch, [port(9880), timeout(180)]).

:- http_handler(/, game_handler, []).

:- http_handler(js(.), http_reply_from_files('js/', []),
           [priority(1000), prefix]).
:- http_handler(css(.), http_reply_from_files('css/', []),
                [priority(1000), prefix]).
:- http_handler(img(.), http_reply_from_files('icons/', []),
                [priority(1000), prefix]).

game_handler(_Request) :-
    reply_html_page(
        [title('Psyche'),
         link([href('https://fonts.googleapis.com/css?family=IBM+Plex+Mono|VT323'), rel(stylesheet)], [])
        ]
        ,
        \minesweeper_page).

minesweeper_page -->
	html([div(id(codeliketext),
		  [\html_requires(style),
		   \html_requires(jquery),
		   \html_requires(pengines_script),
		   \html_requires(script),
		   code(b('***************  PSYCHE  ******************')),
		   code('AN INTERACTIVE-FICTION GAME USING LARKC_CL')
		  ]),
	      div(id(inputarea),
		  [
		   label(for(user), blink('\u25b6')),
		   input([type(text), name(user), id(inputbox), size(60)], [])
		  ])
	     ]).

