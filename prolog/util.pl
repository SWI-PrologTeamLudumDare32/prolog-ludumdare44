:- module(util, [viewIf/1,
		 html_tab/1
		]).

%% ld44Flag(debug).
ld44Flag(nodebug).

viewIf(Item) :-
 	(   ld44Flag(debug) -> 
	    (	html_tab(3),write_term(Item,[quoted(true)]),nl) ;
	    true).

html_tab(N) :-
	foreach(between(1,N,_X),write('&nbsp;')).
