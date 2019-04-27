%% test1 :-
%% 	pengine_rpc('http://localhost:9880', member(X, [a,b,c])).

%% test2 :-
%% 	pengine_rpc('http://localhost:9880', member(X, [a,b,c]),[application(larkc_pengine_app)]).

%% test3 :-
%% 	pengine_rpc('http://localhost:9880', foo(blah),[application(larkc_pengine_app)]).

%% test4(Input) :-
%% 	pengine_rpc('http://localhost:9880', foo(Input),[application(larkc_pengine_app)]).

%% test5 :-
%% 	pengine_rpc('http://localhost:9880', member(A,[1,2,3]),[application(larkc_pengine_app)]).

%% benchmark :-
%% 	time(foreach(between(1,1000,N),test5)).

