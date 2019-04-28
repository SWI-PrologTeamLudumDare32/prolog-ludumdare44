:- module(larkc_hold,[
		      larkc_hold/1
		     ]).

:- use_module(library(pengines)).
:- use_module(nanisearch_helper).
:- use_module(larkc_client_eval_wrappers).

% larkc_hold(Mt,here(X)) and generate clEval(['ASK-TEMPLATE',[quote,'?X'],[quote,[here,'?X']],[quote,Mt]]).

%% larkc_hold(X) :-
%% 	pengine_self(Session),
%% 	writeln([session(Session)]),
%%         getMicrotheoryFromSessionID(Session,Mt),
%% 	writeln([mt(Mt)]),
%% 	writeln(hi).

	%% X =.. List,
	%% %% writeln([list(List)]),
	%% cycQuery([here,'?X'],Mt,Result),
	%% %% writeln([larkc_hold,Result]),
	%% findall(here(Res),member(Res,Result),Assertions),
	%% member(X,Assertions).


larkc_hold(Prolog):-
	pengine_self(Session),
	getMicrotheoryFromSessionID(Session,Mt),
	larkc_hold(Mt,Prolog).

larkc_hold(Mt,Prolog):-
	ISTPROLOG = ist(Mt,Prolog),
	into_cycl_form(ISTPROLOG,ISTFORM),
	writeln([istForm,ISTFORM]),
	harden_cycl_vars(ISTFORM,ISTLISP),
	assertion(ISTFORM = ['ist',_HLMt,_ELAsk]),
	cl_quote(Q),
	writeln([q,Q]),
	writeln(clEval(['CYC:ASK-TEMPLATE',[Q,ISTLISP],[Q,ISTLISP],'EverythingPSC'],Result)).
	%%  clEval(['CYC:ASK-TEMPLATE',[Q,ISTLISP],[Q,ISTLISP],'EverythingPSC'],Result),
	%% member(Result,ResultL),
	%% unify_cycl_form(Result,ISTFORM).

cl_quote('CL:QUOTE') :- !.
cl_quote('\'').	% '


harden_cycl_vars(G,G):- ground(G),!.
harden_cycl_vars(V,N):- var(V),var_property(V,name(VN)),!,atom_concat('?',VN,N).
harden_cycl_vars(V,N):- var(V),!,format(atom(N),'?~w',[V]).
harden_cycl_vars(G,G):- \+ compound(G),!.
harden_cycl_vars(A,AA):- compound_name_arguments(A,F,AL),maplist(harden_cycl_vars,AL,AAL),compound_name_arguments(AA,F,AAL).

cycl_2:-fail.

into_cycl_form(A,AA):- var(A),!,A=AA.
into_cycl_form(',','and'):- !.
into_cycl_form(';','or'):- !.
into_cycl_form(A,AA):- atomic(A),!,A=AA.
into_cycl_form((A:-B),'sentenceImplies'(BB,AA)):- cycl_2, !, into_cycl_form(A,AA),into_cycl_form(B,BB).
into_cycl_form((A:-B),'implies'(BB,AA)):- !, into_cycl_form(A,AA),into_cycl_form(B,BB).
into_cycl_form(M:P,AA):- \+ atomic(P),!,into_cycl_form(ist(M,P),AA).
into_cycl_form(A,AA):- A=..AL, maplist(into_cycl_form,AL,AA).



unify_cycl_form(Binding,Binding):- (var(Binding);number(Binding)),!.
unify_cycl_form(string(B),string(B)):-!.
unify_cycl_form(Binding,BindingP):-atom(Binding),atom_concat('#$',BindingP,Binding),!.
unify_cycl_form(nart(B),nart(BB)):-unify_cycl_form(B,BB),!.
unify_cycl_form(nart(B),(BB)):-!,unify_cycl_form(B,BB),!.
%unify_cycl_form(string(B),List):-atomSplit(List,B),!.
unify_cycl_form(string(B),B):-!.
unify_cycl_form(string([]),""):-!.
unify_cycl_form(quote(B),BO):-!,unify_cycl_form(B,BO).
unify_cycl_form([A|L],Binding):-unify_cycl_formCons(A,L,Binding),!.
unify_cycl_form(Binding,Binding):-!.

unify_cycl_formCons(A,L,[A|L]):- (var(A);var(L);A=string(_);number(A)),!.
% unify_cycl_formCons('and-also',L,Binding):-unify_cycl_formS(L,LO), list_to_conj(LO,Binding),!.
% unify_cycl_formCons('eval',L,Binding):-unify_cycl_formS(L,LO), list_to_conj(LO,Binding),!.
% unify_cycl_formCons('#$and-also',L,Binding):-unify_cycl_formS(L,LO), list_to_conj(LO,Binding),!.
unify_cycl_formCons(A,L,Binding):-
	unify_cycl_form(A,AO),
	unify_cycl_formCons(A,AO,L,Binding).
unify_cycl_formCons(A,AO,L,Binding):-
	atom(AO),!,
	unify_cycl_formS(L,LO),
	Binding=..[AO|LO],!.
unify_cycl_formCons(A,AO,L,Binding):-
	unify_cycl_formS(L,LO),
	Binding=[AO|LO],!.

unify_cycl_formS(Binding,Binding):- (var(Binding);atom(Binding);number(Binding)),!.
unify_cycl_formS([],[]).
unify_cycl_formS([V,[L]|M],[LL|ML]):- nonvar(V), cl_quote(V),unify_cycl_formS(L,LL),unify_cycl_formS(M,ML).
unify_cycl_formS([A|L],[AA|LL]):-unify_cycl_form(A,AA),unify_cycl_formS(L,LL).
