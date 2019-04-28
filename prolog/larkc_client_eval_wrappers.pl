:- module(larkc_client_eval_wrappers, [cycQuery/2,
				       cycQuery/3,
				       cycAssert/2,
				       cycAssert/3,
				       cycUnassert/2,
				       cycUnassert/3,
				       q/2,
				       q/3,
				       f/2,
				       allIsa/3]).

:- use_module(larkc_client).

make_quoted_string(A,QA) :-
	atomic_list_concat([
			    '"',
			    A,
			    '"'
			   ],'',QA).

cycQuery(Query,Results) :-
	cycQuery(Query,'EverythingPSC',Results).

% doesn't work right now

cycQuery(Query,Mt,Result) :-
	% convert query to modal notation
	% use dmiles code to do this
	% Goal =.. Query,
	% append([P],A,F),
	% NQuery = ['ASK-TEMPLATE',[quote,'?X'],[quote,F],Mt],
	NQuery = ['ASK-TEMPLATE',[quote,'?X'],[quote,Query],[quote,Mt]],
	write_term(clEval(NQuery,Result),[quoted(true)]),
	clEval(NQuery,Result).

% ?- cycQuery(['isa','?X','Dog'],'EverythingPSC',Result).
% Result = ['Rover-5', 'ThePrototypicalDog', 'HYP-Dog-5968235', 'Dash-VictoriasPet', ['GenericInstanceFn', 'Dog'], 'HYP-JuvenileFnDog-4981756', 'HYP-JuvenileFnDog-59300253', 'HYP-JuvenileFnDog-27104220', 'Muffet'|...].

% ?- clEval(['cyc-query',['quote',['isa','?X','Dog']],[quote,'EverythingPSC']],Result).
% Result = [[['CYC:?X'|'Rover-5']], [['CYC:?X'|'ThePrototypicalDog']], [['CYC:?X'|'HYP-Dog-5968235']], [['CYC:?X'|'Dash-VictoriasPet']], [['CYC:?X', 'GenericInstanceFn', 'Dog']], [['CYC:?X'|'HYP-JuvenileFnDog-4981756']], [['CYC:?X'|...]], [[...|...]], [...]|...].

% ?- clEval(['ask-template',[quote,'?X'],['quote',['isa','?X','Dog']],[quote,'EverythingPSC']],Result).
% Result = ['Rover-5', 'ThePrototypicalDog', 'HYP-Dog-5968235', 'Dash-VictoriasPet', ['GenericInstanceFn', 'Dog'], 'HYP-JuvenileFnDog-4981756', 'HYP-JuvenileFnDog-59300253', 'HYP-JuvenileFnDog-27104220', 'Muffet'|...].



q(isa(X,Y),Result) :-
	q(isa(X,Y),'BaseKB',Result).
q(isa(X,Y),Mt,Result) :-
	cycQuery([isa,X,Y],Mt,Result).

cycAssert(Assertion,Result) :-
	cycAssert(Assertion,'BaseKB',Result).

cycAssert(Assertion,Mt,Result) :-
	NAssert = ['cyc-assert',[quote,Assertion],[quote,Mt]],
	clEval(NAssert,Result).

cycUnassert(Assertion,Result) :-
	cycUnassert(Assertion,'BaseKB',Result).

cycUnassert(Assertion,Mt,Result) :-
	NAssert = ['cyc-unassert',[quote,Assertion],[quote,Mt]],
	clEval(NAssert,Result).

f(A,O) :-
	make_quoted_string(A,QA),
	findConstant(A,TmpO),
	(   TmpO = [] ->
	    clEval(['CREATE-CONSTANT',QA],O) ; O = TmpO),
	write([findOrCreateConstant,O]).

%% f(A,O) :-
%% 	write([a]),
%% 	make_quoted_string(A,QA),
%% 	write([QA]),
%% 	clEval(['FIND-OR-CREATE-CONSTANT',QA],O),
%% 	write([findOrCreateConstant,O]).

findConstant(A,O) :-
	make_quoted_string(A,QA),
	clEval(['FIND-CONSTANT',QA],O).

cap(A,O) :-
	make_quoted_string(A,QA),
	clEval(['CONSTANT-APROPOS',QA],O).

% ?- cap('Dog',X).
% X = ['AfricanWildDog', 'Akita-TheDog', 'AlabamaWaterdog', 'Albany-ColonieDiamondDogs-BaseballTeam', 'AllDogsGoToHeaven-TheMovie', 'AllDogsGoToHeaven2-TheMovie', 'AllDogsGoToHeavenActivityCenter-TheGame', 'AlphaDog-Movie', 'ArmandoGalarraga-BaseballPlayer'|...].

apropos(A,O) :-
	make_quoted_string(A,QA),
	clEval(['APROPOS',QA],O).

createOnlyIfNewConstant(A,O) :-
	make_quoted_string(A,QA),
	clEval(['FIND-CONSTANT',QA],O),
	(   O = [] ->
	    (	write('CREATING CONSTANT: '),write(A),nl,f(A,O)) ;
	    (	write('CONSTANT ALREADY EXISTS, NOT CREATING: '),write(A),nl)).

comment(A,O) :-
	clEval(['COMMENT',A],O).

ata(A,O) :-
	clEval(['ALL-TERM-ASSERTIONS',A],Results),
	findall(F,(member(Result,Results),clEval(['cyc:assertion-ist-formula',Result],F)),O),
	writeln(O).

allIsa(A,Mt,O) :-
	clEval(['ALL-ISA',A,Mt],O),
	writeln(O).

allInstances(A,Mt,O) :-
	clEval(['ALL-INSTANCES',A,Mt],O),
	writeln(O).

% ?- f('AndrewDougherty',X).
% X = 'CYC:CONSTANT'(336897, '"AndrewDougherty"').

% ?- cap('AndrewDougherty',X).
% X = ['AndrewDougherty'].

% ?- cycAssert(['isa','AndrewDougherty','Person'],'BaseKB',Result).
% Result = 'SL:T'.

% ?- cycQuery(['isa','AndrewDougherty','?X'],'BaseKB',Result).
% Result = [['CollectionUnionFn', ['TheSet', 'BiologicalLivingObject', 'IntelligentAgent']], 'Agent-Underspecified', ['CollectionUnionFn', ['TheSet', 'Person', 'MultiPersonAgent']], 'TemporallyExistingThing', ['CollectionUnionFn', ['TheSet', 'SpecifiedInformationBearingThingType'|...]], 'PartiallyIntangible', ['CollectionUnionFn', [...|...]], ['CollectionUnionFn'|...], 'Individual'|...].
