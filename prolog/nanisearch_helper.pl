:- module(nanisearch_helper, [init/1,
			      getMicrotheoryFromSessionID/2,
			      loadNaniSearchIntoLarKC/1,
			      cycAssert/3,
			      cycUnassert/3,
			      cycQuery/3,
			      allIsa/3
			     ]).

:- use_module(larkc_client_eval_wrappers).
:- use_module(nanisearch_helper).

:- dynamic microtheory/2, predicate/2.

types([
       are([kitchen,office,hall,diningRoom,cellar],room),
       are([key],keyType),
       are([desk,apple,washingMachine,nani,broccoli,crackers,computer,envelope,stamp,key],naniObject),
       are([flashlight],device),
       genls(device,naniObject),
       genls(keyType,naniObject)
     ]).

init([
      door(office, hall),
      door(kitchen, office),
      door(hall, diningRoom),
      door(kitchen, cellar),
      door(diningRoom, kitchen),
      opened(office, hall),
      opened(kitchen, office),
      opened(hall, diningRoom),
      key_for_door(key,hall,diningRoom),
      opened(kitchen, cellar),
      opened(diningRoom, kitchen),
      location(desk, office),
      location(apple, kitchen),
      location(flashlight, office),
      location(washingMachine, cellar),
      location(nani, washingMachine),
      location(broccoli, kitchen),
      location(crackers, kitchen),
      location(computer, office),
      location(envelope, desk),
      location(stamp, envelope),
      location(key, envelope),
      here(kitchen),

      neg(turned_on(flashlight)),
      neg(have(flashlight))
     ]).


generatePredicatesFromTypesAndInit(SessionID) :-
	init(Init),
	member(TmpAssertion,Init),
	(   TmpAssertion = neg(Assertion) -> true ; TmpAssertion = Assertion),	
	Assertion =.. [Predicate|Arguments],
	length(Arguments,Arity),
	(   predicate(Predicate,Arity) -> true ; assert(predicate(Predicate,Arity))),
	fail.
generatePredicatesFromTypesAndInit(SessionID) :-
	getMicrotheoryFromSessionID(SessionID,Microtheory),
	predicate(Predicate,Arity),
	f(Predicate,Result0),
	writeln([result0,Result0]),
	(   Arity = 1 -> PredicateType = 'UnaryPredicate' ;
	    (	Arity = 2 -> PredicateType = 'BinaryPredicate' ;
		(   Arity = 3 -> PredicateType = 'TernaryPredicate' ;
		    (	writeln('ERROR: larger predicate than weve defined'),fail)))),
	cycAssert([isa,Predicate,PredicateType],Microtheory,Result1),
	writeln([result1,Result1]),
	cycAssert([arity,Predicate,Arity],Microtheory,Result2),
	writeln([result2,Result2]),	
	fail.
generatePredicatesFromTypesAndInit(SessionID) :-
	getMicrotheoryFromSessionID(SessionID,Microtheory),
	predicate(Predicate,Arity),
	writeln([predicate,Predicate,arity,Arity]),
	init(Init),
	findall(Arguments,(
			   member(TmpAssertion,Init),
			   (   TmpAssertion = neg(Assertion) -> true ; TmpAssertion = Assertion),
			   Assertion =.. [Predicate|Arguments], length(Arguments,Arity)
			  ),ListOfLists),
	%% writeln([listOfLists,ListOfLists]),
	foreach(between(1,Arity,N),
		(
		 findall(Type,(member(Arguments,ListOfLists),nth1(N,Arguments,Argument),allIsa(Argument,Microtheory,Types),member(Type,Types)),AllTypes),
		 nth1(1,AllTypes,Type),
		 cycAssert([argIsa,Predicate,N,Type],Microtheory,Result1),
		 writeln([result1a,Result1])
		)),
	fail.
generatePredicatesFromTypesAndInit(_).

/*
predicates([
	    isa(here,'UnaryPredicate'),
	    arity(here,1),
	    arg1Isa(here,room),

	    isa(door,'BinaryPredicate'),
	    arity(door,2),
	    arg1Isa(door,room),
	    arg2Isa(door,room),

	    isa(opened,'BinaryPredicate'),
	    arity(opened,2),
	    arg1Isa(opened,room),
	    arg2Isa(opened,room),

	    isa(key_for_door,'TernaryPredicate'),
	    arity(key_for_door,3),
	    arg1Isa(key_for_door,keyType),
	    arg2Isa(key_for_door,room),
	    arg3Isa(key_for_door,room),

	    isa(location,'BinaryPredicate'),
	    arity(location,2),
	    arg1Isa(location,naniObject),
	    %% arg2Isa(location,naniObject or room)

	    isa(have,'UnaryPredicate'),
	    arity(have,1),
	    arg1Isa(have,naniObject),

	    isa(turned_on,'UnaryPredicate'),
	    arity(turned_on,1),
	    arg1Isa(turned_on,device)
	   ]).
*/

processTypes(SessionID) :-
	getMicrotheoryFromSessionID(SessionID,Microtheory),
	types(Assertions),
	member(Assertion,Assertions),
	(   Assertion = are(Objects,Type) ->
	    (
	     createTypeIfNotExists(Type,Microtheory,_Result1),
	     member(Object,Objects),
	     createObjectIfNotExists(Object,Type,Microtheory,_Result2)
	    ) ;
	    (	Assertion = genls(SubType,SuperType) ->
		(
		 createTypeIfNotExists(SubType,Microtheory,Result3),
		 writeln([result3,Result3]),
		 createTypeIfNotExists(SuperType,Microtheory,Result4),
		 writeln([result4,Result4]),
		 cycAssert([genls,SubType,SuperType],Microtheory,Result5),
		 writeln([result5,Result5])
		) ; true)),
	fail.
processTypes(_).

getMicrotheoryFromSessionID(SessionID,Microtheory) :-
	atomic_list_concat(['LD44',SessionID,'Mt'],'-',Microtheory),
	writeln([microtheory,Microtheory]),
	(   microtheory(Microtheory,SessionID) ->
	    true ;
	    (	
		f(Microtheory,_Result1),
		cycAssert([isa,Microtheory,'Microtheory'],'BaseKB',_Result2),
		assert(microtheory(Microtheory,SessionID))
	    )).

createTypeIfNotExists(Type,Microtheory,_Result) :-
	write([type,Type]),
	f(Type,Result1),
	writeln([resA,Type,Result1,Microtheory]),
	cycAssert([isa,Type,collection],Microtheory,Result2),
	writeln([resB,Result2]).

createObjectIfNotExists(Object,Type,Microtheory,_Result) :-
	f(Object,Result1),
	writeln(Result1),
	cycAssert([isa,Object,Type],Microtheory,_Result2).


/*
processPredicates(SessionID) :-
	getMicrotheoryFromSessionID(SessionID,Microtheory),
	predicates(PredicateAssertions),
	member(Assertion,PredicateAssertions),
	(   Assertion = isa(Predicate,_Type) ->
	    (	f(Predicate,Result1), writeln([res1,Result1]), true) ;
	     true),
	Assertion =.. List,
	cycAssert(List,Microtheory,Result2),
	writeln([res2,Assertion,Result2]),
	fail.
processPredicates(_).
*/

processInit(SessionID) :-
	getMicrotheoryFromSessionID(SessionID,Microtheory),
	init(InitAssertions),
	member(TmpAssertion,InitAssertions),
	(   TmpAsserion = neg(Assertion) -> fail ; TmpAssertion = Assertion),
	Assertion =.. List,
	cycAssert(List,Microtheory,Result2),
	writeln([res2,Assertion,Result2]),
	fail.
processInit(_).

processTypesPredicatesAndInit(SessionID) :-
	processTypes(SessionID),
	generatePredicatesFromTypesAndInit(SessionID),
	%% processPredicates(SessionID),
	processInit(SessionID).

loadNaniSearchIntoLarKC(SessionID) :-
	(   microtheory(_,SessionID) -> true ;
	    (	
		writeln('loading session'),
		processTypesPredicatesAndInit(SessionID),
		writeln('done loading session')
	    )).


