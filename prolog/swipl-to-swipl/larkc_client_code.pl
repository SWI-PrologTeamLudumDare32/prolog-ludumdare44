larkc_load_contexts(AgentName,FormalogName,Contexts) :-
	getVar(AgentName,FormalogName,'context',Var),
	kbs2Data(Var,CurrentContext),
	forall(member(Context,Contexts),
	       (
		setContext(AgentName,FormalogName,Context),
		nl,write('LOADING CONTEXT: '),write(Context),nl,
		larkc_load_database(AgentName,FormalogName,Context)
	       )),
	setContext(AgentName,FormalogName,CurrentContext).

larkc_load_database(AgentName,FormalogName,Context) :-
	kquery(AgentName,FormalogName,nil,Bindings),
	larkc_assert_list(Bindings,Context).

larkc_assert_list( [ ], Context ).
larkc_assert_list( [ X | Y ], Context ):- write('Asserting: '),write_term(X,[quoted(true)]),larkc_assert(X,Context,Result),nl,larkc_assert_list( Y ).

noFunctionsInMicrotheoryNames(0).

larkcAssert(Assertion,Context,[Result1,Result2]) :-
	(   noFunctionsInMicrotheoryNames(1) -> 
	    ensureMicrotheoryExists(Context,Microtheory,Result1) ;
	    ensureContextMtExists(Context,Microtheory,Result1)),
	ensureConstantsInTermExist(Assertion),
	cycAssert(Assertion,Microtheory,Result2).
	
larkcUnassert(Assertion,Context,[Result1,Result2]) :-
	(   noFunctionsInMicrotheoryNames(1) -> 
	    ensureMicrotheoryExists(Context,Microtheory,Result1) ;
	    ensureContextMtExists(Context,Microtheory,Result1)),
	%% ensureConstantsInTermExist(Assertion),
	cycUnassert(Assertion,Microtheory,Result2).

larkcQuery(Query,Context,[Result1,Result2]) :-
	(   noFunctionsInMicrotheoryNames(1) -> 
	    ensureMicrotheoryExists(Context,Microtheory,Result1) ;
	    ensureContextMtExists(Context,Microtheory,Result1)),
	%% ensureConstantsInTermExist(Query),
	cycQuery(Query,Microtheory,Result2).

ensureContextMtExists(Context,Microtheory,Result) :-
	make_quoted_string(Context,QContext),
	Microtheory = ['larkcMtFn',QContext],
	cycAssert([isa,Microtheory,'Microtheory'],'BaseKB',Result).
	
ensureMicrotheoryExists(Context,Microtheory,Result) :-
	split_string(Context,'::','',TmpItems),
	findall(Item,(member(Item,TmpItems),Item \= ""),Items),
	append([['LarKC-'],Items,['-Mt']],TmpMicrotheory),
	atomic_list_concat(TmpMicrotheory,'-',Microtheory),
	f(Microtheory,Result1),
	cycAssert([isa,Microtheory,'Microtheory'],'BaseKB',Result2),
	Result = [Result1,Result2].

ensureConstantsInTermExist(Assertion) :-
	getAllConstantsInTerm(Assertion,Constants),
	print_term(Constants,[]),
	forall(member(Constant,Constants),findConstant(Constant,Result)).

getAllConstantsInTerm(Term,Constants) :-
	(   is_list(Term) ->
	    (   
		findall(TmpResults,(member(Item,Term),getAllConstantsInTerm(Item,TmpResults)),AllTmpResults),
		append(AllTmpResults,Results)
	    ) ;
	    (	Term =.. [P|Args] -> 
	    	(   getAllConstantsInTerm(Args,TmpResults),append([[P],TmpResults],Results)) ;
		   Results = [Term])),
	print_term(Results,[]),
	list_to_set(Results,Constants).

defineLarkcMyFn :-
	f(larkcMtFn,_),
	cycAssert([isa,larkcMtFn,'UnaryFunction'],'BaseKB',_),
	cycAssert([resultIsa,larkcMtFn,'Microtheory'],'BaseKB',_),
	cycAssert([arg1Isa,larkcMtFn,'CharacterString'],'BaseKB',_),
	cycAssert([arity,larkcMtFn,1],'BaseKB',_),
	cycAssert([isa,[larkcMtFn,'"Org::CYC::Base"'],'Microtheory'],'BaseKB',X).

testLarkcMyFn :-
	cl_eval(['CYC-ASSERT',[quote,[isa,'AndrewDougherty','Person']],[quote,[larkcMtFn,'"Org::CYC::BaseKB"']]],X),
	cl_eval(['ALL-ISA',larkcMtFn,'BaseKB'],X),
	cycQuery([arity,larkcMtFn,'?X'],'BaseKB',X),
	cl_eval(['ALL-ISA','AndrewDougherty','POSICommunityMt'],X),
	f('MaximilianDougherty',_),
	cl_eval(['CYC-ASSERT',[quote,[isa,'MaximilianDougherty','Dog']],[quote,[larkcMtFn,'"Org::CYC::BaseKB"']]],X),
	cl_eval(['ALL-ISA','MaximilianDougherty',[quote,[larkcMtFn,'"Org::CYC::BaseKB"']]],X),
	cl_eval(['HL-WHY-NOT-WFF',[quote,[isa,'MaximilianDougherty','Dog']],[quote,[larkcMtFn,'"Org::CYC::BaseKB"']]],X).