:- use_module(library(fd)).
:- pipe(in,out).
:- op(900,fy,nott).

:- set_flag(variable_names,on).
:- set_flag(all_dynamic, off).

aclp_solve(Goal, InDelta, NewDelta) :-
        solve(Goal, InDelta, NewDelta, []),
	nl,nl,writeln('Solution = '),
        full_write(NewDelta).
%        term_variables(NewDelta, DVs), aclp_labeling(DVs), 
%        term_string(NewDelta,NewDeltaStr),
%        nl,write('Solution = '),writeln(NewDeltaStr).

aclp_solve(Goal, InDelta) :-
        solve(Goal, InDelta, NewDelta, []),
        term_variables(NewDelta, DVs), aclp_labeling(DVs), 
        nl,nl,writeln('Solution = '),
        full_write(NewDelta).

aclp_solve(Goal) :-
        solve(Goal, [], NewDelta, []),
        term_variables(NewDelta, DVs), aclp_labeling(DVs), 
        nl,nl,writeln('Solution = '),
        full_write(NewDelta).

full_write([]).
full_write([X|R]) :- term_string(X,Xstr), writeln(Xstr), full_write(R).

solve(true, New_Delta, New_Delta, SearchList) :- !.

solve((A,B), Delta, New_Delta, SearchList) :- !,
	solve(A, Delta, Inter_Delta, SearchList),
        solve(B, Inter_Delta, New_Delta, SearchList).

% NAF implementation
solve(not A, Delta, Delta, SearchList) :- 
        A =.. [Functor|Rest],
        concat_atoms('not_',Functor,NF),
        NewA =.. [NF|Rest],
        member(NewA, Delta).

solve(not A, Delta, New_Delta, SearchList) :- !,
        A =.. [Functor|Rest],
        concat_atoms('not_',Functor,NF),
        NewA =.. [NF|Rest],
        add_hypothesis(NewA, Delta, DeltaPlus),
        write("Trying abducible: "), writeln(NewA),
        fail_ICS(NewA, DeltaPlus, New_Delta, SearchList),
        write("               Accepted abducible: "), writeln(NewA).

solve(A->B;C, Delta, New_Delta, SearchList) :- !,
	( solve(A, Delta, Inter_Delta, SearchList) ->
		solve(B, Inter_Delta, New_Delta, SearchList)
	  ; solve(C, Delta, New_Delta, SearchList) ).

solve(A, New_Delta, New_Delta, SearchList) :-
	system_pred(A), !,
        (constraint(A) ->
            unfold_constraint(A, UnfoldedA, VarsInA),
            bug_fix_call(UnfoldedA),
            propagate(VarsInA) 
         ;  call(A)).

solve(A, New_Delta, New_Delta, SearchList) :-
        not abducible(A), 
	A = test(AbdPred), !,
        member(AbdPred,New_Delta).

solve(A, New_Delta, New_Delta, SearchList) :-
        not abducible(A),
        A = not_in(AbdPred), !,
        nonmember(AbdPred,New_Delta).

solve(A, Delta, New_Delta, SearchList) :-
	not abducible(A), !,
	clause(A,Goals),
	solve(Goals, Delta, New_Delta, SearchList).

solve(A, Delta, New_Delta, SearchList) :-
        abducible(A),
        printf(out,"%vw",A),write(out,".\n"),flush(out),
        read(in,Abducible),
        term_string(Abducible,S),
        term_string(AbdForSearch,S),
        check_similarity(AbdForSearch, SearchList),
        append(SearchList, [AbdForSearch], NewSearchList),
        solve_abducible(A, Delta, New_Delta, NewSearchList).

solve_abducible(A, Delta, New_Delta, SearchList) :-
	member(A, Delta),
        term_variables(A,VarsInA),
        propagate(VarsInA),
	New_Delta = Delta.

solve_abducible(A, Delta, New_Delta, SearchList) :-
	add_hypothesis(A, Delta, DeltaPlus),
        write("Trying abducible: "), writeln(A),
        fail_ICS(A, DeltaPlus, New_Delta, SearchList),
        write("               Accepted abducible: "), writeln(A).

system_pred(nott(P)) :- !.
system_pred(not_(P)) :- !.
system_pred(A) :-
	functor(A, Functor, Arity),
	current_built_in(Functor/Arity).

arithm_op(A) :-
        not var(A),
        functor(A, Functor, Arity),
        arithmetic(Functor,Arity).

arithmetic(+,2).
arithmetic(-,2).
arithmetic(*,2).
arithmetic(/,2).

abducible(A) :-
	functor(A, Functor, Arity),
	abducible_predicate(Functor/Arity).

fail_ICS(AbducibleOrg, HypSoFar, NewHypSoFar, SearchList) :-
	printf(out,"%vw",AbducibleOrg),write(out,".\n"),flush(out),
	read(in,Abducible),
	findall(IBody,
		clause_list(ic,IBody),
		ListOfIBodies),
	findall(Abducible-ResolvedIBody,
		( member(OneIBody, ListOfIBodies),
		  delete(Abducible, OneIBody, ResolvedIBody) ),
		ListOfFilteredResolvedIBodies),
        make_new_conjuctions(ListOfFilteredResolvedIBodies, Abducible, [],
                                ListOfResolvedIBodies),
        AbducibleOrg = Abducible,!,
	failure_all(ListOfResolvedIBodies, Abducible, HypSoFar,
                    NewHypSoFar, SearchList).

failure_all([], Abducible, HypSoFar, HypSoFar, SearchList).
failure_all(ListOfResolvedIBodies, Abducible, HypSoFar, NewHypSoFar, SearchList) :-
	select_first_body(ListOfResolvedIBodies, SelectedIBody, RestOfIBodies),
	not empty(SelectedIBody),
	failure_one(SelectedIBody, Abducible, HypSoFar, InterHyp, SearchList),
	failure_all(RestOfIBodies, Abducible, InterHyp, NewHypSoFar, SearchList).

failure_one(SelectedIBody, Abducible, HypSoFar, InterHyp, SearchList) :-
	select_literal(SelectedIBody, SelectedLiteral, RestOfLiterals),
	failure_on_literal(SelectedLiteral, RestOfLiterals,
			   Abducible, HypSoFar, InterHyp, SearchList).

failure_on_literal(SelectedLiteral, RestOfLiterals,
		   Abducible, HypSoFar, InterHyp, SearchList) :-
	not abducible(SelectedLiteral),
	not constraint(SelectedLiteral),
	not system_pred(SelectedLiteral),
	SelectedLiteral \= test(AbdPred), !, %%% Backtracking
	printf(out,"%vw",SelectedLiteral),write(out,".\n"),flush(out),
	read(in,NewSelectedLiteral),
	findall(NewSelectedLiteral-Body,
		clause_list(NewSelectedLiteral,Body),
		SelectedLiteralBodies),
	make_new_conjuctions2(SelectedLiteralBodies, SelectedLiteral,
		     RestOfLiterals, ListOfNewConjuctions),!, %%% Backtracking
	failure_all(ListOfNewConjuctions, Abducible, HypSoFar, InterHyp, SearchList).

failure_on_literal(SelectedLiteral, RestOfLiterals,
                   Abducible, HypSoFar, InterHyp, SearchList) :-
        not abducible(SelectedLiteral),
	constraint(SelectedLiteral), !, %%% Backtracking
	negate_constraint(SelectedLiteral, NegConstr, TestVars,HypSoFar),
	execute_constr(NegConstr, TestVars, RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList).

failure_on_literal(SelectedLiteral, RestOfLiterals,
                   Abducible, HypSoFar, InterHyp, SearchList) :-
        not abducible(SelectedLiteral),
	not constraint(SelectedLiteral),
	system_pred(SelectedLiteral), !, %%% Backtracking
        ( SelectedLiteral = not(PositiveLiteral) ->
           (memberchk(soft, RestOfLiterals) ->
              append_before_last(RestOfLiterals,[not_(PositiveLiteral)],NewRestOfLiterals)
            ; append(RestOfLiterals,[not_(PositiveLiteral)],NewRestOfLiterals) )
          ; NewRestOfLiterals = RestOfLiterals ),
	execute_system(SelectedLiteral,NewRestOfLiterals,Abducible,HypSoFar,InterHyp, SearchList).

failure_on_literal(SelectedLiteral, RestOfLiterals,
                   Abducible, HypSoFar, InterHyp, SearchList) :-
        abducible(SelectedLiteral), !, %%% Backtracking
	make_new(HypSoFar,SelectedLiteral,RestOfLiterals,ListOfNewConjuctions),
	!,
	failure_all(ListOfNewConjuctions, Abducible, HypSoFar, InterHyp, SearchList).

execute_constr(NegConstr, TestVars, RestOfLiterals, Abducible, HypSoFar, HypSoFar, SearchList) :-
        (NegConstr = terms_different ->
           true
         ; 
          ( issoft(TestVars,NegConstr) ->
              Type = soft ; Type = hard ),
          call(NegConstr),
   	  propagate(TestVars),
          term_variables(HypSoFar, VarsInDelta),
          propagate(VarsInDelta),
          ( Type = soft ->
            ! ; true) ). % Backtrack only when NegConstr is a hard constraint

execute_constr(NegConstr, TestVars, RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList) :-
        ( NegConstr = terms_different ->
            fail
          ; true ),
        reverse_constr(NegConstr, PosConstr),
        bug_fix_call(PosConstr),
        propagate(TestVars),
        term_variables(HypSoFar, VarsInDelta),
        propagate(VarsInDelta),
        failure_one(RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList).

reverse_constr(fail, true).
reverse_constr(A #/\ B, NegConstr) :- !,
        reverse_constr(A, NA),
        reverse_constr(B, NB),
        NegConstr =.. ['#\\/', NA, NB].
reverse_constr(A #\/ B, NegConstr) :- !,
        reverse_constr(A, NA),
        reverse_constr(B, NB),
        NegConstr =.. ['#/\\', NA, NB].
reverse_constr(Constr, NegConstr) :-
        Constr =.. [PredName|RestOfConstr],
        negate(PredName, NegPredName),
        NegConstr =.. [NegPredName|RestOfConstr].

bug_fix_call(A #/\ B):-!,
        call(A),
        bug_fix_call(B).
bug_fix_call(A):- call(A).

issoft([],_).
issoft([Z],X##Y):-
  dom(Z, DomainList),
  (nonvar(X) -> 
     C = X
   ; C = Y),
  nonmember(C,DomainList).

issoft([_,_],X#<Y):-
   is_domain(X),is_domain(Y),
   maxdomain(X,Max),
   mindomain(Y,Min),
   Max < Min.

issoft([_,_],X#<=Y):-
   is_domain(X),is_domain(Y),
   maxdomain(X,Max),
   mindomain(Y,Min),
   Max =< Min.

issoft([_,_],X#>Y):-
   is_domain(X),is_domain(Y),
   mindomain(X,Min),
   maxdomain(Y,Max),
   Min > Max.

issoft([_,_],X#>=Y):-
   is_domain(X),is_domain(Y),
   mindomain(X,Min),
   maxdomain(Y,Max),
   Min >= Max.


propagate(TestVars) :-
	( \+once(aclp_labeling(TestVars)) ->
		fail
	  ;	true).

execute_system(nott(PositivePredicate), RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList) :-
                solve_test(PositivePredicate,HypSoFar,InterHyp,SearchList).

execute_system(nott(PositivePredicate), RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList) :-
                !,
        failure_one(RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList).

execute_system(not(PositivePredicate), RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList) :-
                solve_test(PositivePredicate,HypSoFar,InterHyp,SearchList).

execute_system(not(PositivePredicate), RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList) :-
                !,
	failure_one(RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList).

execute_system(not_(PositivePredicate), RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList) :-
                solve(PositivePredicate,HypSoFar,InterHyp,SearchList).

execute_system(not_(PositivePredicate), RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList) :-
                !,
	failure_one(RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList).

execute_system(SystemPred, RestOfLiterals, Abducible, HypSoFar, HypSoFar, SearchList) :-
	  not call(SystemPred).

execute_system(SystemPred, RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList) :-
	  ( (SystemPred \= writeln(Term), SystemPred \= write(Term)) ->
		call(SystemPred)
	     ;  true ),
	failure_one(RestOfLiterals, Abducible, HypSoFar, InterHyp, SearchList).

check_similarity(_, []).
check_similarity(AbdforSearch, [OldAbd | OldAbds]):-
	(\+ variant(AbdforSearch, OldAbd)),
	check_similarity(AbdforSearch, OldAbds).

make_new_conjuctions([], _, _, []).
make_new_conjuctions([SelectedLiteral-Body|RestLBodies], SelectedLiteral,
                    RestOfLiterals, [OneConjuction|RestOfConjuctions]) :-
       append(Body, RestOfLiterals, OneConjuction),
       make_new_conjuctions(RestLBodies, SelectedLiteral,
               RestOfLiterals, RestOfConjuctions).
make_new_conjuctions([NewSelectedLiteral-Body|RestLBodies], SelectedLiteral,
                    RestOfLiterals, Conjuctions) :-
		NewSelectedLiteral \= SelectedLiteral,
		make_new_conjuctions(RestLBodies, SelectedLiteral,
		RestOfLiterals, Conjuctions).

make_new_conjuctions2([], _, _, []).
make_new_conjuctions2([SelectedLiteral-Body|RestLBodies], SelectedLiteralOrg,
                    RestOfLiterals,RestOfConjuctions):-
         SelectedLiteral \= SelectedLiteralOrg,!,
         make_new_conjuctions2(RestLBodies, SelectedLiteralOrg,
              RestOfLiterals, RestOfConjuctions).
make_new_conjuctions2([SelectedLiteral-Body|RestLBodies], SelectedLiteralOrg,
                    RestOfLiterals, [OneConjuction|RestOfConjuctions]) :-
	( RestLBodies = [] ->
	SelectedLiteral = SelectedLiteralOrg,
	append(Body,RestOfLiterals,OneConjuction)
  ;
	copy_term(SelectedLiteralOrg-RestOfLiterals,NewSelected-NewRest),
	term_variables(SelectedLiteralOrg-RestOfLiterals,RestV),
	term_variables(NewSelected-NewRest,NewRestV),
	unify_domain_var(RestV,NewRestV),
	NewSelected = SelectedLiteral,
	append(Body, NewRest, OneConjuction) ),
	make_new_conjuctions2(RestLBodies, SelectedLiteralOrg,
               RestOfLiterals, RestOfConjuctions).

make_new([],_,_,[]).
make_new([Abd|Abds],Selected,Rest,NewConjuctions) :-
	( (not_unify(Abd, Selected);Abds=[]) ->
		NewSelected = Selected, NewRest = Rest
	  ;	copy_term(Selected-Rest,NewSelected-NewRest),
		term_variables(Rest,RestV),
		term_variables(NewRest,NewRestV),
	        unify_domain_var(RestV,NewRestV) ),
	( Abd = Selected ->
		NewConjuctions = [Rest|Others]
	   ;	NewConjuctions = Others ),
	make_new(Abds, NewSelected, NewRest, Others).
	
unify_domain_var([], []).
unify_domain_var([Var|Vars],[NVar|NVars]) :-
	( (is_domain(Var),is_domain(NVar)) ->
		Var = NVar
	  ;	true ),
	unify_domain_var(Vars, NVars).


constraint(Goal) :-
	functor(Goal, Functor, Arity),
	constr(Functor/Arity).

constr('##'/2).
constr('#='/2).
constr('#<'/2).
constr('#>'/2).
constr('#>='/2).
constr('#<='/2).
constr('#/\\'/2).
constr('#\\/'/2).

construct_conjunct([],[],true).
construct_conjunct([A1|A1s],[A2|A2s],E):-
        construct_conjunct(A1s,A2s,Es),
        term_variables((A1,A2),Var),    % both constants and only fd variables
        Var \= [], all_are_fdvar(Var),!,
        (Es = true
                -> E = (A1 #= A2)
                ;       E = ((A1 #= A2) #/\ Es)
                ).
construct_conjunct([A1|A1s],[A2|A2s],Es):-
        construct_conjunct(A1s,A2s,Es),
        A1 = A2.

construct_disjunct([],[],true).
construct_disjunct([A1|A1s],[A2|A2s],E):-
        construct_disjunct(A1s,A2s,Es),
        term_variables((A1,A2),Var),    % both constants and only fd variables
        Var \= [], all_are_fdvar(Var),!,
        (Es = true
                -> E = (A1 ## A2)
                ;       E = ((A1 ## A2) #\/ Es)
                ).
construct_disjunct([A1|A1s],[A2|A2s],Es):-
        construct_disjunct(A1s,A2s,Es),
        A1 = A2.   


all_are_fdvar([]).
all_are_fdvar([V|Vars]):-
        is_domain(V),
        all_are_fdvar(Vars).

negate_constraint(true, fail, [], _).  % Antonis addition on 4.4.00

negate_constraint(Constr, NegConstr, TestVars, HypSoFar) :-
        Constr =.. ['#=', Term1, Term2],
        not arithm_op(Term1),  not arithm_op(Term2),
        ( (compound(Term1), compound(Term2)) -> !,
            Term1 =.. [F1|Arg1], Term2 =.. [F2|Arg2],
            ( F1 \= F2 ->
                NegConstr = terms_different,
                TestVars = []
              ; ( construct_conjunct(Arg1,Arg2,Conj) ->
                    negate_constraint(Conj, NegConstr, TestVars, HypSoFar) % Antonis 4.4.00
                   ; NegConstr = true, TestVars = [] ) )
          ; ( (compound(Term1);compound(Term2)) -> !,
                NegConstr = terms_different,
                TestVars = []
              ; fail ) ).

negate_constraint(Constr, NegConstr, TestVars, HypSoFar) :-
        Constr =.. ['##', Term1, Term2],
        not arithm_op(Term1),  not arithm_op(Term2),
        ( (compound(Term1), compound(Term2)) -> !,
            Term1 =.. [F1|Arg1], Term2 =.. [F2|Arg2],
            ( F1 \= F2 ->
                NegConstr = fail,
                TestVars = []
              ; ( construct_disjunct(Arg1,Arg2,Conj) ->
                   negate_constraint(Conj, NegConstr, TestVars, HypSoFar)
                 ; NegConstr = fail, TestVars = [] ) )
          ; ( (compound(Term1);compound(Term2)) -> !,
                NegConstr = fail,
                TestVars = []
              ; fail ) ).


negate_constraint(Constr, NegConstr, TestVars,_) :-
	Constr =.. [PredName|RestOfConstr],
	PredName \= '#/\\',
	negate(PredName, NegPredName),
	NegConstr =.. [NegPredName|RestOfConstr],
	term_variables(RestOfConstr, TestVars).
negate_constraint(Constr, NegConstr, TestVars, HypSoFar) :-
        Constr =.. ['#/\\', Constr1, Constr2],
        negate_constraint(Constr1, NegConstr1, TestVars1, HypSoFar),
        negate_constraint(Constr2, NegConstr2, TestVars2, HypSoFar),
%        append(TestVars1,TestVars2,TestVars),
	NegConstr3 =.. ['#\\/', NegConstr1, NegConstr2],
	term_variables(HypSoFar, VarsInDelta),
	( TestVars1 = [] ->
	  ( call(NegConstr1) ->
		NegConstr =true, TestVars=[]
	     ;  NegConstr = NegConstr2, TestVars =TestVars2)
          ; (TestVars2 = [] ->
             ( call(NegConstr2) ->
                  NegConstr =true, TestVars=[]
               ;  NegConstr = NegConstr1 , TestVars =TestVars1)
             ; ( \+ (call(NegConstr1),propagate(VarsInDelta)) ->
			Ans1 = 0
		  ;     Ans1 = 1 ),
	       ( \+ (call(NegConstr2),propagate(VarsInDelta)) ->
                        Ans2 = 0
                  ;     Ans2 = 1 ),
	       final_constr(Ans1,Ans2,NegConstr1,TestVars1,NegConstr2,TestVars2,NegConstr3,NegConstr,TestVars) )).

final_constr(1,1,_,_,_,_,NegConstr,NegConstr,[]).
final_constr(1,0,NegConstr,TestVars,_,_,_,NegConstr,TestVars).
final_constr(0,1,_,_,NegConstr,TestVars,_,NegConstr,TestVars).
final_constr(0,0,_,_,_,_,_,fail,[]).

negate(##, #=).
negate(#=, ##).
negate(#>, #<=).
negate(#<, #>=).
negate(#<=, #>).
negate(#>=, #<).

unfold_constraint(Constr, NegConstr, TestVars) :-
        Constr =.. ['##', Term1, Term2],
        ( (compound(Term1), compound(Term2)) -> !,
            Term1 =.. [F1|Arg1], Term2 =.. [F2|Arg2],
            ( F1 \= F2 ->
                NegConstr = true,
                TestVars = []
              ; ( construct_disjunct(Arg1,Arg2,Conj) ->
                     NegConstr = Conj,
                     term_variables(NegConstr, TestVars) 
                   ; NegConstr = true, TestVars = [] ) )
          ; ( (compound(Term1);compound(Term2)) -> !,
                NegConstr = Constr,
                TestVars = []
              ; NegConstr = Constr, TestVars = [] ) ).

unfold_constraint(Constr, NegConstr, TestVars) :-
        Constr =.. ['#=', Term1, Term2],
        ( (compound(Term1), compound(Term2)) -> !,
            Term1 =.. [F1|Arg1], Term2 =.. [F2|Arg2],
            ( F1 \= F2 ->
                NegConstr = fail,
                TestVars = []
              ; ( construct_conjunct(Arg1,Arg2,Conj) ->
                   NegConstr = Conj,
                   term_variables(NegConstr, TestVars) 
                 ; NegConstr = fail, TestVars = [] ) )
          ; ( (compound(Term1);compound(Term2)) -> !,
                NegConstr = Constr,
                TestVars = []
              ; NegConstr = Constr, TestVars = []  ) ).

unfold_constraint(Constr, NegConstr, TestVars) :-
        Constr =.. [F|Rest],
        F \= '##', F \= '#=',
        NegConstr = Constr,
        term_variables(NegConstr, TestVars).

select_first_body([FirstBody|RestBodies], FirstBody, RestBodies).

select_literal([SelectedLiteral|RestOfLiterals], SelectedLiteral,
		RestOfLiterals).

clause_list(Head, BodyList) :-
	clause(Head, Body),
	convert_to_list(Body, BodyList).

convert_to_list((A,B), [A|X]) :- !, convert_to_list(B, X).
convert_to_list(true, []) :- !.
convert_to_list(A, [A]).

empty([]).

add_hypothesis(Hypothesis, HypSoFar, [Hypothesis|HypSoFar]).

append_before_last([X], [], [X]).
append_before_last([X], [Goal], [Goal | Rest]):-
  append_before_last([X], [], Rest).
append_before_last([X | Xs], [Goal], [X | Rest]):-
 append_before_last(Xs, [Goal], Rest).

solve_test(true, New_Delta, New_Delta, SearchList) :- !.

solve_test((A,B), Delta, New_Delta, SearchList) :- !,
	solve_test(A, Delta, Inter_Delta, SearchList),
        solve_test(B, Inter_Delta, New_Delta, SearchList).

% NAF implementation
solve_test(not A, Delta, New_Delta, SearchList) :- !,
	A =.. [Functor|Rest],
	concat_atoms('not_',Functor,NF),
	NewA =.. [NF|Rest],
	member(NewA, Delta),
	New_Delta = Delta.

solve_test(A->B;C, Delta, New_Delta, SearchList) :- !,
	( solve_test(A, Delta, Inter_Delta, SearchList) ->
		solve_test(B, Inter_Delta, New_Delta, SearchList)
	  ; solve_test(C, Delta, New_Delta, SearchList) ).

solve_test(A, New_Delta, New_Delta, SearchList) :-
	system_pred(A), !,
	call(A),
        (constraint(A) ->
            (term_variables(A,VarsInA),
             propagate(VarsInA) )
          ; true).

solve_test(A, New_Delta, New_Delta, SearchList) :-
        not abducible(A), 
	A = test(AbdPred), !,
        member(AbdPred,New_Delta).

solve_test(A, New_Delta, New_Delta, SearchList) :-
        not abducible(A),
        A = not_in(AbdPred), !,
        nonmember(AbdPred,New_Delta).

solve_test(A, Delta, New_Delta, SearchList) :-
	not abducible(A), !,
	clause(A,Goals),
	solve_test(Goals, Delta, New_Delta, SearchList).

solve_test(A, New_Delta, New_Delta, SearchList) :-
        abducible(A),
        member(A, New_Delta),
        term_variables(A,VarsInA),
        propagate(VarsInA).

aclp_labeling([]) :- !.
aclp_labeling(List) :-
          deleteff(Var, List, Rest),
          indomain(Var),
          aclp_labeling(Rest).

:- set_flag(goal_expansion, off).
:- dynamic abducible_predicate/1.
