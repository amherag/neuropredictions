/*  Agent0 Interpreter
 *   File #4 : int.com
 *   Commitment Rules
 */


/*   Commitment Rules are stored as :
 *
 *   commit(Agent,Msgpattern,Mntlcond,Agent2,Action)
 *
 *   where Agent is the agent for which the commitment rule is being
 *   made (since more than one agent can be in the system at one time)
 *   Msgpattern is of the form : 
 *          [Agent2,inform,[Time2,Fact]] | [Agent2,request,Action]
 *   Mntlcond is of the form :
 *          [[Time1,Fact1],[Time2,Fact2],[Time3,Fact3]]
 *    which means that Agent needs to believe  (Fact1 @ Time1 ^ 
 *        Fact2 @ Time2 ^ Fact3 @ Time3) to commit to doing Action 
 *   Agent2 is the agent being committed to
 *   Action is of the form :
 *          do(p-action)                 | inform(Agent3,[Time,Fact])    | 
 *          request(Time,Agent2,Action)  | unrequest(Time,Agent3,Action)
 *
 *
 *  Commitments are stored as :
 *
 *   cmt(Agent,Agent2,Action)
 *
 * Agents also believe all of their commitments -
 *
 *   cmt(Agent,Agent2,Action) -> b(Agent,cmt(Agent,Agent2,Action),[Time,t])
 * 
 * where Time is the time that the commitment was made.
 */

/* Dummy Items : */
:- assert(cmt(a,b,c)).
:- assert(can(a,b)).
:- assert(refrain(a,b)).
:- retract(cmt(a,b,c)).
:- retract(can(a,b)).
:- retract(cmt(a,b,c)).


process_unreq(Agent,Agent2,Action) :-
              not(cmt(Agent,Agent2,Action));
              cmt(Agent,Agent2,Action),
              retract(cmt(Agent,Agent2,Action)),
              what_time_is_it(Now),
              add_belief(Agent,[Now,not(cmt(Agent,Agent2,Action))]).

/*  If the agent has no commitment rule, or if the agent cannot do the      */
/*  action, we ignore the request                                           */
process_req(Agent,Msgcond,Action) :- 
              not(commit(Agent,Msgcond,Mnlt,Agent2,Action));
              not(no_if_can(Agent,Action));
              refrain(Agent,Action).

no_if_can(Agent,if(Mntl,Action)) :- no_if_can(Agent,Action).

no_if_can(Agent,Action) :- can(Agent,Action).

process_req(Agent,Msgcond,Action) :- 
              commit(Agent,Msgcond,Mntcond,Agent2,Action),
              process_mental_cond(Agent,Mntcond,Holds),
              what_time_is_it(Time),
              make_commitment(Agent,Agent2,Time,Action,Holds),!.

process_reqs(Agent,Msgcond,Action) :-
              process_req(Agent,Msgcond,Action),
              fail;true.


/* If the mental condition didn't hold, the agent makes no commitment */
make_commitment(Agent,Agent2,Time,Action,f).

make_commitment(Agent,Agent2,Time,Action,u).

/* If the mental condition holds, the agent makes the commitment */
make_commitment(Agent,Agent2,Time,Action,t) :-
              assertz(cmt(Agent,Agent2,Action)),
              add_belief(Agent,[Time,cmt(Agent,Agent2,Action)]).

check_degenerate_comt_rule(Agent) :- commit(Agent,nil,Mntcond,Agent2,Action),
                              process_mental_cond(Agent,Mntcond,Holds),
                              what_time_is_it(Time),
                              make_commitment(Agent,Agent2,Time,Action,Holds),
                              !.

check_degenerate_comt_rules(Agent) :- check_degenerate_comt_rule(Agent),
                              fail;true.

process_mental_cond(Agent,[],t).

process_mental_cond(Agent,[First|Rest],f) :-
                             not(process_mental_cond(Agent,First,t)),!.

process_mental_cond(Agent,[First|Rest],TV) :- process_mental_cond(Agent,Rest,TV).

process_mental_cond(Agent,not(Mntlcond),TV) :-
                            process_mental_cond(Agent,Mntlcond,OTV),
                            invert_truth(OTV,TV).

process_mental_cond(Agent,b([Time,Belief]),TV) :-
                             believes(Agent,[Time,Belief],TV).

check_commitments(Agent) :- cmt(Agent,Agent2,Action),
                           check_one_commitment(Agent,Agent2,Action,Flag),
                           fail;true.

check_one_commitment(Agent,Agent2,if(Mntlcond,Action),t) :-
                      process_mental_cond(Agent,Mntlcond,f),
                      safe_retract(cmt(Agent,Agent2,Action)),!.

check_one_commitment(Agent,Agent2,if(Mntlcond,Action),t) :-
                      process_mental_cond(Agent,Mntlcond,u),
                      safe_retract(cmt(Agent,Agent2,Action)),!.

check_one_commitment(Agent,Agent2,if(Mntlcond,Action),Flag) :-
                      process_mental_cond(Agent,Mntlcond,t),
                      check_one_commitment(Agent,Agent2,Action,Flag2),
                      Flag2 = f,Flag = f,!; 
                      safe_retract(cmt(Agent,Agent2,if(Mntlcond,Action))),
                      Flag = t.
                     
check_one_commitment(Agent,Agent2,do(Time,PrivateAction),Flag) :-
                      what_time_is_it(Now),
                      less(Now,Time),Flag = f,!;
                      call(PrivateAction),
                      safe_retract(cmt(Agent,Agent2,do(Time,PrivateAction))),
                      Flag = t,!.

check_one_commitment(Agent,Agent2,inform(Time,Agent3,Fact),Flag) :-
                      what_time_is_it(Now),
                      less(Now,Time),Flag = f,!;
                      send_message(Agent,Agent3,inform,Fact),
                      safe_retract(cmt(Agent,Agent2,inform(Time,Agent3,Fact))),
                      flag = t,!.


check_one_commitment(Agent,Agent2,request(Time,Agent3,Fact),Flag) :-
                      what_time_is_it(Now),
                      less(Now,Time),Flag = f,!;
                      send_message(Agent,Agent3,request,Fact),
                      safe_retract(cmt(Agent,Agent2,request(Time,Agent3,Fact))),
                      Flag = t,!.


check_one_commitment(Agent,Agent2,unrequest(Time,Agent3,Fact),Flag) :-
                      what_time_is_it(Now),
                      less(Now,Time),Flag = f,!;
                      send_message(Agent,Agent3,unrequest,Fact),
                      safe_retract(cmt(Agent,Agent2,unrequest(Time,Agent3,Fact))),
                      flag = t,!.


check_one_commitment(Agent,Agent2,refrain(Time,Agent3,Fact),Flag) :-
                      what_time_is_it(Now),
                      less(Now,Time),Flag = f,!;
                      assertz(refrain(Agent,Action)),
                      safe_retract(cmt(Agent,Agent2,refrain(Time,Agent3,Fact))),
                      flag = t,!.
	
safe_retract(A) :- not(A);
                   retract(A).