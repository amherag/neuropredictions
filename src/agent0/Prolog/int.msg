/*   Agent0 Intepreter
 *   File #3 :
 *   Message Handler / Time Handler
 *
 *   THIS FILE CONTAINS ALL ROUTINES THAT ARE
 *   SPECIFIC TO QUINTUS PROLOG !!
 */

/*  This file reads in messages from the message
 *  file and updates the agent's mental condition.
 *
 *  It also writes outgoing messages
 */


read_messages(Agent):-
	a_concat_atom([Agent,'.msg'],FileName),
	a_file_exists(FileName), !, see(FileName),
        write('getting message ..'),nl,
	read_content(Agent),
	seen,a_delete_file(FileName).

read_messages(_).

read_content(Agent):-
	read(Message),
	(Message == end_of_file -> true;
         do_one_message(Agent,Message),
 	 read_content(Agent)).

do_one_message(Agent,[Agent2,request,Action]) :- 
       process_req(Agent,[Agent2,request,Action],Action).

do_one_message(Agent,[Agent2,inform,[Time,Fact]]) :-
       add_belief(Agent,[Time,Fact]),
       what_time_is_it(CurrTime),
       add_belief(Agent,[Time,b(Agent2,Fact)]).

do_one_message(Agent,[Agent2,unrequest,Action]) :-
       process_unreq(Agent,Agent2,Action).

send_message(Agent,Agent2,Type,Content):-
	a_concat_atom([Agent2,'.msg'],FileName),
 	a_file_exists(FileName), !,
        a_open_file(FileName,append,Stream),
        a_concat_atom(['[',Agent,',',Type,','],Message),
        a_write(Stream,Message),
        a_write(Stream,Content),
        a_write(Stream,'].
'),
        a_close(Stream).

send_message(Agent,Agent2,Type,Content):-
	a_concat_atom([Agent2,'.msg'],FileName),
        a_open_file(FileName,write,Stream),
        a_concat_atom(['[',Agent,',',Type,','],Message),
        a_write(Stream,Message),
        a_write(Stream,Content),
        a_write(Stream,'].
'),
        a_close(Stream).


