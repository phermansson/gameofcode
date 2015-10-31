% erl -name apa -setcookie apa
% epmd -names

-module(opponent).
-compile(export_all).
-behaviour(gen_server).

-record(state, {health = 1000}).
    
% Client side 
start(Health) ->
    {ok, Pid} = gen_server:start_link(opponent, Health, []),
    Pid.

handle_attack(Pid, Damage) ->
    gen_server:cast(Pid, {handle_attack, Damage}).

attack(MyPid, OpponentPid, Damage) ->
    gen_server:cast(MyPid, {attack, OpponentPid, Damage}).

report_health(Pid) ->
    gen_server:call(Pid, report_health).

 % - - - - -- -- - - - - --- - - --- - - -

% Server side

init(Health) ->
    say("init(~p)~n",[Health]),
    %process_flag(trap_exit, true),
    {ok, #state{health = Health}}.

terminate(Reason, State) ->
    say("Just died with reason ~p, state = ~p~n", 
	[Reason, State]).

handle_cast({handle_attack, Damage}, 
	    State = #state{health = PreviousHealth}) ->
    say("Received an attack, health status before attack: ~p~n",
	[State#state.health]),	
    NewState = State#state{health = PreviousHealth - Damage},
    
    heal(Damage * 100),
    
    say("Health after shoot : ~p~n",
	[NewState#state.health]),
    if NewState#state.health > 0 ->
	    {noreply, NewState};
       true ->
	    {stop, normal, NewState}
    end;

handle_cast({attack, OpponentPid, Damage}, State) ->
    say("Attacking ~p~n",[OpponentPid]),
    heal(10),
    opponent:handle_attack(OpponentPid, Damage),
    {noreply, State}.

handle_call(report_health, _From, State)->
    say("~p healthstatus: ~p~n",[self(), State#state.health]),
    {reply, State#state.health, State};

handle_call(terminate, _From, State)->
    say("~p terminate: ~p~n",[self(), State]),
    {reply, State, State};

handle_call(Arg, _From, State)->
    say("~p received an untrapped call(~p): ~p~n",[self(), Arg, State]),
    {reply, State, State}.

handle_info(Info, State) ->
    say("~p received info ~p, ~p.", [self(), Info, State]),
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    say("code_change ~p, ~p, ~p", [OldVsn, State, Extra]),
    {ok, State}.    

% local functions

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).


%%-----------------------------------------------------------------------------
%%Lord Heal Master
%%----------------------------------------------------------------------------
heal(Damage) ->
    tail_heal(Damage).

heal_p(0)->0;
heal_p(1)->1;
heal_p(N)->heal_p(N-1)+heal_p(N-2).
%% with guards
heal_g(N) when N == 0 ->0;
heal_g(N) when N == 1->1;
heal_g(N) when N >= 2 -> heal_g(N-1)+heal_g(N-2).
%% tail recursion
tail_heal_h(End,N,Lastheal,SecondLastheal) ->
  case N of
    End -> Lastheal + SecondLastheal;
    0 -> tail_heal_h(End, 1, 0, 0) ;
    1 -> tail_heal_h(End, 2, 1, 0) ;
    _ -> tail_heal_h(End,N+1,SecondLastheal+Lastheal,Lastheal)
  end.
tail_heal(N)->
     tail_heal_h(N,0,0,0).
