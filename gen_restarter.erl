-module(gen_restarter).
-export([start/2, stop/1]).
%
% gen_restarter: Tries to restart any child that 'stops' (supervisors reads 'EXIT') and is able
% to stop each child itself.
%
% defininitions: a child is a tuple of from {Module, Function, Args}
%                children refers to a list of child tuples

% ---------------------------------
% public interface
% ---------------------------------
start(Name, {Module, Function, Args}) ->
    register(Name, spawn_link(gen_restarter, init, [{Module, Function, Args}])), ok.

stop(Name) ->
    Name ! {stop, self()},
    receive 
        {reply, Reply} ->
            Reply
    end.

% ---------------------------------
% module internal
% ---------------------------------

init(Children) ->
    process_flag(trap_exit, true),
    loop(start_children(Children)).

start_children([]) -> [];
start_children([{Module, Function, Args} | Other]) ->
    case (catch apply(Module, Function, Args)) of 
        {ok, Pid} -> 
            [{Pid, {Module, Function, Args}} | start_children(Other)];
        _ -> 
            start_children(Other)
    end.

% find the Pid in the children list, start it again and delete the old from the list
restart_child(Pid, Children) ->
    {value, {Pid, {Module, Function, Args}}} = lists:keysearch(Pid, 1, Children),
    {ok, NewPid} = apply(Module, Function, Args),
    [{NewPid, {Module, Function, Args}} | lists:keydelete(Pid, 1, Children)].

terminate([{Pid, _} | Children]) ->
    exit(Pid, kill),
    terminate(Children);
terminate(_Children) -> ok.

% loop/1: this is the body of the supervisor, where all the monitoring logic happens
loop(Children) ->
    receive
        {'EXIT', Pid, _Reason} ->
            NewChildren = restart_child(Pid, Children),
            loop(NewChildren);
        {stop, From} ->
            From ! {reply, terminate(Children)}
    end.