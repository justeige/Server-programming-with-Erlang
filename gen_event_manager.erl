-module(gen_event_manager).
-export([start/2]).

%% ------------------------------------
%% example for an generic event manger
%% ------------------------------------

% start/2: spawn the manager process and register it with 'Name'
start(Name, EventHandlers) ->
    register(Name, spawn(gen_event_manager, init, [EventHandlers])), ok.

init(EventHandlers) -> 
    loop(initialize(EventHandlers)).

initialize([]) -> [];
initialize([{Handler, Data} | Rest]) ->
    [{Handler, Handler:init(Data)} | initialize(Rest)].


% loop/1: handle message requests and termination
loop(_State) ->
    receive
        {request, _From, _Message} ->
            ok;
        {stop, _From} ->
            ok
        end.