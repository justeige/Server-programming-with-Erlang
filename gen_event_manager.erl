-module(gen_event_manager).
-compile(export_all).

%% ------------------------------------
%% example for an generic event manger
%% ------------------------------------

% start/2: spawn the manager process and register it with 'Name'
% EventHandlers is list of {Handler, Data} tuples where Handler := name of callback 
% and Data := data passed into the handlers init()
start(Name, EventHandlers) ->
    register(Name, spawn(gen_event_manager, init, [EventHandlers])), ok.

init(EventHandlers) -> 
    loop(initialize(EventHandlers)).

% initialize/1: for every handler -> call its init/1 
initialize([]) -> [];
initialize([{Handler, Data} | Rest]) ->
    [{Handler, Handler:init(Data)} | initialize(Rest)].

% stop/1: stop the event handler
stop(Name) ->
    Name ! {stop, self()},
    receive
        {reply, Reply} 
            -> Reply
    end.

% terminate/1: call terminate on every handler; return a list of {Handler, Value}
terminate([]) -> [];
terminate([{Handler, Data} | Rest]) ->
    [{Handler, Handler:terminate(Data)} | terminate(Rest)].

% reply/2: forward a message
reply(To, Msg) ->
    To ! {reply, Msg}.

%send_event/2
send_event(Name, Event) ->
    call(Name, {send_event, Event}).

% call/2: forward a message and return its reply
call(Name, Msg) ->
    Name ! {request, self(), Msg},
    receive 
        {reply, Reply} ->
            Reply
    end.

% add_handler/3: add a new handler
add_handler(Name, Handler, InitData) ->
    call(Name, {add_handler, Handler, InitData}).

% remove_handler/2
remove_handler(Name, Handler) ->
    call(Name, {delete_handler, Handler}).

% get_data/2: returns processed data from a handler
get_data(Name, Handler) ->
    call(Name, {get_data, Handler}).

% handle_msg/2: handle all requests except terminaton 
handle_msg({add_handler, Handler, InitData}, LoopData) ->
    { ok, [{Handler, Handler:init(InitData)} | LoopData]};
handle_msg({get_data, Handler}, LoopData) ->
    case lists:keysearch(Handler, 1, LoopData) of 
        false -> {{error, instance}, LoopData}; % can't find the data
        {value, {Handler, Data}} -> {{data, Data}, LoopData}
    end;
handle_msg({send_event, Event}, LoopData) ->
    {ok, event(Event, LoopData)};
handle_msg({delete_handler, Handler}, LoopData) ->
    case lists:keysearch(Handler, 1, LoopData) of
        false -> {{error, instance}, LoopData};
        {value, {Handler, Data}} ->
            Reply = {data, Handler:terminate(Data)},
            NewLoopData = lists:keydelete(Handler, 1, LoopData),
            {Reply, NewLoopData}
    end.

% event/1: forward an event to the handler callback
event(_Event, []) -> [];
event(Event, [{Handler, Data}|Rest]) ->
    [{Handler, Handler:handle_event(Event, Data)} | event(Event, Rest)].

% loop/1: handle message requests and termination
loop(State) ->
    receive
        {request, From, Message} ->
            {Reply, NewState} = handle_msg(Message, State),
            reply(From, Reply),
            loop(NewState);

        {stop, From} ->
            reply(From, terminate(State))
        end.