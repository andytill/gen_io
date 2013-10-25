-module(gen_io_srv).
-behaviour(gen_server).

-record(state, {
    module,
    state
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
    [
    child_spec/1,
    start_link/2
    ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(gen_io_spec, {
    module              :: atom(), 

    spec_id             :: atom(),

    args = []           :: list(),

    shutdown = 5000     :: integer(),

    registered_name     :: atom()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

child_spec(#gen_io_spec{ module = Module, shutdown = Shutdown } = Spec) ->
    {
        ?MODULE, 
        {?MODULE, start_link, [{Module, Spec}]},
        permanent, 
        Shutdown, 
        worker, 
        [?MODULE]
    }.

start_link(Module, #gen_io_spec{ args = Args } = Spec) ->
    io:format("gen_io_srv:start_link Module is ~p~n", [Module]),
    gen_server:start_link(?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
%application:start(gen_io).
init([Module]) when is_atom(Module) ->
    io:format("Init Module is ~p", [Module]),
    {ok, State} = Module:init([]),
    {ok, #state{ module = Module, state = State }};
init({Module, Args}) when is_atom(Module) ->
    io:format("Init Module is ~p", [Module]),
    {ok, State} = Module:init(Args),
    {ok, #state{ module = Module, state = State }}.

handle_call(_Request, _From, State) ->
    io:format("gen_io_srv:handle_call ~p~n", [State]),

    {reply, ok, State}.

handle_cast(_Msg, State) ->
    io:format("gen_io_srv:handle_cast ~p~n", [State]),
    {noreply, State}.

handle_info(Info, #state{ module = M, state = S0 } = State) ->
    io:format("gen_io_srv:handle_info ~p~n", [State]),
    S1 = case M:handle_info(Info, S0) of 
        {noreply, S1}           -> S1;
        {noreply, {io, IO}, S1} -> process_io(M, IO), S1
    end,

    {noreply, State#state{ state = S1 }}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

process_io(M, IOs) when is_list(IOs) ->
    % just fire and forget for _now_, no piping results
    [process_io2(M, IO) || IO <- IOs],
    ok.

process_io2(_, {func, Module, Function, Args}) ->
    erlang:apply(Module, Function, Args);
process_io2(Module, IO) ->
    Module:process_io(IO).
