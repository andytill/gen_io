
-module(gen_io).

-behaviour(gen_server).

-include("gen_io.hrl").

-record(state, {
    % the module we are wrapping with gen_io
    module :: atom(),

    % the state of the module we're wrapping
    state  :: any()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
    [
    child_spec/1,
    start_link/1
    ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec child_spec(#gen_io_spec{}) -> supervisor:child_spec().
child_spec(#gen_io_spec{ spec_id = Id, shutdown = Shutdown } = Spec) ->
    {
        ?MODULE, 
        {?MODULE, start_link, [Spec]},
        permanent, 
        Shutdown, 
        worker, 
        [?MODULE]
    }.

start_link(#gen_io_spec{ registered_name = Name, args = Args } = Spec) ->
    try
        case Name of
            undefined -> gen_server:start_link(?MODULE, Spec, []);
            Name      -> gen_server:start_link(Name, ?MODULE, Spec, [])
        end
    catch
        C:R -> error_logger:error_msg("ERROR starting ~p ~p:~p ~p~n", [Spec, C, R, erlang:get_stacktrace()])
    end.


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(#gen_io_spec{ module = Module, args = Args }) when is_atom(Module) ->
    {ok, State} = Module:init(Args),
    {ok, #state{ module = Module, state = State }}.

handle_call(Request, From, #state{ module = M, state = S0 } = State) ->
    S1 = case M:handle_call(Request, From, S0) of 
        {reply, S1}           -> S1;
        {reply, {io, IO}, S1} -> process_io(M, IO), S1
    end,

    {noreply, State#state{ state = S1 }}.

handle_cast(Msg, #state{ module = M, state = S0 } = State) ->
    S1 = case M:handle_cast(Msg, S0) of 
        {noreply, S1}           -> S1;
        {noreply, {io, IO}, S1} -> process_io(M, IO), S1
    end,

    {noreply, State#state{ state = S1 }}.

handle_info(Info, #state{ module = M, state = S0 } = State) ->
    S1 = case M:handle_info(Info, S0) of 
        {noreply, S1}           -> S1;
        {noreply, {io, IO}, S1} -> process_io(M, IO), S1
    end,

    {noreply, State#state{ state = S1 }}.

terminate(Reason, #state{ module = M, state = S }) ->
    M:terminate(Reason, S).

code_change(OldVsn, #state{ module = M, state = S0 } = State, Extra) ->
    {ok, S1} = M:code_change(OldVsn, S0, Extra),
    {ok, State#state{ state = S1 }}.

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
