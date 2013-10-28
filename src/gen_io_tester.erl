-module(gen_io_tester).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
    [
    process_io/1
    ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% gen_server/gen_io Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    io:format("gen_io_tester:handle_call ~p~n", [State]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    io:format("gen_io_tester:handle_cast ~p~n", [State]),
    {noreply, State}.

handle_info(lol, State) ->
    io:format("gen_io_tester:handle_info ~p~n", [State]),
    {noreply, {io, [roflcopter]}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process_io(roflcopter) ->
    io:format(">>>> GOT A roflcopter~n", []).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

