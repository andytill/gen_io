-module(gen_io_tester).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, process_io/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Module) ->
    io:format("gen_io_tester:start_link ~p~n", [Module]),
    try gen_server:start_link({local, ?SERVER}, gen_io_srv, [Module], []) of 
        Result -> io:format("gen_io_tester:start_link RESULT ~p~n", [Result]), Result
    catch
        C:R -> io:format("gen_io_tester:start_link ERROR ~p:~p~n", [C, R])
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
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

