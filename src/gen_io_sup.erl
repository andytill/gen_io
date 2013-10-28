
-module(gen_io_sup).

-behaviour(supervisor).

-include("gen_io.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Specs = [
        gen_io_srv:child_spec(#gen_io_spec{
            module          = gen_io_tester,
            spec_id         = gen_io_tester,
            registered_name = {local, gen_io_tester}
        }) 
    ],
    {ok, { {one_for_one, 5, 10}, Specs} }.

