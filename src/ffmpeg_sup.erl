-module(ffmpeg_sup).

-behaviour(supervisor).

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
  Supervisors = [{ffmpeg_server,
                  {ffmpeg_server, start_link, []},
                  permanent,
                  2000,
                  worker,
                  []
                }],
  {ok, {{one_for_one, 3, 10}, Supervisors}}.

