%% Copyright
-module(ffmpeg).
-author("ilia").

%% API
-export([start/0, stop/0]).

start() ->
  application:start(ffmpeg).

stop() ->
  application:stop(ffmpeg),
  application:unload(ffmpeg).