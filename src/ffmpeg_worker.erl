
-module(ffmpeg_worker).

-include_lib("erlmedia/include/video_frame.hrl").
-include("../include/video_frame_ff.hrl").
-include("../include/ffmpeg_worker.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, cast_frame/2, init_ffmpeg/4]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link(Options) ->
  gen_server:start_link(?MODULE, Options, []).

start_worker() ->
  start_worker(filename:join(code:lib_dir(ffmpeg, priv), "flussonic_ffmpeg")).

start_worker(Path) ->
  Port = erlang:open_port({spawn_executable, Path}, [{packet,4},{arg0, "flussonic_ffmpeg"},binary,exit_status]),
  {program, Port}.

send({program, Port}, Term) ->
  erlang:port_command(Port, erlang:term_to_binary(Term)).

send_frame(#video_frame{content = audio} = Frame, #ffmpeg_worker{port = Port, audio_output = Output} = State) ->
  send(Port, transform_frame(Frame)),
  Reply  = case fetch(Port) of
    #video_frame_ff{} = NewFrame ->
      transform_frame(NewFrame, Output);
    Else ->
      {error, Else}
  end,
  {Reply, State}.

%% gen_server callbacks

init(Options) ->
  Port = start_worker(),
  Owner = proplists:get_value(owner, Options),
  Bitrate = proplists:get_value(bitrate, Options),
  Sample_rate = proplists:get_value(sample_rate, Options),
  Channels = proplists:get_value(channels, Options),
  {ok, #ffmpeg_worker{owner = Owner, port = Port, audio_output = #init_output{options = [{bitrate, Bitrate}, {sample_rate, Sample_rate}, {channels, Channels}]}}}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(#video_frame{} = Frame, State) ->
  response_to_owner(send_frame(Frame, State));

handle_cast({init, audio, Codec, Config}, #ffmpeg_worker{port = Port, audio_output = Output} = State) ->
  Input = #init_input{content = audio, codec = ev_to_av(Codec), config = Config},
  response_to_owner({send_init(Port, Input, Output), State#ffmpeg_worker{audio_input = Input}});

handle_cast({init, video, Codec, Config}, #ffmpeg_worker{port = Port, video_output = Output} = State) ->
  Input = #init_input{content = video, codec = ev_to_av(Codec), config = Config},
  response_to_owner({send_init(Port, Input, Output), State#ffmpeg_worker{audio_input = Input}});

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

cast_frame(Pid, Frame) ->
  gen_server:cast(Pid, Frame).

init_ffmpeg(Pid, Content, Codec, Config) ->
  gen_server:cast(Pid, {init, Content, Codec, Config}).

transform_frame(#video_frame_ff{content = audio, pts = Pts, dts = Dts, codec = Codec, stream_id = Stream_id, flavor = keyframe, body = Body, next_id = Next_id}, #init_output{options = Options}) ->
  Bitrate = proplists:get_value(bitrate, Options),
  Sample_rate = proplists:get_value(sample_rate, Options),
  Channels = proplists:get_value(channels, Options),
  #video_frame{content = audio, pts = Pts, dts = Dts, codec = Codec, stream_id = Stream_id, flavor = frame, sound = {Channels, av_to_ev(Bitrate), av_to_ev(Sample_rate)}, body = Body, next_id = Next_id}.

transform_frame(#video_frame{content = Content, dts = Dts, pts = Pts, stream_id = Stream_id, codec = Codec, flavor = Flavor, body = Body, next_id = Next_id}) ->
  #video_frame_ff{content = Content, dts = Dts, pts = Pts, stream_id = Stream_id, codec = Codec, flavor = Flavor, body = Body, next_id = Next_id}.

send_init(Port, Input, Output) ->
  send(Port, Input),
  case fetch(Port) of
    ready ->
      send(Port, Output),
      case fetch(Port) of
        #video_frame_ff{} = NewFrame ->
          transform_frame(NewFrame, Output);
        Else ->
          {error, Else}
      end;
    Else ->
      {error, Else}
  end.

response_to_owner({Term, #ffmpeg_worker{owner = Owner} = State}) ->
  gen_server:cast(Owner, Term),
  {noreply, State}.

fetch(Port) ->
  fetch(Port, 2000).

fetch({program, Port}, Timeout) ->
  receive
    {Port, {exit_status, 0}} -> closed;
    {Port, {exit_status, Code}} -> {exit, Code};
    {Port, {data, Data}} -> erlang:binary_to_term(Data);
    {Port, Data} -> {ok, Data}
  after Timeout ->
    {error, timeout}
  end.

ev_to_av(h264) -> libx264;
ev_to_av(aac) -> libfaac;
ev_to_av(speex) -> libspeex;
ev_to_av(rate5) -> 5512;
ev_to_av(rate11) -> 11025;
ev_to_av(rate22) -> 22050;
ev_to_av(rate44) -> 44100;
ev_to_av(bit8) -> 8000;
ev_to_av(bit16) -> 16000;
ev_to_av(Codec) -> Codec.

av_to_ev(libx264) -> h264;
av_to_ev(libfaac) -> aac;
av_to_ev(libspeex) -> speex;
av_to_ev(5512) -> rate5;
av_to_ev(11025) -> rate11;
av_to_ev(22050) -> rate22;
av_to_ev(44100) -> rate44;
av_to_ev(8000) -> bit8;
av_to_ev(16000) -> bit16;
av_to_ev(Codec) -> Codec.