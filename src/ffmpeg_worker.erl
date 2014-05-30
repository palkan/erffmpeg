-module(ffmpeg_worker).

-include_lib("erlmedia/include/video_frame.hrl").
-include("../include/video_frame_ff.hrl").
-include("../include/ffmpeg_worker.hrl").
-include("log.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, init_video/3, init_audio/5, transcode/2, finish/1]).

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

send_frame(Port, #video_frame{content = audio} = Frame) ->
  send(Port, transform_frame(Frame)).


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

%% gen_server callbacks

init(Options) ->
  Port = start_worker(),
  Bitrate = proplists:get_value(bitrate, Options),
  Sample_rate = proplists:get_value(sample_rate, Options),
  Channels = proplists:get_value(channels, Options),
  {ok, #ffmpeg_worker{port = Port, audio_output = #init_output{content = audio, codec = libfdk_aac, track_id = 2, options = [{bitrate, Bitrate}, {sample_rate, Sample_rate}, {channels, Channels}]}}}.

handle_call({init_audio, Codec, Config, SampleRate, Channels}, {Pid, _Ref}, #ffmpeg_worker{owner = undefined, port = Port, audio_output = Output} = State) ->
  Options = case Codec of
              speex ->
                [{sample_rate, 16000}, {channels, ev_to_av(Channels)}];
              _ ->
                [{sample_rate, ev_to_av(SampleRate)}, {channels, ev_to_av(Channels)}]
            end,
  Input = #init_input{content = audio, codec = ev_to_av(Codec), config = Config, options = Options},
  {reply, send_init(Port, Input, Output), State#ffmpeg_worker{owner = Pid, audio_input = Input}};

handle_call({init_audio, Codec, Config, SampleRate, Channels}, _From, #ffmpeg_worker{port = Port, audio_output = Output} = State) ->
  Options = case Codec of
              speex ->
                [{sample_rate, 16000}, {channels, ev_to_av(Channels)}];
              _ ->
                [{sample_rate, ev_to_av(SampleRate)}, {channels, ev_to_av(Channels)}]
            end,
  Input = #init_input{content = audio, codec = ev_to_av(Codec), config = Config, options = Options},
  {reply, send_init(Port, Input, Output), State#ffmpeg_worker{audio_input = Input}};

handle_call({init_video, Codec, Config}, {Pid, _Ref}, #ffmpeg_worker{owner = undefined, port = Port, video_output = Output} = State) ->
  Input = #init_input{content = video, codec = ev_to_av(Codec), config = Config},
  {reply, send_init(Port, Input, Output), State#ffmpeg_worker{owner = Pid, video_input = Input}};

handle_call({init_video, Codec, Config}, _From, #ffmpeg_worker{port = Port, video_output = Output} = State) ->
  Input = #init_input{content = video, codec = ev_to_av(Codec), config = Config},
  {reply, send_init(Port, Input, Output), State#ffmpeg_worker{video_input = Input}};

handle_call(finish, _From, #ffmpeg_worker{port = Port} = State) ->
  send(Port, {finish}),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({transcode, #video_frame{} = Frame}, #ffmpeg_worker{port = Port} = State) ->
  send_frame(Port, Frame),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({Port, Data}, #ffmpeg_worker{port = {program, Port}, owner = Owner, audio_output = AOutput, video_output = VOutput} = State) ->
  case handle_data(Data) of
    #video_frame_ff{content = audio} = Frame ->
      response_to_owner({accumulate_ffmpeg, transform_frame(Frame, AOutput)}, Owner),
      {noreply, State};
    #video_frame_ff{content = video} = Frame ->
      response_to_owner({accumulate_ffmpeg, transform_frame(Frame, VOutput)}, Owner),
      {noreply, State};
    closed ->
      hls_media:finish(Owner),
      {stop, normal, State};
    Else ->
      ?D(Else),
      {stop, ffmpeg_error, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

init_audio(Pid, Codec, Config, SampleRate, Channels) ->
  gen_server:call(Pid, {init_audio, Codec, Config, SampleRate, Channels}).

init_video(Pid, Codec, Config) ->
  gen_server:call(Pid, {init_video, Codec, Config}).

transcode(Pid, Frame) ->
  gen_server:cast(Pid, {transcode, Frame}).

finish(Pid) ->
  gen_server:call(Pid, finish).

transform_frame(#video_frame_ff{content = audio, pts = Pts, dts = Dts, codec = Codec, stream_id = Stream_id, flavor = keyframe, body = Body, next_id = Next_id}, #init_output{options = Options}) ->
  Bitrate = proplists:get_value(bitrate, Options),
  Sample_rate = proplists:get_value(sample_rate, Options),
  Channels = proplists:get_value(channels, Options),
  #video_frame{content = audio, pts = trunc(Pts), dts = trunc(Dts), codec = Codec, stream_id = Stream_id, flavor = frame, sound = {Channels, av_to_ev(Bitrate), av_to_ev(Sample_rate)}, body = Body, next_id = Next_id};

transform_frame(#video_frame_ff{content = audio, pts = Pts, dts = Dts, codec = Codec, stream_id = Stream_id, flavor = config, body = Body, next_id = Next_id}, #init_output{options = Options}) ->
  Bitrate = proplists:get_value(bitrate, Options),
  Sample_rate = proplists:get_value(sample_rate, Options),
  Channels = proplists:get_value(channels, Options),
  #video_frame{content = audio, pts = trunc(Pts), dts = trunc(Dts), codec = Codec, stream_id = Stream_id, flavor = config, sound = {Channels, av_to_ev(Bitrate), av_to_ev(Sample_rate)}, body = Body, next_id = Next_id}.

transform_frame(#video_frame{content = Content, dts = Dts, pts = Pts, stream_id = Stream_id, codec = Codec, flavor = Flavor, body = Body, next_id = Next_id}) ->
  #video_frame_ff{content = Content, dts = Dts, pts = Pts, stream_id = Stream_id, codec = Codec, flavor = Flavor, body = Body, next_id = Next_id}.

response_to_owner(Term, Owner) ->
  gen_server:cast(Owner, Term).

fetch(Port) ->
  fetch(Port, 2000).

fetch({program, Port}, Timeout) ->
  receive
    {Port, Data} ->
      handle_data(Data)
  after Timeout ->
    {error, timeout}
  end.

handle_data({exit_status, 0}) ->
  closed;

handle_data({exit_status, Code}) ->
  {exit, Code};

handle_data({data, Data}) ->
  erlang:binary_to_term(Data);

handle_data(Data) ->
  {ok, Data}.

ev_to_av(h264) -> libx264;
ev_to_av(aac) -> libfdk_aac;
ev_to_av(speex) -> libspeex;
ev_to_av(rate5) -> 5512;
ev_to_av(rate11) -> 11025;
ev_to_av(rate22) -> 22050;
ev_to_av(rate44) -> 44100;
ev_to_av(bit8) -> 8000;
ev_to_av(bit16) -> 16000;
ev_to_av(mono) -> 1;
ev_to_av(stereo) -> 2;
ev_to_av(Codec) -> Codec.

av_to_ev(libx264) -> h264;
av_to_ev(libfdk_aac) -> aac;
av_to_ev(libspeex) -> speex;
av_to_ev(5512) -> rate5;
av_to_ev(11025) -> rate11;
av_to_ev(22050) -> rate22;
av_to_ev(44100) -> rate44;
av_to_ev(8000) -> bit8;
av_to_ev(16000) -> bit16;
av_to_ev(Codec) -> Codec.