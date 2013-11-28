
-record(init_output, {
  content = undefined :: frame_content(),
  codec = undefined :: frame_codec(),
  track_id = undefined :: non_neg_integer(),
  options = []
}).

-record(init_input, {
  content = undefined :: frame_content(),
  codec = undefined :: frame_codec(),
  config = <<>> :: binary()
}).

-record(ffmpeg_worker, {
  owner = undefined,
  port,
  audio_input = undefined,
  audio_output,
  video_input = undefined,
  video_output,
  numbers = []
}).



