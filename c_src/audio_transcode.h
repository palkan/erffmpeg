

static int init_resampler(AVCodecContext *input_codec_context, AVCodecContext *output_codec_context, SwrContext **resample_context);

static int init_fifo(AVAudioFifo **fifo, AVCodecContext *output_codec_context);

static int decode_convert_and_store(AVAudioFifo *fifo,
                                         AVPacket *packet,
                                         AVCodecContext *input_codec_context,
                                         AVCodecContext *output_codec_context,
                                         SwrContext *resampler_context,
                                         int *finished);

static int load_encode_and_reply(AVAudioFifo *fifo, AVCodecContext *output_codec_context);