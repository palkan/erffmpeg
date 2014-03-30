#ifndef AUDIO_TRANSCODE_H
#define AUDIO_TRANSCODE_H

#include <stdio.h>

#include "libavformat/avformat.h"
#include "libavformat/avio.h"

#include "libavcodec/avcodec.h"

#include "libavutil/audio_fifo.h"
#include "libavutil/avassert.h"
#include "libavutil/avstring.h"
#include "libavutil/frame.h"
#include "libavutil/opt.h"

#include "libswresample/swresample.h"

int init_resampler(AVCodecContext *input_codec_context, AVCodecContext *output_codec_context, SwrContext **resample_context);

int init_fifo(AVAudioFifo **fifo, AVCodecContext *output_codec_context);

int decode_convert_and_store(AVAudioFifo *fifo,
                                         AVPacket *packet,
                                         AVCodecContext *input_codec_context,
                                         AVCodecContext *output_codec_context,
                                         SwrContext *resampler_context,
                                         int *finished);

int load_encode_and_reply(AVAudioFifo *fifo, AVCodecContext *output_codec_context);

#endif