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

void init_packet(AVPacket *packet);

int init_resampler(AVCodecContext *input_codec_context, AVCodecContext *output_codec_context, SwrContext **resample_context);

int init_fifo(AVAudioFifo **fifo, AVCodecContext *output_codec_context);

int decode_convert_and_store(AVAudioFifo *fifo,
                                         AVPacket *packet,
                                         AVCodecContext *input_codec_context,
                                         AVCodecContext *output_codec_context,
                                         SwrContext *resampler_context,
                                         int *finished);

int load_and_encode(AVAudioFifo *fifo, AVCodecContext *output_codec_context, AVPacket *output_packet, int *got_packet_ptr, int *nb_samples);

int encode_audio_frame(AVFrame *frame, AVCodecContext *output_codec_context, AVPacket *output_packet, int *got_packet_ptr, int *nb_samples);

int convert_and_store(AVFrame *input_frame,
                        AVAudioFifo *fifo,
                        AVCodecContext *input_codec_context,
                        AVCodecContext *output_codec_context,
                        SwrContext *resampler_context);

#endif