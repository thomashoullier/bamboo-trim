#!/bin/sh
# Create video from frames.

ffmpeg -framerate 2 -start_number 1 -i frames/%05d.png output.webm
