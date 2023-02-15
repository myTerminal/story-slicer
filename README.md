# story-slicer

[![Built with Lisp](https://img.shields.io/badge/built%20with-Lisp-blueviolet)](https://lisp-lang.org)
[![License](https://img.shields.io/github/license/myTerminal/story-slicer.svg)](https://opensource.org/licenses/MIT)  
[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/Y8Y5E5GL7)

A command-line tool to slice videos for social-media stories 

## Installation

There are a few different ways to get *story-slicer*.

### Compile from source

    # Clone project to the local workspace
    git clone https://github.com/myTerminal/story-slicer.git

    # Switch to the project directory
    cd story-slicer

    # Install with `make`
    make install

### Through a package manager

*story-slicer* will soon be available to install from your operating system's package manager.

## How to Use

A simple way to use *story-slicer* is to run it in a command-line terminal passing in the path of the video file to be sliced and the output directory path.

    story-slicer ~/Videos/input.mp4 ~/Downloads

By default, the video file `input.mp4` is split into several videos of 30 seconds in length and placed in the specified output directory. You can also optionally specify the length of the individual slices as the second argument to the command.

    story-slicer ~/Videos/input.mp4 ~/Downloads 60

The above command would split the video into several videos of 60 seconds each.

One can also provide a start offset, which is the number of seconds to ignore from the input video. So, the above command would turn into:

    story-slicer ~/Videos/input.mp4 ~/Downloads 60 15

and it would ignore the first 15 seconds of the input video.

Just the way you can skip a part of the video from the start, you can also omit a part towards the end.

    story-slicer ~/Videos/input.mp4 ~/Downloads 60 15 250

The above command splits the input video `input.mp4` into slices that are 60 seconds long, but skips the first 15 seconds of the input video, and ends the splitting process at 250 seconds.

### Further help with commands

To learn more about usage, refer to `manpage`:

    man story-slicer

## Updating

In order to update *story-slicer*, simply run:

    story-slicer-update

## Uninstalling

In order to uninstall *story-slicer*, simply run:

    story-slicer-uninstall

## External Dependencies

Being written with Common Lisp, *story-slicer* depends on [SBCL](https://www.sbcl.org). In most cases, it will be automatically installed while generating the binary, but if it doesn't please install it before running the installation.

The other required programs are as follows:

 - [ffmpeg](https://ffmpeg.org)

## To-do

* Implement installation of ffmpeg automatically
* Implement detection of file extension
* Implement padding of slice numbers
* Re-organize/simplify code
