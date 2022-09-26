# alarm-clock

[![MELPA](https://melpa.org/packages/alarm-clock-badge.svg)](https://melpa.org/#/alarm-clock)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

An alarm clock for Emacs

Derived from [wlemuel/alarm-clock](https://github.com/wlemuel/alarm-clock).

## Requirements

-   Emacs 24 or higher
-   [f](https://stable.melpa.org/#/f) file library; install with `(package-install 'f)
-   [mpg123](http://mpg123.org) (gnu/linux)
-   [terminal-notifier](https://github.com/julienXX/terminal-notifier) (macOS)
-   [notify-send](https://manpages.debian.org/stretch/libnotify-bin/notify-send.1.en.html) (gnu/linux)

## Get started

-   Get alarm-clock
    -   Manually download alarm-clock and set-up your load path.
-   To auto-start alarm-clock every time you open Emacs add these lines to your .emacs file:

          (require 'alarm-clock)

## Basic Usage

### `alarm-clock-set`

Set an alarm clock with the time following tips.
TIME can be "6"(6 seconds), "11 minutes", "10 hours", "11:40pm", etc.
MESSAGE will be shown when notifying at setting time.

### `alarm-clock-list-view`

Display the alarm clock list.
Use `a` to set a new alarm clock, `d` or `C-k` to delete current alarm
clock, `g` to refresh the view, and space (`' '`) to turn off
a ringing alarm (M-x alarm-clock-stop).

### `alarm-clock-stop`

Turn off the currently ringing alarm.

### `alarm-clock-save`

Save alarm-clock to cache file.

### `alarm-clock-restore`

Restore alarm-clock from cache file.

## Enhancements

This fork of
[wlemuel/alarm-clock](https://github.com/wlemuel/alarm-clock) add these
features:

-   Allow repeating the alarm clock sound multiple times,
    asynchornously, or until stopped via M-x alarm-clock-stop
    or by pressing the SPACE key in the alarm list window.
-   Allow alarm-clock-list-view to work even if there are no alarms,
    to allow using `a` to create a new one or SPACE to stop a ringing
    alarm.
-   Show time remaining until the alarm fires in the alarm list view.
    Press `g` to refresh (and update the time remaining).
-   Aligned the header-line-format with content
-   New customization variables in the `alarm-clock` group:
    - `alarm-clock-play-sound-repeat`: Number of times to repeat the
      sound when an alarm rings. Use M-x alarm-clock-stop to quiet the alarm.
    - `alarm-clock-play-auto-view-alarms`: If non-nil, display the alarm
    clock list when ringing an alarm, to allow using SPACE to run alarm-clock-stop

## Configurations

### Enable autosave-and-restore feature

-   Change cache file path by setting the variable `alarm-clock-cache-file` to any file path you like.
-   Add these lines to your `.emacs` file:

          (alarm-clock--turn-autosave-on)

## Q & A

-   Meet `(wrong-type-argument package-desc nil)` on Mac OSX.

    -   Install `gnu-tar`.

        > brew install gnu-tar

    -   Try to set `quelpa-build-tar-executable` to the path of `gtar`, (e.g "/usr/local/bin/gtar").

    -   Then reinstall this package.

## Appendix

I'd be glad to receive patches,
comments and your considered criticism.

_Have fun with alarm-clock!_
