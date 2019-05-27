# alarm-clock

[![MELPA](https://melpa.org/packages/alarm-clock-badge.svg)](https://melpa.org/#/alarm-clock)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

An alarm clock for Emacs

## Requirements

- Emacs 24 or higher
- [mpg123](http://mpg123.org) (gnu/linux)
- [terminal-notifier](https://github.com/julienXX/terminal-notifier) (macOS)
- [notify-send](https://manpages.debian.org/stretch/libnotify-bin/notify-send.1.en.html) (gnu/linux)

## Get started

- Via [MELPA](https://melpa.org).

- Get alarm-clock
  - Manually download alarm-clock and set-up your load path.
- To auto-start alarm-clock every time you open Emacs add these lines to your .emacs file:

        (require 'alarm-clock) ; Not needed if you use package.el

## Basic Usage

##### `alarm-clock-set`

Set an alarm clock with the time following tips.  
TIME can be "6"(6 seconds), "11 minutes", "10 hours", "11:40pm", etc.  
MESSAGE will be shown when notifying at setting time.

##### `alarm-clock-list-view`

Display the alarm clock list.  
Use `a` to set a new alarm clock, `C-k` to delete current alarm clock.

##### `alarm-clock-save`

Save alarm-clock to cache file.

##### `alarm-clock-restore`

Restore alarm-clock from cache file.

## Configurations

#### Enable autosave-and-restore feature.
- Change cache file path by setting the variable `alarm-clock-cache-file` to any file path you like.
- Add these lines to your .emacs file:

        (alarm-clock--turn-autosave-on)


## Q & A

- Meet `(wrong-type-argument package-desc nil)` on Mac OSX.

  - Install `gnu-tar`.

    > brew install gnu-tar

  - Try to set `quelpa-build-tar-executable` to the path of `gtar`, (e.g "/usr/local/bin/gtar").
  - Then reinstall this package.

## Appendix

I'd be glad to receive patches,
comments and your considered criticism.

_Have fun with alarm-clock!_
