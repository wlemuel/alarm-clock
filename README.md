# alarm-clock
An alarm clock for Emacs

## Requirements
* Emacs 24 or higher
* [mpg123](http://mpg123.org) (gnu/linux)

## Get started

* Get alarm-clock
   * Manually download alarm-clock and set-up your load path.
   
* To auto-start alarm-clock every time you open Emacs and these lines to your .emacs file:

        (require 'alarm-clock) ; Not needed if you use package.el
        
## Basic Usage

##### `alarm-clock-set`

Set an alarm clock with the time following tips.  
TIME can be "6"(6 seconds), "11 minutes", "10 hours", "11:40pm", etc.  
MESSAGE will be shown when notifying at setting time.

##### `alarm-clock-list-view`

Display the alarm clock list.  
Use `a` to set a new alarm clock, `C-k` to delete current alarm clock.
