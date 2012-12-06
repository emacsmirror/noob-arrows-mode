noob-arrows-mode.el
===================

Emacs training minor mode that replaces arrow key functionality with help services

Activating noob-arrows-mode or global-noob-arrows-mode turns the arrow keys into help buttons. This forces the novice Emacs user to use alternative navigation techniques (not that this is necessarily inherently superior to using the arrow keys... but knowing how to do this can't hurt). The mode causes a help buffer to be shown whenever an arrow key is pressed, creating a convenient method for reminding users how to use Emacs core set of commands. The help window appears above, below, left, or right of the current window depending on which corresponding arrow key was pressed. Default help contents are provided, but users can optionally set an arbitrary file for creating new help tips (see Installation). Other useful key combinations such as C-up, C-down, etc. retain their original functionality. 

Usage
=====

The command <code>M-x noob-arrows-mode</code> will turn the minor mode on in the current buffer only. The command <code>M-x global-noob-arrows-mode</code> will turn it on for all buffers. You can turn either version off by simply running the same command again.

After turning on the mode, pressing an arrow key will display a help buffer in the corresponding direction of the button press. Pressing another arrow key (or C-x 1) will hide the help buffer. The content of the help buffer is set by default, but can be overridden by setting the variable <code>noob-arrows-help-file</code> (see Installation: Optional Settings). Thus, you can customize the contents of the help buffer to your needs, and/or adjust its content as you begin to learn various commands.

Installation
============

Manual
------

Ensure that noob-arrows-mode.el is on the custom-theme-load-path and require the package by adding the following lines to your startup file (presumably, your startup file is located at ~/.emacs.d/init). You will need to substitute the code below with the appropriate file path:

```lisp
(add-to-list 'custom-theme-load-path "~/path/to/noob-arrows-mode.el")
(require 'noob-arrows-mode)
```

Restart Emacs and everything should be ready to use. 

Optional Settings
-----------------

If you would like to change the content of the help buffer you may do so by setting the variable <code>noob-arrows-help-file</code> in your startup files. To accomplish this you add the following lines to one of your startup file. You will need to substitute the code below with the appropriate file path:

```lisp
(setq noob-arrows-help-file "~/path/to/noob-arrows-help-file.txt")
``` 

Once set, noob-arrows-mode will show the contents of specified help file in the popup buffer. The is useful when the user has successfully learned the core commands and would like to have new tips displayed instead. If this variable is unset, or if the file doesn't exist, the default help settings will be shown.


Bugs & Improvements
===================

Please report any problems that you find, along with any suggestions or contributions. 
