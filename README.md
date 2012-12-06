noob-arrows-mode.el
===================

Emacs training minor mode that replaces arrow key functionality with help services

Activating noob-arrows-mode or global-noob-arrows-mode turns the arrow keys into help buttons. This forces the novice Emacs user to use alternative navigation techniques (not that this is necessarily inherently superior to using the arrow keys... but knowing how to do this can't hurt). The mode causes a pre-specified help file to be shown whenever an arrow key is pressed, creating a convenient method for reminding users how to use Emacs core set of commands. The help window appears above, below, left, or right of the current window depending on which corresponding arrow key was pressed. The help file can be user-edited to allow for change as new skills are learned. Other useful key combinations such as C-up, C-down, etc. retain their original functionality. 

Usage
=====

After turning on the mode, pressing an arrow key will display a help buffer in the corresponding direction of the button press. Pressing another arrow key (or C-x 1) will hide the help buffer. This package comes with an example help file, but any plain text file will work. Thus, you can customize the contents of the help buffer to your needs. All you need to do is set the variable <code>noob-arrows-help-file</code> to a file path that identifies the text file. Typically, this would be set in one of your startup files (presumably ~/.emacs.d/init):

```lisp
(setq noob-arrows-help-file "~/.emacs.d/plugins/noob-arrows-mode.el/noob-arrows-help-file.txt")
```

Installation
============

Manual
------

Ensure that noob-arrows-mode.el is on the custom-theme-load-path and then require the package like this: 

```lisp
(add-to-list 'custom-theme-load-path "~/path/to/noob-arrows-mode.el")
(require 'noob-arrows-mode)
```

Next, you will need to either create a help file or use a premade file (one is included in this git repo). You must then set the variable <code>noob-arrows-help-file</code> as described above in your startup files. Restart Emacs and everythingYou can then toggle the mode on/off at any time by running one of two commands:
<code>M-x noob-arrows-mode</code> or <code>M-x global-noob-arrows-mode</code>

The first command will turn the minor mode on in the current buffer only. The second command will turn it on for all buffers. You can turn either version off by simply running the same command again.

Bugs & Improvements
===================

Please report any problems that you find, along with any suggestions or contributions. 
