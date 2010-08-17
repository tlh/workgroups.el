# workgroups.el - workgroups for windows

## Commentary

workgroups-mode.el is a window configuration persistence minor mode
for GNU Emacs.  It allows you to persist window configurations, called
"workgroups" because it's shorter and funnier, between sessions.
workgroups-mode saves the window layout of the current frame, as well
as each window's buffer's filename if it's visiting a file, or its
buffername otherwise.  And that's it. It doesn't try to save
complicated information about the buffer, like major or minor modes.
If you save workgroups that include things like erc or gnus buffers,
you should launch those applications and buffers again in your next
session before restoring the workgroup that includes them. Nothing bad
will happen otherwise, of course.  workgroups-mode will just default
to a buffer that already exists, like \*scratch\*.

<tt>workgroups-list</tt> contains all the currently available workgroups.
You can switch to workgroups (i.e. restore window configurations),
bury them, go to the previous or next workgroup circularly, etc.
<tt>workgroups-save</tt> saves <tt>workgroups-list</tt> to a file, which can then be
loaded in another session.  Workgroups are added to <tt>workgroups-list</tt>
by calling <tt>workgroups-add</tt>, removed by calling <tt>workgroups-kill</tt>, and
can be moved to the end of <tt>workgroups-list</tt> by calling
<tt>workgroups-bury</tt>.  In general, operations on workgroups and
<tt>workgroups-list</tt> behave as similarly to buffers and buffer-lists as
possible.

## Installation:

 - Put <tt>workgroups-mode.el</tt> somewhere on your emacs load path

 - Add this line to your .emacs file:

        (require 'workgroups-mode)

## Configuration:

Once you've added a few workgroups with <tt>workgroups-add</tt>, you should
save them to a file with <tt>workgroups-save</tt>.  You can designate a file
to be automatically loaded when workgroups-mode is started by setting
<tt>workgroups-default-file</tt> like so:

    (setq workgroups-default-file "/path/to/workgroups/file")

If <tt>workgroups-autoswitch</tt> is non-nil, the first workgroup in a file
will automatically be switched to when the file is loaded:

    (setq workgroups-autoswitch t)

With these two options set, workgroups mode will automatically load
the default file and switch to the first workgroup in it at emacs
startup.

To turn on workgroups-mode, either issue the command:

    M-x workgroups-mode

Or put this in your .emacs file:

    (workgroups-mode t)

Check the documentation of the customizable variables below for more
configuration options.

## Some sample keybindings:

    (global-set-key (kbd "C-c w a") 'workgroups-add)
    (global-set-key (kbd "C-c w k") 'workgroups-kill)
    (global-set-key (kbd "C-c w b") 'workgroups-switch)
    (global-set-key (kbd "C-c w s") 'workgroups-save)
    (global-set-key (kbd "C-c w f") 'workgroups-find-file)
    (global-set-key (kbd "C-c w u") 'workgroups-update)
    (global-set-key (kbd "C-c w r") 'workgroups-revert)
    (global-set-key (kbd "C-c w j") 'workgroups-bury)
    (global-set-key (kbd "C-c w e") 'workgroups-show-current)
    (global-set-key (kbd "C-s-,")   'workgroups-previous)
    (global-set-key (kbd "C-s-.")   'workgroups-next)

## Or the ido versions if you use ido-mode:

    (global-set-key (kbd "C-c w a") 'workgroups-ido-add)
    (global-set-key (kbd "C-c w b") 'workgroups-ido-switch)
    (global-set-key (kbd "C-c w k") 'workgroups-ido-kill)

## License

workgroups.el is released under the GPL. See <tt>workgroups.el</tt>
