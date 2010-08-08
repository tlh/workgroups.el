# workgroups.el - workgroups for windows

## Commentary

workgroups.el is a simple window configuration persistence package.
It saves the window layout of the current frame, and, if a window's
buffer is visiting a file, it saves the filename as well.  And that's
it. It doesn't try to save complicated information about the buffer,
like major or minor modes. If you save configurations that include
things like erc or gnus buffers, you should launch those applications
and buffers again in your next session before restoring the
configuration that includes them. Nothing bad will happen otherwise,
of course -- workgroups will just default to a buffer that already
exist.

## Features

 - Saving window configurations

 - Restoring window configurations

 - Persisting window configurations across sessions


## Installation

 - Put <tt>workgroups.el</tt> somewhere on your emacs load path

 - Add this line to your <tt>.emacs</tt> file:

    (require 'workgroups)

## Configuration

 - to change the file that configs are saved in:

    (setq workgroups-configs-file "/path/to/new/file")

 - <tt>workgroups-restore-hook</tt> is a hook that's run whenever a
   workgroup is restored. You can add functions to it like this:

    (add-hook 'workgroups-restore-hook 'foo)


## Some sample keybindings

    (global-set-key (kbd "C-c C-g C-a") 'workgroups-add-config)
    (global-set-key (kbd "C-c C-g C-r") 'workgroups-restore-config)
    (global-set-key (kbd "C-c C-g C-d") 'workgroups-delete-config)
    (global-set-key (kbd "C-c C-g C-u") 'workgroups-update-config)
    (global-set-key (kbd "C-c C-g C-v") 'workgroups-revert-config)
    (global-set-key (kbd "C-s ,")       'workgroups-prev-config)
    (global-set-key (kbd "C-s .")       'workgroups-next-config)


Or if you use ido-mode:

    (global-set-key (kbd "C-c C-g C-a") 'workgroups-ido-add-config)
    (global-set-key (kbd "C-c C-g C-r") 'workgroups-ido-restore-config)
    (global-set-key (kbd "C-c C-g C-d") 'workgroups-ido-delete-config)

## License

workgroups.el is released under the GPL. See <tt>workgroups.el</tt>
