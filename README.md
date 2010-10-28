# Workgroups for Windows (for Emacs)

Workgroups is a session management package for GNU Emacs.  It allows
you to store and restore window configurations (called "workgroups"
because it's shorter and funnier), save them to and load them from
disk, morph between them and perform various other operations on them.

Here's what the Elisp info docs have to say about window
configurations `(info "(Elisp)Window Configurations")`:

> A 'window configuration' records the entire layout of one frame--all
> windows, their sizes, which buffers they contain, how those buffers
> are scrolled, and their values of point and the mark; also their
> fringes, margins, and scroll bar settings.  It also includes the
> value of `minibuffer-scroll-window'.  As a special exception, the
> window configuration does not record the value of point in the
> selected window for the current buffer.  Also, the window
> configuration does not record the values of window parameters; see
> *Note Window Parameters::."

The problem with Emacs' window configurations is that they're opaque C
types: you can't peer inside them in Emacs.  To get at the information
in a window configuration, you must restore it in a frame, then access
that frame's parameters.

Here's what the same info node has to say about window configuration
opacity:

> Other primitives to look inside of window configurations would make
> sense, but are not implemented because we did not need them."

Greeeeat.  Workgroups solves this problem by implementing an
independent window configuration object -- one that is translucent,
frobbable and serializable.  Workgroups' window configurations (called
"wconfigs") save all the settings listed above, and more.  For
instance, if a region is highlighted in `transient-mark-mode`, that
region will still be highlighted after restarting Emacs and restoring
that wconfig.  And wconfigs can be constructed programatically,
without the need to manipulate a live frame, vastly simplifying things
like frame morphing, window moving, frame reversing and other
operations.


## Getting Workgroups

The latest version of Workgroups can always be found
[here](http://github.com/tlh/workgroups.el). You can clone the repo by
running:

    git clone git://github.com/tlh/workgroups.el


## Installation

- Put `workgroups.el` somewhere on your Emacs load path

- Byte-compile `workgroups.el`.  This isn't required, but it'll speed
  some things up.  To do so, enter:

        M-x byte-compile-file RET /path/to/workgroups.el RET

- Add this line to your `.emacs` file:

        (require 'workgroups)


## Configuration

- Set your prefix key (or not).  The prefix key for Workgroups'
  commands defaults to `C-z`.  You could set it to `C-c w` like this:

        (setq wg-prefix-key (kbd "C-c w"))

  Workgroups saves the prefix key's current definition when it's
  enabled, and restores it when it's disabled, so you don't have to
  worry about stomping keydefs if you want to try out different
  prefixes.

- There are many other customization options.  See the customization
  section in the source for details, or use:

        M-x customize-group RET workgroups RET


## Usage

- Turn on `workgroups-mode` either by issuing the command:

        M-x workgroups-mode RET

  or by evaluating this form, which can be added to your `.emacs`
  file:

        (workgroups-mode 1)

  You should see "wg" in the minor mode list on the mode-line.

- To get started right away, hit `<prefix> ?` for a list of commands
  and their bindings.


## Tutorial

### Workgroup Creation

To start off, add a few workgroups.  Hit `<prefix> c` to issue the
command `wg-create-workgroup`, give it a name, hit `RET`, and a new
workgroup is created.  Maybe split the screen a few times with `C-x 2`
and `C-x 3`, and switch to different buffers in some of the windows to
make it unique.  Repeat this process a few times to create some
different workgroups.


### Workgroup Switching

`<prefix> v` issues the command `wg-switch-to-workgroup`.  This will
do a completing-read (with
[ido](http://www.emacswiki.org/emacs/InteractivelyDoThings) if it's
enabled) on the available workgroup names, and switch to the workgroup
with that name.  `<prefix> n` will switch to the workgroup rightward
in the workgroups list from the current workgroup, and `<prefix> p`
will switch to the one leftward in the list.  `<prefix> 0` through
`<prefix> 9` switch to the workgroup at that position in the
workgroups list.  Try switching between your workgroups now.


### Base and Working Configs

Window configs drift through use. Windows get resized, different
buffers get selected, point and mark change, and so on.  When you
switch from one workgroup to another, then back to the first, you want
it to be in the same state that you left it in so you don't lose your
place.  At the same time, it's convenient to be able to revert a
workgroup to a known-good state when it gets hoplessly mangled.  For
this reason, every workgroup actually consists of two wconfigs: a base
config and a working config [1].  The base config is the pristine
wconfig you'd like to be able to revert back to, and the working
config is whatever the frame happens to look like while you're using
it [2].  You set the base config to the working config with `<prefix>
u` -- `wg-update-workgroup` -- and you set the working config to the
base config with `<prefix> r` -- `wg-revert-workgroup`.  The two
commands are mirror images of each other: the former sets the base to
the working, and the latter sets the working to the base.  You can
update all workgroups' base configs to their working configs with
`<prefix> U` -- `wg-update-all-workgroups` -- and you can revert all
workgroups' working configs to their base configs with `<prefix> R` --
`wg-revert-all-workgroups`.  Update all your workgroups with `<prefix>
U` now.

[1] That's not entirely true: working configs are actually properties
of frames.  Every frame has its own working config for each workgroup.
This is because when working with multiple frames, one expects the
working config for that frame to remain the same.  If you move to
another frame and modify a workgroup's working config, then switch
back to the first frame, it doesn't feel right when the working config
changed while you were gone.  This may seem complicated, but in
practice it's very natural.  Base configs are the same across all
frames, though.

[2] Workgroups tracks working configs lazily: it doesn't update the
working config every time changes are made to the frame -- only when
the working config is requested by a function.


### Saving and Loading

Saving and loading was the original motivation for writing Workgroups.
You can save your workgroups to a file with `<prefix> C-s` --
`wg-save` -- and you can load workgroups from a file with `<prefix>
C-l` -- `wg-load`.  Save your workgroups now.

Once you have a file of saved workgroups, it's convenient to load
it on Emacs startup.  To do so you can add a line like this to
your `.emacs`:

    (wg-load "/path/to/saved/workgroups")

So your final `.emacs` setup may look something like this:

    (add-to-list 'load-path "/path/to/workgroups.el")
    (require 'workgroups)
    (setq wg-prefix-key (kbd "C-c a"))
    (workgroups-mode 1)
    (wg-load "/path/to/saved/workgroups")

The customization variable `wg-switch-on-load` controls whether to
automatically switch to the first workgroup when a file is loaded.  It
defaults to `t`, so when you add the above to your `.emacs` file, the
first workgroup in the file will automatically be switched to on Emacs
startup.


### Killing and Yanking

WRITE ME


### Cloning

WRITE ME


### Offsetting and Swapping

WRITE ME


### Switching to Buffers

WRITE ME


### Messaging

WRITE ME


### Frame Morph

WRITE ME


### Help

To bring up a help buffer listing all the commands and their bindings,
hit `<prefix> ?` -- `wg-help`.



## FAQ

**Q:** Why is it called "Workgroups"?

**A:** Mostly because it's funny, but it also makes sense.  I needed a
  name that would also work for the window configuration objects being
  manipulated.  Elscreen has "screens", which works well.  I couldn't
  call them "window configurations" because it's too long, and Emacs
  already uses that for something else.  It'd be misleading, too,
  since a workgroup is actually a named set of multiple wconfigs (a
  base config and a working config for each frame).  So "Workgroups"
  it is.  I'll have to do something special for the 3.11 release.

**Q:** Why should I use Workgroups instead of Elscreen?

**A:** Workgroups provides persistence, base/working configs,
  frame-morphing and other chrome, and cleaner code.  And it's
  maintained.

**Q:** What's difference between a window configuration, a wconfig and
a workgroup?

**A:** A "Window configuration" is Emacs' opaque internal
  representation of frame state.  "wconfigs" are Workgroups' own
  independent window configuration type.  And "workgroups" are a named
  set of multiple wconfigs (a base config and a working config for
  each frame).


## A Note On Application Buffers

Workgroups doesn't currently save the state of applications like ERC
or Gnus, though this is on the TODO list.  If you save a workgroup
that includes application buffers, and then you restore that workgroup
in another Emacs session before relaunching those applications and
buffers, Workgroups will just default to `*scratch*`.  To get back to
your saved state, launch those applications and buffers and hit
`<prefix> R` to `wg-revert-all-workgroups`.


## License

Copyright (C) 2010 tlh
Workgroups for Windows (for Emacs) is released under the GPL.
See the file `workgroups.el`.
