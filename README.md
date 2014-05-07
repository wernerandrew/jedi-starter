Jedi Starter Kit
================

This starter kit lets you play with Jedi from the safety of a
virtual machine.  You'll need Vagrant and VirtualBox to get this
working.  I haven't checked exhaustively, but it seems to work
fine with VirtualBox 4.2.16 and Vagrant 1.4.3.

### Installation

1.  Clone the repository:

        $ git clone git@github.com:wernerandrew/jedi-starter.git

2.  Provision the VM and ssh in:

        $ cd jedi-starter
        $ vagrant up
        $ vagrant ssh

3.  Open Emacs, which should install the packages.

4.  Execute `M-x jedi:install-server` from within Emacs.

The provisioning step will softlink `~/.emacs` in the VM to
`jedi-starter.el` in your shared directory.  You can edit that
file if you want to try out tweaks to the default config.

### Using Jedi

This version includes my (Drew) preferred keybindings for
jedi-mode.  Additionally, the in-function tooltip pops up
only on demand (a bit of a hack, via the `jedi:get-in-function-call-delay`
variable).  If you want to stick with the defaults, you can comment out
or remove the following lines from jedi-starter.el:

``` lisp
(setq jedi:get-in-function-call-delay 10000000)
(add-hook 'python-mode-hook 'jedi-config:setup-keys)
```

The major keybinds (default and custom) can be summarized as follows:

Command                         | Default | Custom
--------------------------------|:-------:|:-----:
jedi:goto-definition            | C-c .   | M-.
jedi:goto-definition-pop-marker | C-c ,   | M-,
jedi:show-doc                   | C-c ?   | M-?
jedi:get-in-function-call       | None    | M-/

### Tips for non-virtualenv users

If you can't use virtualenv (which may be the case for
Anaconda users), you may want to try the following:

1.  You'll still need pip to install some dependencies; try `easy_install pip` if you don't have it.

2.  You'll need to install jedi and epc manually:

        $ pip install epc
        $ pip install jedi
    
3.  Right after `(require 'jedi)` in your init file, include the following:

        (setq jedi-config:use-system-python t)

(Note that this all assumes you're using substantially all of the 
configuration code in `jedi-config.el`, including helper functions
and defined config variables.)
