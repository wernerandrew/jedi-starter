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
