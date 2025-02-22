distro_name := `cat /etc/os-release | sed -r '/^NAME=[^=]/!d;s/NAME=([^=])/\1/g;s/\"//g'`

stow_ignore := if '{{ distro_name }}' == "NixOS" {
    '^(.local/bin/.*)|.config/(?!emacs|nix(pkgs)?)$'
} else {
    '^(.config/nix(pkgs)?)$'
}

default:
    just --list

deploy:
    stow -R --ignore '{{ stow_ignore }}' .

# TODO: install packages that i use on all machines, and set the
#       package manager commands depending on distro_name
# TODO: make zsh the default shell and install oh-my-zsh
# TODO: install things like greetd and start-sway script, as well as linking
#       the 'laptop' or 'desktop' portion of the sway script depending on
#       hostname
# TODO: if not on NixOS, install quicklisp
# TODO: set up windows 10 virtual machine
# TODO: if on arch, set up AUR

install:
    @echo 'Not implemented yet.'
