set unstable

distro_name := `cat /etc/os-release | sed -r '/^NAME=[^=]/!d;s/NAME=([^=])/\1/g;s/\"//g'`

nixos_err_msg := 'The goal of this rule is achieved through the declarative Nix config.'
yay_err_msg := "Since Yay is an AUR helper, it doesn't make sense to install it on a non-Arch system."
runit_err_msg := "This rule configures a Runit service, so it doesn't make sense to run it on a non-Void system."

stow_ignore := if distro_name == 'NixOS' {
    '^(\.local/bin/)|\.config/.*(?!emacs|nix(pkgs)?)$'
} else {
    '^(\.config/nix(pkgs)?)$'
}

wm-config := if `hostname` == 'ross-desktop' {
    'desktop'
} else {
    'laptop'
}

pkg_install_cmd := if which('pacman') != '' {
    'pacman -S'
} else if which('apt') != '' {
    'apt install'
} else if which('xbps-install') != '' {
    'xbps-install -S'
} else if which('nix') != '' {
    error('Install Nix packages declaratively only.')
} else {
    error('No supported package manager found.')
}

export ZSH := "${HOME}/.local/share/oh-my-zsh"

iosevka := if distro_name == 'Void' {
    'font-iosevka'
} else if distro_name == 'Arch' {
    'ttc-iosevka'
} else if distro_name == 'Ubuntu' {
    'fonts-iosevka'
} else {
    'iosevka'
}

base-pkgs := replace('''
 zsh alacritty emacs-pgtk eza ripgrep bat dust fzf rsync tree-sitter
 git rlwrap curl btop fzf tealdeer entr gimp libreoffice mpv neovim
 pandoc qbittorrent ark thunar firefox starship tmux tmuxinator pass
 pass-git-helper pass-otp pass-update flatpak lutris wine pipewire
 wireplumber sbcl guile clang leiningen clojure-lsp rustup
 xdg-user-dirs bash-language-server papirus-icon-theme platformio
 playerctl bc nerd-fonts
''' + iosevka, "\n", " ")

waybar := if distro_name == 'Void' {
    'Waybar'
} else {
    'waybar'
}

swaync := if distro_name == 'Void' {
    'SwayNotificationCenter'
} else if distro_name == 'Debian' {
    'sway-notification-center'
} else if distro_name == 'Ubuntu' {
    'sway-notification-center'
} else {
    'swaync'
}

wm-packages := replace('''
 sway swaybg swayidle swaylock i3status-rust fuzzel wob imv greetd
 tuigreet pavucontrol grim slurp jq wl-clipboard gnome-keyring
 xdg-desktop-portal xdg-desktop-portal-wlr xdg-desktop-portal-gtk
''' + waybar + ' ' + swaync, "\n", " ")

global-zshenv := 'export ZDOTDIR="$HOME/.config/zsh"'

default:
    @just --choose

stow:
    stow -R --ignore '{{ stow_ignore }}' .

setup-archlinux: setup install-yay

setup-void: setup setup-user-runsvdir

setup: install-packages configure-zsh install-wm install-flatpak setup-emacs

install-packages:
    {{ if distro_name == 'NixOS' { error(nixos_err_msg) } else { '' } }}
    sudo {{ pkg_install_cmd }} {{ base-pkgs }}

configure-zsh:
    {{ if distro_name == 'NixOS' { error(nixos_err_msg) } else { '' } }}
    sudo echo {{ global-zshenv }} > /etc/zsh/zshenv
    source /etc/zsh/zshenv
    sudo chsh -s "$(which zsh)"
    chsh -s "$(which zsh)"
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
    git clone https://github.com/zsh-users/zsh-autosuggestions "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions"
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"

install-wm:
    {{ if distro_name == 'NixOS' { error(nixos_err_msg) } else { '' } }}
    sudo {{ pkg_install_cmd }} {{ wm-packages }}
    ln -s '{{ source_directory() }}/.config/sway/config' '{{ source_directory() }}/.config/sway/{{ wm-config }}'
    sudo install -vm755 '{{ source_directory() }}/.config/sway/start-sway' /usr/bin

install-quicklisp:
    {{ if distro_name == 'NixOS' { error(nixos_err_msg) } else { '' } }}
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)'

install-yay:
    {{ if distro_name != 'Arch Linux' { error(yay_err_msg) } else { '' } }}
    sudo pacman -S --needed git base-devel
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si

install-flatpak:
    {{ if distro_name == 'NixOS' { error(nixos_err_msg) } else { '' } }}
    flatpak remote-add --if-not-exists --user flathub https://dl.flathub.org/repo/flathub.flatpakrepo

setup-user-runsvdir:
    {{ if distro_name != 'Void' { error(runit_err_msg) } else { '' } }}
    sudo ln -s '{{ source_directory() }}/sv/runsvdir-ross' /etc/sv

setup-emacs:
	emacsclient --eval '(org-babel-tangle-file "{{ source_directory() }}/.config/emacs/config.org")'
