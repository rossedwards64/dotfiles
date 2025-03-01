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

export ZSH := `echo ${HOME}/.local/share/oh-my-zsh`
export ZSH_CUSTOM := `echo ${HOME}/.local/share/oh-my-zsh/custom`
export ZDOTDIR := `echo ${HOME}/.config/zsh`

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
 pandoc qbittorrent ark Thunar firefox starship tmux ruby-tmuxinator pass
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
 sway swaybg swayidle i3status-rust fuzzel wob imv greetd
 tuigreet pavucontrol grim slurp jq wl-clipboard gnome-keyring
 xdg-desktop-portal xdg-desktop-portal-wlr xdg-desktop-portal-gtk
''' + waybar + ' ' + swaync, "\n", " ")

greetd_config := '''

[terminal]
vt = 7

[default_session]
command = "tuigreet --cmd /usr/bin/start-sway -t -g \'WELCOME TO WORM LINUX\' --asterisks"
user = "_greeter"
'''

sway-desktop-file := '''

[Desktop Entry]
Name=Sway
Comment=An i3-compatible Wayland compositor
Exec=/usr/bin/start-sway
Type=Application
'''

default:
    @just --choose

stow:
    stow -R --ignore '{{ stow_ignore }}' .

setup-archlinux: setup install-yay

setup-void: setup install-xbps-templates setup-user-runsvdir

setup: install-packages configure-zsh install-wm install-flatpak setup-emacs stow
    
install-packages:
    {{ if distro_name == 'NixOS' { error(nixos_err_msg) } else { '' } }}
    sudo {{ pkg_install_cmd }} {{ base-pkgs }}

configure-zsh:
    {{ if distro_name == 'NixOS' { error(nixos_err_msg) } else { '' } }}
    echo 'export ZDOTDIR={{ ZDOTDIR }}' | sudo tee /etc/zsh/zshenv
    . /etc/zsh/zshenv
    sudo chsh -s /bin/zsh
    chsh -s /bin/zsh
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended --keep-zshrc || true
    git clone https://github.com/zsh-users/zsh-autosuggestions `${ZSH_CUSTOM}/plugins/zsh-autosuggestions` || true
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git `${ZSH_CUSTOM}/plugins/zsh-syntax-highlighting` || true
    mkdir -p ~/.local/share/zsh/
    touch ~/.local/share/zsh/history

install-wm:
    {{ if distro_name == 'NixOS' { error(nixos_err_msg) } else { '' } }}
    sudo {{ pkg_install_cmd }} {{ wm-packages }}
    ln -s '{{ justfile_directory() }}/.config/sway/{{ wm-config }}' '{{ justfile_directory() }}/.config/sway/config' || true
    sudo install -vm755 '{{ justfile_directory() }}/.config/sway/start-sway' /usr/bin
    sudo tee /etc/greetd/config.toml >/dev/null <<-EOF {{ greetd_config }}
    sudo tee /usr/share/wayland-sessions/sway.desktop >/dev/null <<-EOF {{ sway-desktop-file }}

install-quicklisp:
    {{ if distro_name == 'NixOS' { error(nixos_err_msg) } else { '' } }}
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)'

install-flatpak:
    {{ if distro_name == 'NixOS' { error(nixos_err_msg) } else { '' } }}
    flatpak remote-add --if-not-exists --user flathub https://dl.flathub.org/repo/flathub.flatpakrepo

setup-emacs:
	emacsclient --eval '(org-babel-tangle-file "{{ source_directory() }}/.config/emacs/config.org")'

# VOID SPECIFIC RULES

setup-user-runsvdir:
    {{ if distro_name != 'Void' { error(runit_err_msg) } else { '' } }}
    sudo ln -s '{{ justfile_directory() }}/sv/runsvdir-ross' /etc/sv || true; \
    sudo ln -s /etc/sv/runsvdir-ross /var/service || true; \
    mkdir -p '{{ home_directory() }}/.config/service'
    for s in '{{ justfile_directory() }}/.config/service/*'; do \
        ln -s $s '{{ home_directory() }}/.config/service' || true; \
    done

install-xbps-templates:
    {{ if distro_name != 'Void' { error(runit_err_msg) } else { '' } }}
    mkdir -p ~/Documents/programming/repos
    git clone https://github.com/void-linux/void-packages '{{ home_directory() }}/Documents/programming/repos/void-packages' || true
    '{{ home_directory() }}/Documents/programming/repos/void-packages/xbps-src' binary-bootstrap
    for t in '{{ justfile_directory() }}/xbps-templates/*'; do \
        cp -r $t '{{ home_directory() }}/Documents/programming/repos/void-packages/srcpkgs'; \
         '{{ home_directory() }}/Documents/programming/repos/void-packages/xbps-src' pkg $(basename $t); \
        sudo xbps-install --repository \
          '{{ home_directory() }}/Documents/programming/repos/void-packages/hostdir/binpkgs' \
          $(basename $t); \
    done

# ARCH SPECIFIC RULES

install-yay:
    {{ if distro_name != 'Arch Linux' { error(yay_err_msg) } else { '' } }}
    sudo pacman -S --needed git base-devel
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si
