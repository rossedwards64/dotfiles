set runtimepath+=~/.config/vim

source ~/.config/vim/vimrcs/plugins.vim
source ~/.config/vim/vimrcs/basic.vim
source ~/.config/vim/vimrcs/filetypes.vim
source ~/.config/vim/vimrcs/plugins_config.vim
source ~/.config/vim/vimrcs/extended.vim
" my config
try
    source ~/.config/vim/vimrcs/my_configs.vim
catch
endtry
