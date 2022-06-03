
" plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let g:python3_host_prog='/usr/local/opt/python/libexec/bin/python'
let g:python3_host_prog='/opt/homebrew/bin/python3'
call plug#begin('~/.vim/plugged')
" common develpment
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'tpope/vim-commentary'
Plug 'rizzatti/dash.vim'
" git
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/gv.vim'
Plug 'machakann/vim-highlightedyank'
" golang development
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
" use coc-settings.json
Plug 'buoto/gotests-vim'
Plug 'sebdah/vim-delve'
" javascript
Plug 'yuezk/vim-js'
Plug 'maxmellon/vim-jsx-pretty'
" Plug 'pangloss/vim-javascript'
" Plug 'leafgarland/typescript-vim'
" Plug 'peitalin/vim-jsx-typescript'
" Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
Plug 'jparise/vim-graphql'
" flutter development
Plug 'dart-lang/dart-vim-plugin'
Plug 'udalov/kotlin-vim'
" Plug 'natebosch/vim-lsc'
" Plug 'natebosch/vim-lsc-dart'
" Plug 'thosakwe/vim-flutter'
" erlang
Plug 'vim-erlang/vim-erlang-runtime'
Plug 'vim-erlang/vim-erlang-omnicomplete'
" others development
Plug 'mattn/emmet-vim'
Plug 'elzr/vim-json'
Plug 'cespare/vim-toml'
Plug 'plasticboy/vim-markdown'
Plug 'fatih/vim-hclfmt'
Plug 'uarun/vim-protobuf'
Plug 'zah/nim.vim'
" Plug 'ludovicchabant/vim-gutentags'
" Plug 'skywind3000/gutentags_plus'
" Plug 'leafgarland/typescript-vim'
" Plug 'pangloss/vim-javascript'
Plug 'vim-scripts/nginx.vim'
Plug 'rust-lang/rust.vim'
Plug 'diepm/vim-rest-console'
Plug 'tomtom/tlib_vim'
" navigation
Plug 'scrooloose/nerdtree'
Plug 'vim-scripts/mru.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tacahiroy/ctrlp-funky'
Plug 'junegunn/goyo.vim', { 'for': 'markdown' }
Plug 'jlanzarotta/bufexplorer'
Plug 'Asheq/close-buffers.vim'
Plug 'mileszs/ack.vim'
Plug 'dense-analysis/ale'
Plug 'itchyny/lightline.vim'
" ranger
Plug 'rbgrouleff/bclose.vim'
Plug 'francoiscabrol/ranger.vim'
" edit
Plug 'easymotion/vim-easymotion'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'ollykel/v-vim'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'terryma/vim-expand-region'
Plug 'MarcWeber/vim-addon-mw-utils'
" Plug 'garbas/vim-snipmate'
Plug 'michaeljsmith/vim-indent-object'
" tools
Plug 'simeji/winresizer'
Plug 'ianva/vim-youdao-translater'
" Plug 'mhinz/vim-startify'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
" scheme
Plug 'vim-scripts/peaksea' 
Plug 'joshdick/onedark.vim'
Plug 'rakr/vim-one'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'cocopon/iceberg.vim'
Plug 'morhetz/gruvbox'
Plug 'mhartington/oceanic-next'
Plug 'tyrannicaltoucan/vim-deep-space'

call plug#end()

