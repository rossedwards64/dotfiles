""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" venjiang's nvim config 
" http://blog.venjiang.com                       
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" =>General
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 行号
set nu
" 相对行号
set relativenumber
augroup relative_number
    autocmd!
    " autocmd InsertEnter * :set norelativenumber
    " autocmd InsertLeave * :set relativenumber
    autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
    autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
    " Fix syntax highlighting
    autocmd BufEnter * :syntax sync fromstart
augroup END
" 行号背景
hi LineNr ctermbg=235 
" 列标尺
" set colorcolumn=80
" hi ColorColumn ctermbg=237
" set autowrite
set updatetime=100
" 关闭错误提示音 
set vb t_vb=
" 鼠标支持
set mouse=a
" 退出后显示内容
" set t_ti= t_te=
" au BufWinLeave * silent mkview
" au BufWinEnter * silent loadview

" 高亮
hi Cursor cterm=reverse ctermbg=22
set cursorline
hi CursorLine   cterm=NONE ctermbg=237 ctermfg=none 
hi LineNr term=underline ctermfg=249 ctermbg=235 guifg=#b0b0b0
hi CursorLineNr term=bold ctermfg=214 gui=bold guifg=Yellow
hi SyntasticErrors cterm=none ctermbg=darkred
hi QuickFixLine cterm=none ctermfg=8 ctermbg=52
hi Normal ctermfg=254 ctermbg=234 guifg=#d0d0d0 guibg=#202020
hi MatchParen ctermfg=15 ctermbg=2 gui=underline guifg=#61AFEF
" hi MatchParen term=reverse ctermbg=90 guibg=#800080
" let g:airline_theme='onedark'
" 分隔条
hi VertSplit ctermfg=8 ctermbg=234 guifg=#e0e0e0 guibg=#000000
" 注释
hi Comment ctermfg=66  
" 选区
" hi Visual ctermbg=237 
" 非文本
" hi NonText  ctermfg=7
" guibg=76 guifg=white
" buffer next/previous
" map <leader>bl :bnext<cr>
" map <leader>bh :bprevious<cr>
" Fast saving

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
unmap <space>
unmap <c-space>
unmap <leader>n
" unmap <leader>p
unmap <leader>m
" map <leader>p :cp<cr>
" map <leader>n :lnext<CR>
" map <leader>m :lprevious<CR>
" map <c-n> :cnext<cr>
" map <c-m> :cprevious<cr>
map <c-n> :lnext<cr>
map <c-m> :lprevious<cr>
unmap <leader>pp
noremap <leader>c  :close<cr>
noremap oo  o<Esc>
" noremap ii  i<cr><Esc>
noremap <space><space>  i<space><Esc>
" 编辑
inoremap <c-f> <right>
inoremap <c-l> <right>
inoremap <c-b> <left>
inoremap <c-j> <down>
inoremap <c-k> <up>
inoremap <c-a> <home>
inoremap <c-e> <end>
inoremap <c-d> <delete>
" ESC
inoremap jk <Esc>
" inoremap <Esc> <Nop>

" tab
noremap <leader>1  :tabn1<cr>
noremap <leader>2  :tabn2<cr>
noremap <leader>3  :tabn3<cr>
noremap <leader>4  :tabn4<cr>
noremap <leader>5  :tabn5<cr>
noremap <leader>6  :tabn6<cr>
noremap <leader>7  :tabn7<cr>
noremap <leader>8  :tabn8<cr>
noremap <leader>9  :tabn9<cr>
" 终端
"
" open new split panes to right and below
" set splitright
" set splitbelow
" turn terminal to normal mode with escape
tnoremap <Esc> <C-\><C-n>
" start terminal in insert mode
au BufEnter * if &buftype == 'terminal' | :startinsert | endif
" open terminal on ctrl+n
function! OpenTerminal()
  botright split term://zsh
  resize 10
endfunction
nnoremap <c-n> :call OpenTerminal()<CR>
" use alt+hjkl to move between split/vsplit panels
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
" nnoremap <A-h> <C-w>h
" nnoremap <A-j> <C-w>j
" nnoremap <A-k> <C-w>k
" nnoremap <A-l> <C-w>l

" hi Terminal ctermfg=84
" noremap <silent> ;t  :terminal<cr>
" noremap <leader>t  :botright vertical terminal<cr>
" if has('nvim')
"     autocmd TermOpen term://* startinsert
"     autocmd TermEnter term://* startinsert
"     autocmd BufEnter term://* startinsert
" endif
" if has('nvim')
"     augroup terminal_setup | au!
"         autocmd TermOpen * nnoremap <buffer><LeftRelease> <LeftRelease>i
"     augroup end
" endif

" close comfortable-motion
let g:loaded_comfortable_motion = 0
" insert current datetime
iab xdate <c-r>=strftime("%Y-%m-%d %H:%M:%S")<cr>

" coc-highlighting
" autocmd CursorHold * silent call CocActionAsync('highlight')

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => clipboard
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <leader>a ggvG
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => 有道词典
" 在普通模式下，按 ctrl+t， 会翻译当前光标下的单词；
" 在 visual 模式下选中单词或语句，按 ctrl+t，会翻译选择的单词或语句；
" 点击引导键再点y，d，可以在命令行输入要翻译的单词或语句；
" 译文将会在编辑器底部的命令栏显示。
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vnoremap <silent> <C-T> :<C-u>Ydv<CR>
" nnoremap <silent> <C-T> :<C-u>Ydc<CR>
vnoremap <silent> <leader>yt :<C-u>Ydv<CR>
nnoremap <silent> <leader>yt :<C-u>Ydc<CR>
noremap <leader>yd :<C-u>Yde<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => go tests 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gotests_bin = '/Users/venjiang/gopath/bin/gotests'

""""""""""""""""""""""""""""""
" => vlang
""""""""""""""""""""""""""""""
au BufNewFile,BufRead *.v,*.vh	setf vlang

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"  others settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

