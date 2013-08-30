filetype on " recognize file type based on filename extension
filetype plugin on " load plugins directory
filetype plugin indent on

set tabstop=2
set softtabstop=2
set shiftwidth=2
set textwidth=120
set smarttab
set expandtab
set autoindent
set smartindent
retab "convert tabs to spaces

au BufNewFile,BufRead *.hbs set filetype=html

" remember history of cursor
set viminfo='10,\"100,:20,%,n~/.viminfo

" remove trailing whitespace
autocmd BufWritePre * :%s/\s\+$//e

set ofu=syntaxcomplete#Complete

" good binding for paste since i don't use F keys
set pastetoggle=,p

" remap : to ; to save hitting and holding space
nnoremap ; :

" Use Q for formatting the current paragraph (or selection)
vmap Q gq
nmap Q gqap

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

set list
set listchars=tab:>.,extends:#,nbsp:.
autocmd filetype html,xml set listchars-=tab:>.

" html and xml files can have tabs
autocmd filetype html,xml set listchars-=tab:>.

" enable syntax highlighting
syn on

" default colors
"colorscheme desert-warm-256
"colorscheme wombat
colorscheme jellybeans

if has("gui_running")
  "colorscheme wombat
  colorscheme jellybeans
  set guioptions=egmrt
  set guifont=Bitstream\ Vera\ Sans\ Mono:h14
  "set number
  set guioptions-=T " remove the nasty bar at the top
  set lines=46
  set showtabline=2
endif

function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

" indent in visual mode forward and backward
vmap <C-x> >gv
vmap <C-z> <gv

call pathogen#infect()
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" Fuzzy File Finder
let g:fuf_modesDisable = []
let g:fuf_mrufile_maxItem = 1000
let g:fuf_mrucmd_maxItem = 400
let g:fuf_mrufile_exclude = '\v\~$|\.(bak|sw[po])$|^(\/\/|\\\\|\/mnt\/)'

nnoremap <silent> <C-f>l     :FufLine<CR>
nnoremap <silent> <C-f><C-p> :FufFileWithFullCwd<CR>
nnoremap <silent> <C-p>      :FufFileWithCurrentBufferDir<CR>
nnoremap <silent> <C-f>p     :FufFile<CR>
nnoremap <silent> <C-f>D     :FufDirWithFullCwd<CR>
nnoremap <silent> <C-f>d     :FufDir<CR>

map ,f :FufFile **/<CR>

" similar to textmate ctrl-T
map <C-t> :FufFile **/<CR>

" use sudo to save file if needed
cmap w!! w !sudo tee % >/dev/null

" c++ with clang
let g:clang_user_options='|| exit 0'
let g:clang_complete_auto = 1
let g:clang_complete_copen = 1

" vim screen
"let g:ScreenImpl='Tmux' " I use tmux
"noremap <leader>S :ScreenShell " Open a shell/repl
"vnoremap <leader>s :ScreenSend<CR> " send current visual selection to a shell/repl
"noremap <leader>s :ScreenSend<CR> " send a whole buffer to shell/repl
"
