filetype on " recognize file type based on filename extension
filetype plugin on " load plugins directory
filetype plugin indent on

" jellybeans is the best colorscheme for the terminal
"colorscheme jellybeans
colorscheme xoria256

syn on
retab
set modeline
set modelines=50
set tabstop=2
set softtabstop=2
set shiftwidth=2
set textwidth=120
set smarttab
set expandtab
set autoindent
set smartindent
set pastetoggle=<C-v> " good binding for paste since i do not use F keys
set ofu=syntaxcomplete#Complete
set viminfo='10,\"100,:20,%,n~/.viminfo " remember history of cursor
set list " display whitespace
set listchars=tab:>.,extends:#,nbsp:.
set hlsearch
set incsearch
set backspace=2
set encoding=utf8
set ruler
set laststatus=2 " always enable status line
set number " enable line numbers

set statusline=%t       "tail of the filename
set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
set statusline+=%{&ff}] "file format
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%y      "filetype
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file

" remap : to ; to save hitting and holding space
nnoremap ; :

" Use Q for formatting the current paragraph (or selection)
vmap Q gq
nmap Q gqap

" Tab navigation
noremap <C-k> :tabprevious<CR>
noremap <C-j> :tabnext<CR>
noremap <C-t> :tabnew<CR>

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


if version >= 700
  au InsertEnter * hi StatusLine term=reverse ctermbg=0 ctermfg=1 guibg=DarkGray guifg=Red
  au InsertLeave * hi StatusLine term=reverse ctermfg=0 ctermfg=0 guibg=DarkGray guifg=Black
  hi statusline term=reverse ctermbg=0 ctermfg=0 guibg=Black guifg=Gray
endif

" use scope if possible
if has('cscope')
  set cscopetag cscopeverbose

  if has('quickfix')
    set cscopequickfix=s-,c-,d-,i-,t-,e-
  endif

  cnoreabbrev csa cs add
  cnoreabbrev csf cs find
  cnoreabbrev csk cs kill
  cnoreabbrev csr cs reset
  cnoreabbrev css cs show
  cnoreabbrev csh cs help

  command -nargs=0 Cscope cs add $VIMSRC/src/cscope.out $VIMSRC/src
endif

" Force file extensions to be opened as type
au BufNewFile,BufRead *.hbs set filetype=html
au BufNewFile,BufRead *.html.* set filetype=html
au BufNewFile,BufRead *.jst.* set filetype=html
au BufNewFile,BufRead *.hql set filetype=hive
au BufNewFile,BufRead *.q set filetype=hive
au BufNewFile,BufRead Gemfile* set filetype=ruby
au BufNewFile,BufRead *.json set filetype=javascript

" html and xml files can have tabs
autocmd filetype html,xml set listchars-=tab:>.

" remove trailing whitespace on write
autocmd BufWritePre * :%s/\s\+$//e

" c++ with clang
let g:clang_user_options='|| exit 0'
let g:clang_complete_auto = 1
let g:clang_complete_copen = 1

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

" ctrl-f+t does textmate style file find
map <C-f>t :FufFile **/<CR>

if has("gui_running")
  " the wombat colorscheme is excellent for the gui
  " colorscheme wombat
  colorscheme rdark
  set guioptions=egmrt
  set guifont=Bitstream\ Vera\ Sans\ Mono:h14
  "set guifont=Menlo:h14
  set guioptions-=T " remove the nasty bar at the top
  set lines=46
  set showtabline=2
endif
