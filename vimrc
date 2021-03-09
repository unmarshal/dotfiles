set termguicolors
highlight Comment cterm=italic gui=italic

filetype on " recognize file type based on filename extension
filetype plugin on " load plugins directory
filetype plugin indent on

" jellybeans is the best colorscheme for the terminal
colorscheme jellybeans
"colorscheme xoria256
"colorscheme argonaut

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
set incsearch
set backspace=2
set encoding=utf8
set ruler
set laststatus=2 " always enable status line

" set cpp tabs to 2 spaces for chromium project
autocmd filetype cpp setlocal tabstop=2 softtabstop=2 shiftwidth=2

" set py tabs to 3 spaces
autocmd filetype python setlocal tabstop=3 softtabstop=3 shiftwidth=3

" set javascript tabs to 3 spaces
autocmd filetype javascript setlocal tabstop=3 softtabstop=3 shiftwidth=3

" set yaml tabs to 3 spaces
autocmd filetype yaml setlocal tabstop=3 softtabstop=3 shiftwidth=3

" Make highlight search toggle
set hlsearch!
nnoremap <F3> :set hlsearch!<CR>

" Turn off highlight search on startup
set nohlsearch

" Make line numbers toggle
set number!
nnoremap <F2> :set number!<CR>

" Open nerd tree with \[
nnoremap <Leader>] :NERDTreeToggle<CR>

" Turn on numbers at startup
set nonumber " enable line numbers

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


let mapleader = "\<Space>"

nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

" remap : to ; to save hitting and holding space
nnoremap ; :

" Use Q for formatting the current paragraph (or selection)
vmap Q gq
nmap Q gqap

" Tab navigation
noremap <Leader>k :tabprevious<CR>
noremap <Leader>j :tabnext<CR>
noremap <Leader>t :tabnew<CR>
"noremap <-t> :tabnew<CR>

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

" Force file extensions to be opened as type
au BufNewFile,BufRead *.hbs set filetype=html
au BufNewFile,BufRead *.html.* set filetype=html
au BufNewFile,BufRead *.jst.* set filetype=html
au BufNewFile,BufRead *.hql set filetype=hive
au BufNewFile,BufRead *.q set filetype=hive
au BufNewFile,BufRead Gemfile* set filetype=ruby
au BufNewFile,BufRead *.json set filetype=javascript
au BufNewFile,BufRead Vagrantfile set filetype=ruby
au BufNewFile,BufRead *.g4 set filetype=antlr

" html and xml files can have tabs
autocmd filetype html,xml set listchars-=tab:>.

" remove trailing whitespace on write
autocmd BufWritePre * :%s/\s\+$//e

hi Normal guibg=NONE ctermbg=NONE

" let t:is_transparent = 0
" function! Toggle_transparent()
"     if t:is_transparent == 0
"         hi Normal guibg=NONE ctermbg=NONE
"         let t:is_transparent = 1
"     else
"         set background=dark
"         let t:is_tranparent = 0
"     endif
" endfunction
" nnoremap <F4> : call Toggle_transparent()<CR>
"
