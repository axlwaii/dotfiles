" setup vundle:  $ git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
"
source ~/.vim-rspec

 set nocompatible                    " be iMproved

 if has("gui_macvim")
   set guioptions-=T                 "if using a GUI Version
 endif

 set mouse=a                         " Mouse support
 set clipboard=unnamed

 set guifont=DejaVu\ Sans\ Mono\ for\ Powerline:h14
 set fillchars+=stl:\ ,stlnc:\

 set encoding=utf-8                  " encoding UTF-8
 set t_Co=256
 " colorscheme distinguished
 " colorscheme kolor
 set noerrorbells visualbell t_vb=   "disabble these anoying error bells
 set backspace=indent,eol,start
 set hls                             " highlight search
 syntax on

 set ts=2                            " Tabs are 2 spaces
 set bs=2                            " Backspace over everything in insert mode
 set shiftwidth=2                    " Tabs under smart indent
 set autoindent
 " set smarttab
 set expandtab
 set number                          " always show linenumbers

 set cursorline

 set laststatus=2                    " always show statusline

 set statusline=%F%m%r%h%w\
 set statusline+=%{fugitive#statusline()}\
 set statusline+=[%{strlen(&fenc)?&fenc:&enc}]
 set statusline+=\ [line\ %l\/%L]
 set statusline+=%{rvm#statusline()}

 set nolist                          " dont show whitespace as dots
 set hidden

 let mapleader = ","
"set nobackup                       " no backup
 set noswapfile                     " we don't need no ...
 set backupdir=~/.vim/tmp

 filetype off                        " required!

 set rtp+=~/.vim/bundle/vundle/
 call vundle#rc()

 " let Vundle manage Vundle
 " required!
 Bundle 'gmarik/vundle'

 " My Bundles here:
 "
 " original repos on github
 Bundle 'altercation/vim-colors-solarized'
 Bundle 'kchmck/vim-coffee-script'
 Bundle 'mattn/zencoding-vim'
 " Bundle 'ap/vim-css-color'
 Bundle 'tpope/vim-rails.git'
 Bundle 'tpope/vim-rvm'
 Bundle 'tpope/vim-fugitive'
 Bundle 'Yggdroot/indentLine'
 Bundle 'tomtom/tcomment_vim'
 Bundle 'Shougo/unite.vim'
 Bundle 'epmatsw/ag.vim'
 Bundle 'scrooloose/nerdtree'
 Bundle 'scrooloose/syntastic'
 Bundle 'Shougo/vimproc.vim'
 Bundle 'ervandew/supertab'
 " vim-scripts repos
 " Bundle 'L9'
 " non github repos

 filetype plugin indent on     " required!
 "
 " Brief help
 " :BundleList          - list configured bundles
 " :BundleInstall(!)    - install(update) bundles
 " :BundleSearch(!) foo - search(or refresh cache first) for foo
 " :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
 "
 " see :h vundle for more details or wiki for FAQ
 " NOTE: comments after Bundle command are not allowed..

 "Plugin specific configurations
 let g:NERDTreeWinPos = "right"
 let g:Powerline_stl_path_style = 'short'
 let g:indentLine_color_term = 157

 let g:unite_source_history_yank_enable = 1
 let g:unite_source_history_yank_enable = 1


 let g:unite_source_file_mru_limit = 20
 " Solarized
 let g:solarized_termcolors=256
 set background=dark
 colorscheme solarized

 hi statusline ctermbg=White ctermfg=6

 call unite#filters#matcher_default#use(['matcher_fuzzy'])

 nnoremap <leader>f :<C-u>Unite -start-insert file_rec/async<CR>
 nnoremap <leader>g :Unite grep:.<cr>
 nnoremap <leader>y :<C-u>Unite -no-split -buffer-name=yank    history/yank<cr>
 nnoremap <leader>b :<C-u>Unite -no-split -buffer-name=buffer  buffer<cr>
 nnoremap <leader>r :<C-u>Unite file_mru<CR>

  " Custom mappings for the unite buffer
  autocmd FileType unite call s:unite_settings()

  function! s:unite_settings()
    " Play nice with supertab
    let b:SuperTabDisabled=1
    " Enable navigation with control-j and control-k in insert mode
    imap <buffer> <C-j>   <Plug>(unite_select_next_line)
    imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
  endfunction


 "Plugin mappings
 map <leader>n :NERDTreeToggle <CR>
 " map <leader>f :CtrlP<CR>
 " map <leader>b :CtrlPBuffer<CR>
 " Clear search
 map <leader>c :noh<CR>

 """""""""""""""""
 "Code completion"
 """""""""""""""""

 autocmd FileType python set omnifunc=pythoncomplete#Complete
 autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
 autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
 autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
 autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
 autocmd FileType css set omnifunc=csscomplete#CompleteCSS
 autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
 autocmd FileType php set omnifunc=phpcomplete#CompletePHP
 autocmd FileType c set omnifunc=ccomplete#Complete
 autocmd FileType cs set omnifunc=syntaxcomplete#Complete

 "Get rid of whitespace after saving
 autocmd BufWritePre * :%s/\s\+$//e

