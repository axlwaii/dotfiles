" setup vundle:  $ git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
" Setting up Vundle - the vim plugin bundler
  let iCanHazVundle=1
  let vundle_readme=expand('~/.vim/bundle/vundle/README.md')
  if !filereadable(vundle_readme)
      echo "Installing Vundle.."
      echo ""
      silent !mkdir -p ~/.vim/bundle
      silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
      let iCanHazVundle=0
  endif
" Setting up Vundle - the vim plugin bundler end

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

 " set cursorline

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

 set synmaxcol=256                  " syntax highlight is very slow in large columns

 filetype off                        " required!

 set rtp+=~/.vim/bundle/vundle/
 call vundle#rc()

 " let Vundle manage Vundle
 " required!
 Bundle 'gmarik/vundle'

 " My Bundles here:
 "
 " look
 Bundle 'altercation/vim-colors-solarized'
 Bundle 'noahfrederick/vim-hemisu'
 Bundle 'Yggdroot/indentLine'
 " languages 
 Bundle 'tpope/vim-haml'
 Bundle 'tpope/vim-rails.git'
 Bundle 'tpope/vim-rvm'
 Bundle 'mattn/zencoding-vim'
 Bundle 'thoughtbot/vim-rspec'
 " workflow 
 Bundle 'kien/ctrlp.vim'
 Bundle 'vim-scripts/EasyGrep'
 Bundle 'scrooloose/nerdtree'
 Bundle 'tomtom/tcomment_vim'
 Bundle 'ervandew/supertab'
 Bundle 'tpope/vim-fugitive'
 Bundle 'tpope/vim-surround'
 Bundle 'scrooloose/syntastic'
 Bundle 'junegunn/vim-easy-align'
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
 let g:indentLine_color_term = 157
 
 " Configure syntastic
 let g:syntastic_mode_map = {'mode': 'active', 'passive_filetypes': ['erlang', 'html'] }
 let g:syntastic_javascript_checkers = ['jshint']

 " Solarized
 let g:solarized_termcolors=256
 set background=dark
 colorscheme solarized

 hi statusline ctermbg=White ctermfg=6

 nnoremap <leader>f :CtrlP<CR>
 nnoremap <leader>r :CtrlPMRU<CR>
 nnoremap <leader>b :CtrlPBuffer<CR>

 map <leader>n :NERDTreeToggle <CR>
 imap <C-space> <Esc>

 " Clear search
 map <leader>c :noh<CR>

 " Rspec test runner
 map <Leader>t :call RunCurrentSpecFile()<CR>
 map <Leader>s :call RunNearestSpec()<CR>
 map <Leader>l :call RunLastSpec()<CR>
 map <Leader>a :call RunAllSpecs()<CR>
 
 """"""""""""""""""""
 "Filetype indention"
 """"""""""""""""""""
 autocmd FileType javascript setlocal shiftwidth=4 tabstop=4
 

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


function! <SID>StripTrailingWhitespaces()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

autocmd BufWritePre *.py,*.js,*.haml,*.rb,*.html,*.sass,*.scss :call <SID>StripTrailingWhitespaces()

