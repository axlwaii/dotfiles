 " ------------------------------------------ 
 " Setting up Vundle - the vim plugin bundler
 " ------------------------------------------

 let iCanHazVundle=1
 let vundle_readme=expand('~/.vim/bundle/vundle/README.md')

 if !filereadable(vundle_readme)
   echo "Installing Vundle.."
   echo ""
   silent !mkdir -p ~/.vim/bundle
   silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
   let iCanHazVundle=0
 endif

 "-------------------------------------------
 
 if has("gui_macvim")
   set guioptions-=T                 "if using a GUI Version
 endif

 "---------------
 " VUNDLE CONFIG 
 "---------------

 set nocompatible                    " be iMproved
 filetype off                        " required!

 set rtp+=~/.vim/bundle/vundle/
 call vundle#rc()

 " let Vundle manage Vundle
 " required!
 Bundle 'gmarik/vundle'

 " themes
 Bundle 'altercation/vim-colors-solarized'
 Bundle 'noahfrederick/vim-hemisu'
 Bundle 'jonathanfilip/vim-lucius'
 " Statusbar
 Bundle 'bling/vim-airline'
 Bundle 'edkolev/tmuxline.vim'
 Bundle 'Yggdroot/indentLine'
 " languages 
 Bundle 'tpope/vim-rails.git'
 Bundle 'thoughtbot/vim-rspec'
 Bundle 'tpope/vim-rvm'
 Bundle 'mattn/zencoding-vim'
 Bundle 'pangloss/vim-javascript'
 " workflow 
 Bundle 'kien/ctrlp.vim'
 Bundle 'scrooloose/nerdtree'
 Bundle 'tomtom/tcomment_vim'
 Bundle 'ervandew/supertab'
 Bundle 'tpope/vim-fugitive'
 Bundle 'scrooloose/syntastic'
 Bundle 'godlygeek/tabular'
 Bundle 'rking/ag.vim'

 filetype plugin indent on                              " required!

 " -----------------------
 " BASIC VIM CONFIGURATION
 " -----------------------

 syntax on

 let mapleader = ","

 set guifont=DejaVu\ Sans\ Mono\ for\ Powerline:h14
 set number                                             " always show linenumbers
 set backspace=indent,eol,start
 set noerrorbells visualbell t_vb=                      " disabble these anoying error bells
 set nolist                                             " dont show whitespace as dots
 set hidden
 set mouse=a                                            " Mouse support
 set clipboard=unnamed
 set wildmenu
 set wildmode=list:longest,full
 set fillchars+=stl:\ ,stlnc:\
 set encoding=utf-8                                     " encoding UTF-8
 set t_Co=256
 set hls                                                " highlight search
 set incsearch                                          " start searching with the first letter
 set ignorecase                                         " ignore case when searching
 set ts=2                                               " Tabs are 2 spaces
 set bs=2                                               " Backspace over everything in insert mode
 set shiftwidth=2                                       " Tabs under smart indent
 set autoindent
 set smarttab
 set expandtab
 set ruler
 set laststatus=2                                       " always show statusline
 set notimeout                                          " Fix lag in iTerm.
 set ttimeout
 set timeoutlen=50
 set nomodeline
 set nobackup                       " no backup
 set noswapfile                     " we don't need no ...
 set synmaxcol=256                  " syntax highlight is very slow in large columns

 " Solarized
 let g:solarized_termcolors=256
 
 " Default colorscheme
 set background=dark
 colorscheme lucius

 " --------------------
 " PLUGIN CONFIGURATION
 " --------------------

 let g:indentLine_color_term = 157
 
 " Configure syntastic
 let g:syntastic_mode_map = {'mode': 'active', 'passive_filetypes': ['erlang', 'html', 'json'] }
 let g:syntastic_javascript_checkers = ['jshint']

 " Airline
 let g:airline_powerline_fonts = 1
 let g:airline_solarized_reduced = 0
 let g:airline_powerline_fonts = 1
 let g:airline_theme = 'solarized'

 " CtrlP
 nnoremap <leader>f :CtrlPCurWD<CR>
 nnoremap <leader>F :CtrlP<CR>
 nnoremap <leader>r :CtrlPMRU<CR>
 nnoremap <leader>b :CtrlPBuffer<CR>

 " NERDTree
 let g:NERDTreeWinPos = "right"
 map <leader>n :NERDTreeToggle <CR>

 " Clear search
 map <leader>c :noh<CR>

 " Rspec test runner
 map <Leader>t :call RunCurrentSpecFile()<CR>

 " Switch colorscheme dark/light
 map <Leader>s :call SwitchColorScheme()<CR>

 map <Leader># :AgFromSearch<CR>

 " ------------------
 " Filetype indention
 " ------------------
 autocmd FileType javascript setlocal shiftwidth=4 tabstop=4
 autocmd FileType ruby setlocal shiftwidth=2 tabstop=2

 " ---------
 " Functions
 " ---------
 
 "Get rid of whitespace after saving
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

 function! SwitchColorScheme()
   if(&background == 'dark')
     set background=light
     colorscheme lucius 
   else
     set background=dark
     colorscheme lucius
   endif
 endfunction

 autocmd BufWritePre *.py,*.js,*.haml,*.rb,*.html,*.sass,*.scss :call <SID>StripTrailingWhitespaces()
