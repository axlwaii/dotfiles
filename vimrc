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
 Plugin 'flazz/vim-colorschemes'
 Plugin 'vim-airline/vim-airline-themes'
 " Statusbar
 Plugin 'vim-airline/vim-airline'
 Plugin 'Yggdroot/indentLine'
 " config
 Plugin 'editorconfig/editorconfig-vim'
 " languages
 " l: ruby
 Plugin 'tpope/vim-rails.git'
 Plugin 'thoughtbot/vim-rspec'
 Plugin 'tpope/vim-rvm'
 " l: JS
 Plugin 'pangloss/vim-javascript'
 Plugin 'mxw/vim-jsx'
 Plugin 'isRuslan/vim-es6'
 " l: elixir
 Plugin 'elixir-lang/vim-elixir'
 " l: html
 Plugin 'mattn/emmet-vim'
 " l: elm
 Plugin 'lambdatoast/elm.vim'
 " l: css
 Plugin 'csscomb/vim-csscomb'
 " workflow
 Plugin 'vim-scripts/L9'
 Plugin 'roman/golden-ratio'
 Plugin 'terryma/vim-multiple-cursors'
 Plugin 'vim-scripts/AutoComplPop'
 Plugin 'kien/ctrlp.vim'
 Plugin 'scrooloose/nerdtree'
 Plugin 'tomtom/tcomment_vim'
 Plugin 'ervandew/supertab'
 Plugin 'Lokaltog/vim-easymotion'
 Plugin 'tpope/vim-fugitive'
 Plugin 'scrooloose/syntastic'
 Plugin 'godlygeek/tabular'
 Plugin 'rking/ag.vim'

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
 set wildignore=.git,*.swp,*/tmp/*
 set fillchars+=stl:\ ,stlnc:\
 "  set encoding=utf-8                                     " encoding UTF-8
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
 " let g:solarized_termcolors=256

 " Default colorscheme
 "  colorscheme harlequin
 colorscheme made_of_code

 " aliases
 command WQ wq
 command Wq wq
 command QA qa
 command Qa qa
 command W w
 command Q q

 " --------------------------------------------
 " Easier split navigation
 " --------------------------------------------
 " Use ctrl-[hjkl] to select the active split
 nmap <silent> <c-k> :wincmd k<CR>
 nmap <silent> <c-j> :wincmd j<CR>
 nmap <silent> <c-h> :wincmd h<CR>
 nmap <silent> <c-l> :wincmd l<CR>

 " --------------------
 " PLUGIN CONFIGURATION
 " --------------------

 " Speed Up neovim startup
 let g:python_host_skip_check = 1
 let g:python3_host_skip_check = 1

 let g:python_host_prog='/usr/local/bin/python2'
 let g:python3_host_prog='/usr/local/bin/python3'

 let g:indentLine_color_term = 157

 " Configure syntastic
 let g:syntastic_mode_map = {'mode': 'active', 'passive_filetypes': ['erlang', 'html', 'json'] }
 let g:syntastic_javascript_checkers = ['jshint']
 let g:syntastic_scss_checkers = ['scss_lint']
 let g:syntastic_check_on_open = 0
 let g:syntastic_check_on_wq = 0
 let g:syntastic_aggregate_errors = 1

 " Airline
 let g:airline_powerline_fonts = 1
 let g:airline_solarized_reduced = 0
 let g:airline_powerline_fonts = 1
 let g:airline_theme = 'bubblegum'

 " CtrlP
 let g:ctrlp_custom_ignore = {
   \ 'dir':  '\.git$\|\.yardoc\|public$|log\|tmp$',
   \ 'file': '\.so$\|\.dat$|\.DS_Store$'
   \ }

 " EditorConfig
 let g:EditorConfig_core_mode = 'external_command'
 let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']
 let g:EditorConfig_exec_path = '/usr/local/bin/editorconfig'

 " Systastic
 set statusline+=%#warningmsg#
 set statusline+=%{SyntasticStatuslineFlag()}
 set statusline+=%*

 let g:syntastic_always_populate_loc_list = 1
 let g:syntastic_auto_loc_list = 1

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
 map <Leader>0 :call SwitchColorScheme()<CR>
 map <Leader># :AgFromSearch<CR>

  " autocmd BufWritePre *.js,*.rb,*.scss :call <SID>StripTrailingWhitespaces()
 au BufRead,BufNewFile *.es6  set filetype=javascript
"  au BufNewFile,BufRead *.scss set filetype=css

 au FileType scss set omnifunc=csscomplete#CompleteCSS
 au FileType es6  set omnifunc=javascriptcomplete#CompleteJavascript

 " Omni Completion
 set completeopt=longest,menuone
"  set omnifunc=syntaxcomplete#Complete

 " ------------------
 " Filetype indention
 " ------------------
 "  autocmd FileType javascript setlocal shiftwidth=4 tabstop=4
 "  autocmd FileType ruby setlocal shiftwidth=2 tabstop=2

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
     colorscheme hemisu
   else
     set background=dark
     colorscheme hemisu
   endif
 endfunction

 autocmd BufWritePre *.js,*.rb,*.scss :call <SID>StripTrailingWhitespaces()
 au BufRead, BufNewFile *.es6 set filetype=javascript
