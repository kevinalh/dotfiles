filetype plugin on
filetype indent on
set number
set ff=unix
set showcmd

" set nocompatible
set t_Co=16
set tags=./tags;/

set hidden

" Tabs
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

"colorscheme solarized

" Except for makefiles
autocmd FileType make setlocal noexpandtab

" From Django docs
autocmd FileType python set sw=4
autocmd FileType python set ts=4
autocmd FileType python set sts=4

" Pandoc plugin
let g:pandoc#syntax#conceal#use = 0
let g:pandoc#syntax#codeblocks#embeds#langs = ["ruby", "c", "cpp", "cmake", "haskell"]
let g:pandoc#modules#disabled=["spell", "folding"]

" Spell check
set spelllang=es
hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red

let g:ycm_collect_identifiers_from_tags_files = 1 " Let YCM read tags from Ctags file
let g:ycm_use_ultisnips_completer = 1 " Default 1, just ensure
let g:ycm_seed_identifiers_with_syntax = 1 " Completion for programming language's keyword
let g:ycm_complete_in_comments = 0 " Completion in comments
let g:ycm_complete_in_strings = 0 " Completion in string

let g:ycm_autoclose_preview_window_after_completion=1
let g:ycm_autoclose_preview_window_after_insertion=1

let g:ycm_server_python_interpreter='/usr/bin/python2'

if !exists("g:ycm_semantic_triggers")
    let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers['typescript'] = ['.']

" Syntastic recommended settings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Ignore E501
let g:syntastic_python_checkers=["flake8"]
let g:syntastic_python_flake8_args="--ignore=E501"

" use jshint
let g:syntastic_javascript_checkers = ['jshint']

" for highlighting issues
highlight SyntasticError ctermbg=224
highlight SyntasticWarning ctermbg=194

let mapleader = ","

" let g:solarized_visibility = "high"
" let g:solarized_contrast = "normal"

map <leader>n <plug>NERDTreeTabsToggle<CR>
nmap <leader>t :TagbarToggle<CR>

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

nmap { [
nmap } ]
omap { [
omap } ]
xmap { [
xmap } ]

" JavaScript Libraries Syntax
let g:used_javascript_libs = 'jquery'

" Toggles Syntastic error window
" http://stackoverflow.com/questions/17512794/toggle-error-location-panel-in-syntastic
function! ToggleErrors()
    let old_last_winnr = winnr('$')
    lclose
    if old_last_winnr == winnr('$')
        " Nothing was closed, open syntastic error location panel
        Errors
    endif
endfunction

nnoremap <silent> <leader>e :<C-u>call ToggleErrors()<CR>


call plug#begin()

" Make sure you use single quotes

" Previously used but not anymore

" Plug 'altercation/vim-colors-solarized'

" Plug 'morhetz/gruvbox'

" For project-specific configuration
"Plug 'LucHermitte/lh-vim-lib'
"Plug 'LucHermitte/local_vimrc'

" NERDTree
Plug 'scrooloose/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'

" YouCompleteMe
Plug 'Valloric/YouCompleteMe'
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable'}

" JavaScript
" Plug 'ternjs/tern_for_vim'

" TypeScript
" Plug 'Quramy/tsuquyomi'
Plug 'HerringtonDarkholme/yats.vim'

Plug 'scrooloose/syntastic'
Plug 'nvie/vim-flake8'
Plug 'tpope/vim-surround'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'eagletmt/ghcmod-vim'
Plug 'tpope/vim-sensible'
Plug 'majutsushi/tagbar'
Plug 'vim-airline/vim-airline'
" Plug 'othree/javascript-libraries-syntax.vim'
Plug 'elzr/vim-json'
Plug 'tpope/vim-unimpaired'
Plug 'lervag/vimtex'
Plug 'neovimhaskell/haskell-vim'

Plug 'editorconfig/editorconfig-vim'

" Markdown
" Plug 'plasticboy/vim-markdown'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

Plug 'godlygeek/tabular'

" For git support
Plug 'tpope/vim-fugitive'

" Semantic color
Plug 'jeaye/color_coded'

" For CMake
Plug 'jalcine/cmake.vim'

" C / C++
Plug 'vim-scripts/Conque-GDB'
Plug 'vim-scripts/STL-improved'

" Auto close brackets, etc
Plug 'cohama/lexima.vim'

" Add plugins to &runtimepath
call plug#end()

" set background=light
" let g:solarized_termcolors=256
syntax enable

" Function for stripping Trailing Whitespace
" http://unix.stackexchange.com/questions/75430/how-to-automatically-strip-trailing-spaces-on-save-in-vi-and-vim
function! StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfunction

" Whitelists some local configuration files
"call lh#local_vimrc#munge('whitelist', $HOME.'/workspace/AcceleratedC++')

" Line number colors
" set cursorline
" highlight LineNr ctermfg=245
highlight LineNr ctermfg=245
highlight CursorLine cterm=NONE ctermbg=NONE ctermfg=NONE guibg=NONE guifg=NONE
highlight CursorLineNr cterm=bold
set cursorline
" highlight CursorLineNr ctermfg=Yellow

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
"let g:airline_left_sep = '»'
"let g:airline_left_sep = '▶'
"let g:airline_right_sep = '«'
"let g:airline_right_sep = '◀'
"let g:airline_symbols.linenr = '␊'
"let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
"let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
"let g:airline_left_sep = ''
"let g:airline_left_alt_sep = ''
"let g:airline_right_sep = ''
"let g:airline_right_alt_sep = ''
"let g:airline_symbols.branch = ''
"let g:airline_symbols.readonly = ''
"let g:airline_symbols.linenr = ''

set laststatus=2
set mouse=a

