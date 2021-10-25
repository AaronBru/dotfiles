set nocompatible
filetype off

set rtp +=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
call vundle#end()

filetype plugin indent on

if !empty(glob("~/.vimrc_local"))
    source ~/.vimrc_local
endif
