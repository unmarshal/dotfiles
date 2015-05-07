#!/bin/sh

set -e

VIM_BUNDLE_DIR=~/.vim/bundle

mkdir -p $VIM_BUNDLE_DIR
cd $VIM_BUNDLE_DIR

# Must Haves
git clone git@github.com:vim-scripts/FuzzyFinder.git
git clone git@github.com:vim-scripts/LustyJuggler.git
git clone git@github.com:scrooloose/nerdtree.git
git clone git@github.com:tomtom/tcomment_vim.git
git clone git@github.com:tomtom/tlib_vim.git
git clone git@github.com:pangloss/vim-javascript.git
git clone git@github.com:tpope/vim-sensible.git

git clone git@github.com:jlanzarotta/bufexplorer.git
git clone git@github.com:Lokaltog/vim-easymotion.git
git clone git@github.com:tpope/vim-fugitive.git
git clone git@github.com:clones/vim-l9.git
git clone git@github.com:tristen/vim-sparkup.git
git clone git@github.com:tsaleh/vim-supertab.git

# git clone git@github.com:vim-scripts/vim-addon-mw-utils.git

# Language Specific

git clone git@github.com:moll/vim-node.git
git clone git@github.com:Rip-Rip/clang_complete.git
git clone git@github.com:rust-lang/rust.vim.git
git clone git@github.com:fatih/vim-go.git
# git clone git@github.com:derekwyatt/vim-scala.git
# git clone git@github.com:motus/pig.vim.git
# git clone git@github.com:autowitch/hive.vim.git
# git clone git@github.com:kchmck/vim-coffee-script.git
# git clone git@github.com:vim-scripts/VimClojure.git
