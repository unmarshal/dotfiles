source ~/.aliases
source ~/.colors
source ~/.amazon_keys
source ~/.lcd
source ~/.profile

# if TERM is xterm, make it xterm-256color
[ "$TERM" = "xterm" ] && TERM='xterm-256color'

BREW_PATH=`/usr/local/bin/brew --prefix`/bin:`/usr/local/bin/brew --prefix`/sbin
PATH=~/bin:~/.cabal/bin:$BREW_PATH:$PATH
PATH=$PATH:/Users/marshall/depot_tools
JAVA_HOME="$(/usr/libexec/java_home)"
DOCKER_HOST=tcp://$(boot2docker ip 2>/dev/null):2375
NODE_PATH=/usr/local/lib/node_modules
PATH=node_modules/.bin:~/node_modules/.bin:$PATH
TOTAL_HISTORY_CMDS=5000
HISTFILESIZE=$TOTAL_HISTORY_CMDS
HISTSIZE=$TOTAL_HISTORY_CMDS
HISTIGNORE="&:[ ]*:exit"
EDITOR=vim
PS1='$underline$fgcyan<\u@\h>$reset $bold$fgwhite[\t]$reset$fgcyan (\w)$reset $bold$fgpurple$(__git_ps1 "(%s)")$reset\n\$ '

HOMEBREW_GITHUB_API_TOKEN=49eb74b053a76d5cd9a75cb4a19a85a732e182f1

# Add google chromium deploy tools to path
PATH=~/deploy_tools:$PATH

ulimit -n 1024
# ulimit -c unlimited

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# docker initialization
eval "$(boot2docker shellinit 2>/dev/null)"

export PYENV_ROOT="${HOME}/.pyenv"

if [ -d "${PYENV_ROOT}" ]; then
    export PATH="${PYENV_ROOT}/bin:${PATH}"
    eval "$(pyenv init -)"
fi

# Load pyenv-virtualenv automatically by adding
# the following to ~/.bash_profile:

eval "$(pyenv virtualenv-init -)"
