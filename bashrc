source ~/.aliases
source ~/.colors
source ~/.lcd
source ~/.profile

# if TERM is xterm, make it xterm-256color
[ "$TERM" = "xterm" ] && TERM='xterm-256color'

TOTAL_HISTORY_CMDS=5000
HISTFILESIZE=$TOTAL_HISTORY_CMDS
HISTSIZE=$TOTAL_HISTORY_CMDS
HISTIGNORE="&:[ ]*:exit"
EDITOR=vim
PS1='$underline$fgcyan<\u@\h>$reset $bold$fgwhite[\t]$reset$fgcyan (\w)$reset $bold$fgpurple$(__git_ps1 "(%s)")$reset\n\$ '

ulimit -n 1024
# ulimit -c unlimited

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi
