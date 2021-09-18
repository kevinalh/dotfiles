
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/kevinalh/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

direnv hook fish | source

status is-login; and pyenv init --path | source
pyenv init --path | source

set PATH $HOME/.poetry/bin $PATH

source /opt/asdf-vm/asdf.fish

