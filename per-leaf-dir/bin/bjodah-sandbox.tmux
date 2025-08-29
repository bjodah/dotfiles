unbind C-b
set -g prefix 'C-\'
bind 'C-\' send-prefix
set -g mouse on
set -g default-terminal "screen-256color"
set -g status-style "bg=#604050"
set -g remain-on-exit on  # cf. respawn-pane & respawn-pane -k
set -g history-limit 15000  # default is 2000 lines
