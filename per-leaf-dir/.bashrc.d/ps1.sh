case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

if [ "$color_prompt" = yes ]; then
    PS1='$(date "+%H:%M") ${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='$(date "+%H:%M") ${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi

unset color_prompt
