if [ -d "$HOME/vc/anfilt/scripts" ]; then
    export PATH="$HOME/vc/anfilt/scripts:$PATH"
fi
if [ -d /opt/anfilt-cache ]; then
    export ANFILT_CACHE_DIR=/opt/anfilt-cache
fi
