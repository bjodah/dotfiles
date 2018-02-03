notifyme(){
    start=$(date +%s)
    "$@"
    if [[ $? -eq 0 ]]; then
        title="Success"
    else
        title="Failure"
    fi
    [ $(($(date +%s) - start)) -le 15 ] || notify-send "$title" "Command\
 \"$(echo $@)\" took $(($(date +%s) - start)) seconds to finish"
}
