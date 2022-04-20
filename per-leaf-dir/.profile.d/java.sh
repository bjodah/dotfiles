DIR=$(ls -d /usr/lib/jvm/adoptopenjdk-*/ 2>/dev/null | tail -n 1)
if [[ ! -z "$DIR" ]]; then
    export JAVA_HOME="$DIR"
fi
