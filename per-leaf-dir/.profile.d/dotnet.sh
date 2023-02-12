if [ -d "$HOME/dotnet" ]; then
    export DOTNET_CLI_TELEMETRY_OPTOUT=1
    export DOTNET_ROOT="$HOME/dotnet"
    export PATH="$HOME/dotnet":$PATH
fi
