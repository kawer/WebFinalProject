# compile and install server pkgs

echo "Installing server packages..."
sh install.sh
echo "Server succesfully installed! :)\n"

# run server after install

BIN_PATH=~/go/bin/game_server
$BIN_PATH