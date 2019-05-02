# install server packages

PROJECT_PATH=~/go/src/github.com/kawer/WebSnek/Server

cd "$PROJECT_PATH/utils/" && go install
cd "$PROJECT_PATH/database/level/" && go install
cd "$PROJECT_PATH/database/user/" && go install
cd "$PROJECT_PATH/database/" && go install
cd "$PROJECT_PATH/game/" && go install
cd "$PROJECT_PATH/game_httpserver/" && go install
cd "$PROJECT_PATH/game_server/" && go install