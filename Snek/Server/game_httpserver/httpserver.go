package httpserver

import (
	"fmt"
	"net/http"
	"strconv"

	"github.com/kawer/WebSnek/Server/game"
	uuid "github.com/satori/go.uuid"
)

// GetQuestion returns a question of the level and lesson required by the user.
func GetQuestion(w http.ResponseWriter, r *http.Request) {

	tmpTopic := r.FormValue("questionTopic")
	tmpTopicInt, err := strconv.Atoi(tmpTopic)
	if err != nil {
		panic(err)
	}

	resJSON, err := game.GenerateQuestion(tmpTopicInt)

	if err != nil {
		panic(err)
	}

	fmt.Println(string(resJSON))

	game.WriteResponse(w, resJSON)

}

// JoinOrCreateGame either adds the player to a Game or puts it in the Waiting user queue.
func JoinOrCreateGame(w http.ResponseWriter, r *http.Request) {

	tmpUserID := r.FormValue("userId")
	username := r.FormValue("username")

	userID, err := uuid.FromString(tmpUserID)
	if err != nil {
		panic(err)
	}

	fmt.Println(userID, username)

	player := game.InitializePlayer(userID, username)

	fmt.Println(player)

	if game.WaitingUsers.Empty() {
		resJSON, err := game.AddPlayerToWaitingGameQueue(&player)
		if err != nil {
			panic(err)
		}
		game.WriteResponse(w, resJSON)
	} else {
		resJSON, err := game.MakeGame(&player)
		if err != nil {
			panic(err)
		}
		game.WriteResponse(w, resJSON)
	}

}

func GetGameQuestion(w http.ResponseWriter, r *http.Request) {

	tmpGameID := r.FormValue("gameId")
	tmpUserID := r.FormValue("userId")
	tmpQNumber := r.FormValue("qNumber")
	tmpScore := r.FormValue("score")

	req := game.CreateGameQuestionReq(tmpGameID, tmpUserID, tmpQNumber, tmpScore)

	resJSON, err := game.GetNextGameQuestion(req)
	if err != nil {
		panic(err)
	}
	game.WriteResponse(w, resJSON)
}
