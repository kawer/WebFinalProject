package game

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"
	"time"

	uuid "github.com/satori/go.uuid"
)

// InitializePlayer receives the userID and username and creates the UserInGame object
func InitializePlayer(userID uuid.UUID, username string) UserInGame {

	var player = UserInGame{
		UserID:      userID,
		Username:    username,
		LastUpdated: time.Now(),
	}
	return player
}

// AddPlayerToWaitingGameQueue adds new player to the WaitingUsers queue in case there are no available games.
func AddPlayerToWaitingGameQueue(player *UserInGame) ([]byte, error) {

	u1 := uuid.Must(uuid.NewV4())
	var waitingGame WaitingGame
	waitingGame.User = player
	waitingGame.GameID = u1
	WaitingUsers.Put(waitingGame)
	var res = MultiRes{
		GameID: u1,
		Type:   "Wait",
	}

	return json.Marshal(res)
}

func MakeGame(player1 *UserInGame) ([]byte, error) {

	tmpWaitingGame, err := WaitingUsers.Get(1)
	if err != nil {
		panic(err)
	}
	waitingGame, ok := tmpWaitingGame[0].(WaitingGame)
	if !ok {
		panic("Cannot assign object in queue to WaitingGame")
	}
	player2 := waitingGame.User

	gameID := waitingGame.GameID

	questions, err := GenerateMultipleQuestions(10)
	if err != nil {
		panic(err)
	}

	var res = MultiRes{
		GameID: gameID,
		Type:   "Question",
		Payload: VersusPayload{
			Question:  questions[0],
			UserScore: 0,
			Username:  player2.Username,
		},
	}

	var newGame = Game{
		User1:     player1,
		User2:     player2,
		Questions: questions,
	}

	OngoingGames[gameID] = newGame

	return json.Marshal(res)
}

func CreateGameQuestionReq(tmpGameID string, tmpUserID string, tmpQNumber string, tmpScore string) GameQuestionReq {

	gameID, err := uuid.FromString(tmpGameID)
	if err != nil {
		panic(err)
	}

	userID, err := uuid.FromString(tmpUserID)
	if err != nil {
		panic(err)
	}

	intQNumber, err := strconv.Atoi(tmpQNumber)
	if err != nil {
		panic(err)
	}

	qNumber := int8(intQNumber)

	intScore, err := strconv.Atoi(tmpScore)
	if err != nil {
		panic(err)
	}

	score := int8(intScore)

	var req = GameQuestionReq{
		GameID:      gameID,
		UserID:      userID,
		QuestionNum: qNumber,
		Score:       score,
	}

	return req
}

func GetNextGameQuestion(req GameQuestionReq) ([]byte, error) {

	gameID := req.GameID

	if _, ok := OngoingGames[gameID]; !ok {
		var res = MultiRes{
			GameID: gameID,
			Type:   "Wait",
		}

		return json.Marshal(res)
	}

	currentGame := OngoingGames[gameID]
	var player1, player2 *UserInGame

	if currentGame.User1.UserID == req.UserID {
		player1 = currentGame.User1
		player2 = currentGame.User2
	} else {
		player1 = currentGame.User2
		player2 = currentGame.User1
	}

	player1.LastUpdated = time.Now()
	player1.QuestionNum = req.QuestionNum
	player1.Score = req.Score

	if maxInactiveTimeElapsed(*player2, 300) {
		var res = MultiRes{
			GameID: gameID,
			Type:   "Disconnect",
		}
		return json.Marshal(res)

	} else if player2.QuestionNum != player1.QuestionNum {
		var res = MultiRes{
			GameID: gameID,
			Type:   "Wait",
		}
		return json.Marshal(res)
	} else {
		var res = createGameResponse(*player1, *player2, gameID)

		if player1.Ended == true && player2.Ended == true {
			delete(OngoingGames, gameID)
		}

		return json.Marshal(res)
	}

}

func maxInactiveTimeElapsed(player UserInGame, maxInactiveTime float64) bool {

	diff := time.Now().Sub(player.LastUpdated)
	if diff.Seconds() >= maxInactiveTime {
		return true
	}
	return false

}

func createGameResponse(player1 UserInGame, player2 UserInGame, gameID uuid.UUID) MultiRes {

	if player1.QuestionNum == 10 {
		player1.Ended = true
		return createGameResponseLastQuestion(player1, player2, gameID)
	}
	return createOtherGameResponse(player1, player2, gameID)

}

func createGameResponseLastQuestion(player1 UserInGame, player2 UserInGame, gameID uuid.UUID) MultiRes {
	// TODO: Update User collection

	if player1.Score > player2.Score {

		return getMultiRes(0, gameID, player2)

	} else if player1.Score < player2.Score {

		return getMultiRes(1, gameID, player2)

	} else {

		return getMultiRes(2, gameID, player2)
	}
}

func getMultiRes(result GameResult, gameID uuid.UUID, player UserInGame) MultiRes {
	return MultiRes{
		GameID: gameID,
		Type:   "Ended",
		Payload: VersusPayload{
			UserScore: player.Score,
			Username:  player.Username,
			Result:    result,
		},
	}
}

func createOtherGameResponse(player1 UserInGame, player2 UserInGame, gameID uuid.UUID) MultiRes {

	questionNuber := player1.QuestionNum
	fmt.Println("FGSDBXGSEFREGDFRSFRGSRSRGS", questionNuber)

	currentQuestion := OngoingGames[gameID].Questions[questionNuber]
	fmt.Println(currentQuestion)

	return MultiRes{
		GameID: gameID,
		Type:   "Question",
		Payload: VersusPayload{
			UserScore: player2.Score,
			Username:  player2.Username,
			Question:  currentQuestion,
		},
	}

}

func WriteResponse(w http.ResponseWriter, responseJSON []byte) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusAccepted)
	w.Write(responseJSON)
}
