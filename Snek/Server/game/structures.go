package game

import (
	_ "runtime"
	"time"

	uuid "github.com/satori/go.uuid"

	"github.com/golang-collections/go-datastructures/queue"
)

// Structures for question generation and request processing

// QuestionTopic is a type that contains the question topics
type QuestionTopic int

// Defining QuestionTopic
const (
	Identifiers QuestionTopic = 0 + iota
	SimpleArithmetic
	CombinedArithmetic
	Boolean
	Comparisons
	Conditionals
)

// QuestionType is a type that contains the question types
type QuestionType int

// Defining QuestionType
const (
	MultipleChoice QuestionType = 0 + iota
	FillInTheBlank
	SelectionChoice
)

// QuestionDifficulty is a type that contains the question difficulty
type QuestionDifficulty int

// Defining QuestionDifficulty
const (
	Easy QuestionDifficulty = 0 + iota
	Hard
)

type GameResult int

const (
	Win GameResult = 0 + iota
	Lose
	Tie
)

// Req is the request structure
type ReqSingle struct {
	QuestionTopic QuestionTopic `json:"questionTopic"`
}

type Line struct {
	Type  string `json:"type"`
	Value string `json:"value"`
}

type Code struct {
	Lines [][]Line `json:"lines"`
}

type Payload struct {
	Code           Code     `json:"code,omitempty"`
	WrongAnswers   []string `json:"wrongAnswers,omitempty"`
	CorrectAnswers []string `json:"correctAnswers,omitempty"`
}

type QuestionRes struct {
	Type    QuestionType `json:"type"`
	Title   string       `json:"title"`
	Payload Payload      `json:"payload"`
}

type VersusPayload struct {
	Question  QuestionRes `json:"question"`
	Username  string      `json:"username"`
	UserScore int8        `json:"userScore"`
	Result    GameResult  `json:"result"`
}

type MultiRes struct {
	GameID  uuid.UUID     `json:"gameId"`
	Type    string        `json:"type"`
	Payload VersusPayload `json:"payload"`
}

type MultipleQuestions struct {
	Ans []QuestionRes `json:"answers"`
}

type GameQuestionReq struct {
	GameID      uuid.UUID
	UserID      uuid.UUID
	QuestionNum int8
	Score       int8
}

type UserInGame struct {
	Username    string
	UserID      uuid.UUID
	QuestionNum int8
	LastUpdated time.Time
	Score       int8
	Ended       bool
}

type Game struct {
	User1     *UserInGame
	User2     *UserInGame
	Questions []QuestionRes
}

// WaitingGame contains UserInGame and ID of game
type WaitingGame struct {
	User   *UserInGame
	GameID uuid.UUID
}

// WaitingUsers contains WaitingGames of users waiting for a game.
var WaitingUsers = queue.New(100000)

var OngoingGames = make(map[uuid.UUID]Game)

var typeGen = map[QuestionTopic][]QuestionType{
	Identifiers:        []QuestionType{MultipleChoice, SelectionChoice, FillInTheBlank},
	SimpleArithmetic:   []QuestionType{MultipleChoice, SelectionChoice, FillInTheBlank},
	CombinedArithmetic: []QuestionType{MultipleChoice, SelectionChoice, FillInTheBlank},
	Boolean:            []QuestionType{MultipleChoice, SelectionChoice, FillInTheBlank},
	Comparisons:        []QuestionType{MultipleChoice, SelectionChoice},
	Conditionals:       []QuestionType{MultipleChoice},
}

var qTopicArg = map[QuestionTopic]string{
	Identifiers:        "identifiers",
	SimpleArithmetic:   "simple-arithmetic",
	CombinedArithmetic: "combined-arithmetic",
	Boolean:            "boolean",
	Conditionals:       "conditionals",
}

var qTypeArg = map[QuestionType]string{
	MultipleChoice:  "multiple-choice",
	FillInTheBlank:  "fill-in-the-blank",
	SelectionChoice: "selection-choice",
}

var qTitle = map[QuestionType]string{
	MultipleChoice:  "Elige la opción correcta",
	FillInTheBlank:  "Rellena el espacio en blanco para que el resultado sea correcto",
	SelectionChoice: "Selecciona las opciones correctas",
}
