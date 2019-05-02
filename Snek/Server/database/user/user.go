package user

import (
	"github.com/globalsign/mgo/bson"
)

type User struct {
	ID        bson.ObjectId `bson:"_id" json:"id"`
	Username  string        `bson:"username" json:"username"`
	Email     string        `bson:"email" json:"email"`
	Password  string        `bson:"password" json:"password"`
	Level     uint8         `bson:"level" json:"level"`
	GamesWon  uint16        `bson:"games_won" json:"games_won"`
	GamesLost uint16        `bson:"games_lost" json:"games_lost"`
	GamesTied uint16        `bson:"games_tied" json:"games_tied"`
}
