package user

import (
	"net/http"

	"github.com/kawer/WebSnek/Server/utils"

	"github.com/globalsign/mgo/bson"

	"github.com/globalsign/mgo"
)

type UserDAO struct {
	db *mgo.Database
}

const (
	COLLECTION = "user"
)

func (u *UserDAO) CreateUser(user User) error {
	user.Level = 1
	err := u.db.C(COLLECTION).Insert(&user)

	return err
}

func (u *UserDAO) Login(username string, password string) (map[string]interface{}, *utils.RequestError) {
	user := make(map[string]interface{})

	err := u.db.C(COLLECTION).Find(bson.M{"username": username}).Select(bson.M{"_id": 1, "username": 1, "password": 1, "level": 1}).One(&user)

	if _, ok := user["username"]; !ok {
		return user, &utils.RequestError{Code: http.StatusNotFound, Message: "Usuario no encontrado"}
	}

	if user["password"] != password {
		return user, &utils.RequestError{Code: http.StatusBadRequest, Message: "El usuario o la contraseña son incorrectos"}
	}

	if err != nil {
		return user, &utils.RequestError{Code: http.StatusInternalServerError, Message: "Algo salió mal"}
	}

	user["password"] = ""

	return user, nil
}

func (u *UserDAO) GetUserByID(id string) (User, error) {
	user := User{}
	err := u.db.C(COLLECTION).FindId(bson.ObjectIdHex(id)).One(&user)

	return user, err
}

func (u *UserDAO) GetUserLevel(id string) (uint8, error) {
	level := make(map[string]uint8)
	err := u.db.C(COLLECTION).FindId(bson.ObjectIdHex(id)).Select(bson.M{"level": 1}).One(&level)

	if err != nil {
		return 0, err
	}

	return level["level"], err
}

func (u *UserDAO) UpdateUserLevel(id string, level uint8) (uint8, error) {
	err := u.db.C(COLLECTION).UpdateId(bson.ObjectIdHex(id), bson.M{"$set": bson.M{"level": level}})

	return level, err
}

func (u *UserDAO) UpdateUser(id string, user User) (User, error) {
	err := u.db.C(COLLECTION).UpdateId(bson.ObjectIdHex(id), &user)
	return user, err
}

func (u *UserDAO) UpdateGamesQuantity(id string, t string) (map[string]int, error) {
	gamesResults := make(map[string]int)
	gameResultType := "games_won"

	if t == "1" {
		gameResultType = "games_lost"
	} else if t == "2" {
		gameResultType = "games_tied"
	}

	err := u.db.C(COLLECTION).UpdateId(bson.ObjectIdHex(id), bson.M{"$inc": bson.M{gameResultType: 1}})

	if err != nil {
		return nil, err
	}

	err = u.db.C(COLLECTION).FindId(bson.ObjectIdHex(id)).Select(bson.M{gameResultType: 1}).One(&gamesResults)

	return gamesResults, err
}
