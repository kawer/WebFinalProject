package user

import (
	"encoding/json"
	"log"
	"net/http"
	"strconv"

	"github.com/kawer/WebSnek/Server/config"

	"github.com/gorilla/mux"

	"github.com/globalsign/mgo"
	"github.com/globalsign/mgo/bson"
	utils "github.com/kawer/WebSnek/Server/utils"
)

var userDao UserDAO

func InitRoutes(m *mux.Router) {
	m.HandleFunc(config.API_ROUTE+"/user", createUserHandler).Methods("POST")
	m.HandleFunc(config.API_ROUTE+"/user/{id}", getUserByIDHandler).Methods("GET")
	m.HandleFunc(config.API_ROUTE+"/user/{id}", updateUserHandler).Methods("PUT")
	m.HandleFunc(config.API_ROUTE+"/user/level/{id}", getUserLevelHandler).Methods("GET")
	m.HandleFunc(config.API_ROUTE+"/user/level/{id}/{level}", updateUserLevelHandler).Methods("PUT")
	m.HandleFunc(config.API_ROUTE+"/user/login", loginHandler).Methods("POST")
	m.HandleFunc(config.API_ROUTE+"/user/{id}/game/{result}", gameResultUpdateHandler).Methods("POST")
}

func createUserHandler(w http.ResponseWriter, r *http.Request) {
	newUser := User{}

	if err := json.NewDecoder(r.Body).Decode(&newUser); err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	newUser.ID = bson.NewObjectId()

	if err := userDao.CreateUser(newUser); err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	utils.RespondWithJSON(w, http.StatusAccepted, newUser)
}

func loginHandler(w http.ResponseWriter, r *http.Request) {
	userData := make(map[string]string)

	if err := json.NewDecoder(r.Body).Decode(&userData); err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	username, uOk := userData["username"]
	password, pOk := userData["password"]

	if !uOk || !pOk {
		utils.RespondWithError(w, http.StatusBadRequest, "Falta información")
	}

	user, err := userDao.Login(username, password)

	if err != nil {
		utils.RespondWithError(w, err.Code, err.Message)
		return
	}

	utils.RespondWithJSON(w, http.StatusAccepted, user)
}

func getUserLevelHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	userID := vars["id"]

	level, err := userDao.GetUserLevel(userID)

	if err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	utils.RespondWithJSON(w, http.StatusAccepted, map[string]uint8{"level": level})
}

func updateUserLevelHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	userID := vars["id"]
	newLevel := vars["level"]

	newLevelInt, err := strconv.Atoi(newLevel)

	if err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	level, err := userDao.UpdateUserLevel(userID, uint8(newLevelInt))

	utils.RespondWithJSON(w, http.StatusAccepted, map[string]uint8{"level": level})
}

func getUserByIDHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	userID := vars["id"]

	user, err := userDao.GetUserByID(userID)

	if err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	utils.RespondWithJSON(w, http.StatusInternalServerError, user)
}

func updateUserHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	userID := vars["id"]
	newUser := User{}

	if err := json.NewDecoder(r.Body).Decode(&newUser); err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	newUser, err := userDao.UpdateUser(userID, newUser)

	if err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	utils.RespondWithJSON(w, http.StatusInternalServerError, newUser)
}

func gameResultUpdateHandler(w http.ResponseWriter, r *http.Request) {
	vars := mux.Vars(r)
	userID := vars["id"]
	resultType := vars["result"]

	gameQuantity, err := userDao.UpdateGamesQuantity(userID, resultType)

	if err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	utils.RespondWithJSON(w, http.StatusInternalServerError, gameQuantity)
}

// Init initializes the DAO
func Init(db *mgo.Database) {
	userDao = UserDAO{db: db}
}
