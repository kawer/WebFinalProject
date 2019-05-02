package level

import (
	"log"
	"net/http"

	"github.com/gorilla/mux"

	"github.com/globalsign/mgo"

	"github.com/kawer/WebSnek/Server/utils"
)

var levelDao LevelDao

func InitRoutes(m *mux.Router) {
	m.HandleFunc("/snek/api/level/all", GetAllLevelsHandler).Methods("GET")
	m.HandleFunc("/snek/api/level/{id}", GetLessonsByLevel).Methods("GET")
	m.HandleFunc("/snek/api/level/{id}/{lessonTitle}", GetLectureByTitle).Methods("GET")
}

// GetAllLevelsHandler handles the request for getting all levels in the database
func GetAllLevelsHandler(w http.ResponseWriter, r *http.Request) {
	levels, err := levelDao.getAllLevels()

	if err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	utils.RespondWithJSON(w, http.StatusAccepted, levels)
}

func GetLectureByTitle(w http.ResponseWriter, r *http.Request) {

	vars := mux.Vars(r)
	id := vars["id"]
	lessonTitle := vars["lessonTitle"]

	lectures, err := levelDao.getLecture(id, lessonTitle)

	if err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	utils.RespondWithJSON(w, http.StatusAccepted, lectures)
}

func GetLessonsByLevel(w http.ResponseWriter, r *http.Request) {

	vars := mux.Vars(r)
	id := vars["id"]

	lessons, err := levelDao.getLessons(id)

	if err != nil {
		log.Println(err.Error())
		utils.RespondWithError(w, http.StatusInternalServerError, "Algo salió mal")
		return
	}

	utils.RespondWithJSON(w, http.StatusAccepted, lessons)
}

// Init initializes the Level Dao for accessing the database
func Init(db *mgo.Database) {
	levelDao = LevelDao{db: db}
}
