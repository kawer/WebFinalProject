package main

import (
	"fmt"
	"net/http"

	"github.com/kawer/WebSnek/Server/database"

	level "github.com/kawer/WebSnek/Server/database/level"

	"github.com/globalsign/mgo"
	user "github.com/kawer/WebSnek/Server/database/user"

	"github.com/gorilla/mux"
	"github.com/rs/cors"

	httpserver "github.com/kawer/WebSnek/Server/game_httpserver"
)

func main() {

	m := mux.NewRouter()
	var db = database.ConnectToDB()
	initDAOS(db)
	initRoutes(m)

	m.HandleFunc("/snek/api/question", httpserver.GetQuestion).Methods("GET")
	m.HandleFunc("/snek/api/game", httpserver.JoinOrCreateGame).Methods("POST")
	m.HandleFunc("/snek/api/game/question", httpserver.GetGameQuestion).Methods("POST")

	http.Handle("/", m)
	handler := cors.Default().Handler(m)
	fmt.Println("Server listening in port 8080")

	http.ListenAndServe(":8080", handler)
}

func initDAOS(db *mgo.Database) {
	user.Init(db)
	level.Init(db)
}

func initRoutes(m *mux.Router) {
	user.InitRoutes(m)
	level.InitRoutes(m)
}
