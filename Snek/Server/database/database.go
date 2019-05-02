package database

import (
	"fmt"
	"log"

	"github.com/globalsign/mgo/bson"

	"github.com/globalsign/mgo"
	"github.com/kawer/WebSnek/Server/config"
	level "github.com/kawer/WebSnek/Server/database/level"
	user "github.com/kawer/WebSnek/Server/database/user"
	"github.com/kawer/WebSnek/Server/utils"
)

func ConnectToDB() *mgo.Database {
	session, err := mgo.Dial(config.MONGO_ROUTE)
	if err != nil {
		log.Fatal(err)
	}
	var db = session.DB(config.DATA_BASE_NAME)

	if config.DROP_AND_CREATE {
		dataInit(db)
	}

	if err != nil {
		panic("Connection to the Database failed")
	}

	fmt.Println("Connection established")

	return db
}

func dataInit(db *mgo.Database) {
	userCollection := db.C("user")
	levelCollection := db.C("level")

	userCollection.EnsureIndex(mgo.Index{
		Key:        []string{"username"},
		Unique:     true,
		DropDups:   true,
		Background: true,
		Sparse:     true,
	})

	userCollection.EnsureIndex(mgo.Index{
		Key:        []string{"email"},
		Unique:     true,
		DropDups:   true,
		Background: true,
		Sparse:     true,
	})

	levels := generateLevels()
	for _, level := range levels {
		levelCollection.Insert(level)
	}

	userCollection.Insert(user.User{
		ID:        bson.NewObjectId(),
		Username:  "kawer",
		Email:     "kawer7@gmail.com",
		Password:  "1234",
		Level:     5,
		GamesWon:  10,
		GamesLost: 3,
		GamesTied: 1,
	})

	userCollection.Insert(user.User{
		ID:        bson.NewObjectId(),
		Username:  "messi",
		Email:     "messi@gmail.com",
		Password:  "10",
		Level:     3,
		GamesWon:  600,
		GamesLost: 0,
		GamesTied: 1,
	})

	userCollection.Insert(user.User{
		ID:        bson.NewObjectId(),
		Username:  "fslzr",
		Email:     "fslzr@gmail.com",
		Password:  "fer",
		Level:     2,
		GamesWon:  4,
		GamesLost: 12,
		GamesTied: 0,
	})

	userCollection.Insert(user.User{
		ID:        bson.NewObjectId(),
		Username:  "new",
		Email:     "new@gmail.com",
		Password:  "new",
		Level:     1,
		GamesWon:  0,
		GamesLost: 0,
		GamesTied: 0,
	})

	fmt.Println("Data Init completed")
}

type lessonData struct {
	Title   string
	Lecture string
}

func generateLevels() []level.Level {
	return []level.Level{
		level.Level{
			ID:     bson.NewObjectId(),
			Number: 1,
			Title:  "Introducción",
			Lessons: generateLessons([]lessonData{
				lessonData{
					Title:   "¿Qué es python?",
					Lecture: "lesson1",
				},
			}),
		},
		level.Level{
			ID:     bson.NewObjectId(),
			Number: 2,
			Title:  "Variables",
			Lessons: generateLessons([]lessonData{
				lessonData{
					Title:   "Variables",
					Lecture: "lesson2",
				},
				lessonData{
					Title:   "Nombre de Variables",
					Lecture: "lesson3",
				},
				lessonData{
					Title:   "Tipos de Variables",
					Lecture: "lesson4",
				},
				lessonData{
					Title:   "Tipos de Variables pt. 2",
					Lecture: "lesson5",
				},
				lessonData{
					Title:   "Arreglos",
					Lecture: "lesson6",
				},
			}),
		},
		level.Level{
			ID:     bson.NewObjectId(),
			Number: 3,
			Title:  "Control",
			Lessons: generateLessons([]lessonData{
				lessonData{
					Title:   "Comparaciones",
					Lecture: "lesson7",
				},
			}),
		},
	}
}

func generateLessons(lessonsData []lessonData) []level.Lesson {
	var lessons []level.Lesson
	for index, data := range lessonsData {
		lessons = append(lessons, level.Lesson{
			Title:     data.Title,
			Lecture:   utils.GetMarkdown(data.Lecture),
			Generated: false,
			Questions: []string{
				"Question 1",
			},
			Number: index,
		})
	}
	return lessons
}
