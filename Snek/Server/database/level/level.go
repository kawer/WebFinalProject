package level

import "github.com/globalsign/mgo/bson"

type Level struct {
	ID      bson.ObjectId `bson:"_id" json:"id"`
	Title   string        `bson:"title" json:"title"`
	Lessons []Lesson      `bson:"lessons" json:"lessons"`
	Number  int           `bson:"number" json:"number"`
}

type Lesson struct {
	Title     string   `bson:"title" json:"title"`
	Lecture   string   `bson:"lecture" json:"lecture"`
	Generated bool     `bson:"generated" json:"generated"`
	Questions []string `bson:"questions" json:"questions"`
	Number    int      `bson:"number" json:"number"`
}
