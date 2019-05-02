package level

import (
	"github.com/globalsign/mgo"
	"gopkg.in/mgo.v2/bson"
)

type LevelDao struct {
	db *mgo.Database
}

const (
	COLLECTION = "level"
)

func (d *LevelDao) getAllLevels() ([]Level, error) {
	levels := []Level{}
	err := d.db.C(COLLECTION).Find(bson.M{}).All(&levels)
	return levels, err
}

func (d *LevelDao) getLessons(id string) ([]Level, error) {
	levels := []Level{}
	err := d.db.C(COLLECTION).Find(bson.M{}).Select(bson.M{"lessons": 1}).All(&levels)
	return levels, err
}

func (d *LevelDao) getLecture(levelID string, lessonTitle string) ([]Lesson, error) {
	lessons := []Lesson{}
	err := d.db.C(COLLECTION).Find(bson.M{"_id": bson.ObjectIdHex(levelID), "lessons.title": lessonTitle}).Select(bson.M{"lessons.title": 1, "lessons.lecture": 1}).All(&lessons)
	return lessons, err
}
