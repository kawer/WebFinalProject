package utils

import (
	"encoding/json"
	"go/build"
	"io/ioutil"
	"net/http"
	"os"
)

func RespondWithJSON(w http.ResponseWriter, code int, payload interface{}) {
	response, _ := json.Marshal(payload)
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(code)
	w.Write(response)
}

func RespondWithError(w http.ResponseWriter, code int, msg string) {
	RespondWithJSON(w, code, map[string]string{"error": msg})
}

func GetMarkdown(name string) string {
	goPath := os.Getenv("GOPATH")
	if goPath == "" {
		goPath = build.Default.GOPATH
	}
	filename := goPath + "/src/github.com/kawer/WebSnek/Server/assets/lessons/" + name + ".md"
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}
	return string(content)
}
