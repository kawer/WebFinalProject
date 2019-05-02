package game

import (
	"encoding/json"
	"fmt"
	"math/rand"
	"os"
	"os/exec"
)

func GenerateMultipleQuestions(numQuestions int) ([]QuestionRes, error) {
	numTopics := 4

	var questions []QuestionRes
	var question QuestionRes

	for i := 0; i < numQuestions; i++ {

		randomTopic := rand.Intn(numTopics) + 1

		resJSON, err := GenerateQuestion(randomTopic)
		if err != nil {
			return questions, err
		}
		//fmt.Println(string(resJSON))
		json.Unmarshal(resJSON, &question)

		questions = append(questions, question)
		fmt.Println(question)
		question = QuestionRes{}
	}

	//fmt.Println(questions)

	return questions, nil
}

func GenerateQuestion(tmpTopicInt int) ([]byte, error) {

	var res QuestionRes

	topic := QuestionTopic(tmpTopicInt)

	fmt.Println(topic)

	numTypes := len(typeGen[topic])
	randomType := rand.Intn(numTypes)
	randomType = 0
	questionType := typeGen[topic][randomType]

	topicArg := qTopicArg[topic]
	typeArg := qTypeArg[questionType]

	fmt.Println(topicArg)
	fmt.Println(typeArg)

	user := os.Getenv("USER")
	fmt.Println(user)

	output, err := excecutePygen(topicArg, typeArg)

	if err != nil {
		os.Stderr.WriteString(err.Error())
		output, err = excecutePygen(topicArg, typeArg)
		if err != nil {
			os.Stderr.WriteString(err.Error())
			output, err = excecutePygen(topicArg, typeArg)
			if err != nil {
				os.Stderr.WriteString(err.Error())
				output, err = excecutePygen(topicArg, typeArg)
			}
			if err != nil {
				os.Stderr.WriteString(err.Error())
				output, err = excecutePygen(topicArg, typeArg)
			}
			if err != nil {
				os.Stderr.WriteString(err.Error())
				output, err = excecutePygen(topicArg, typeArg)
			}
		}

	}

	//fmt.Println(string(output))

	var payload Payload
	json.Unmarshal(output, &payload)
	res.Payload = payload

	title := qTitle[questionType]
	res.Type = questionType
	res.Title = title

	return json.Marshal(res)
}

func excecutePygen(topicArg string, typeArg string) ([]byte, error) {

	user := os.Getenv("USER")
	fmt.Println(user)

	output, err := exec.Command("/Users/"+user+"/.local/bin/pygen", topicArg, typeArg).CombinedOutput()
	if err != nil {
		os.Stderr.WriteString(err.Error())
	}
	return output, err
}
