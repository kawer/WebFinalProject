import React, { useState, useEffect } from "react";
import { withRouter } from "react-router-dom";
import { LocalLibrary } from "@material-ui/icons";
import { Card, CardContent, Typography, Button } from "@material-ui/core";
import ServiceLevel from "../services/ServiceLevel";
import ServiceUser from "../services/ServiceUser";
import StorageHelper from "../utils/StorageHelper";
import Question from "../components/Question";
import ServiceGame from "../services/ServiceGame";

function PageLessons({ errorHandler, match, history }) {
  const levelId = match.params.levelId;
  const [level, setLevel] = useState(null);
  const [lessons, setLessons] = useState([]);
  const [user, setUser] = useState(null);

  const [playing, setPlaying] = useState(false);
  const [question, setQuestion] = useState(null);

  useEffect(() => {
    async function getLessons() {
      try {
        const { data } = await ServiceLevel.getAllLevels();
        const level = data.find(level => level.id === levelId);
        setLevel(level);
        setLessons(level.lessons);
      } catch (error) {
        errorHandler(error);
      }
    }
    getLessons();
    setUser(StorageHelper.getUser());
  }, []);

  function onSelectedLesson(lessonNumber) {
    history.push(match.url + `/${lessonNumber}`);
  }

  async function onFinishedLevel() {
    try {
      await ServiceUser.leveledUpUser(user.userId, user.level + 1);
      StorageHelper.userLeveledUp();
    } catch (error) {
      errorHandler(error);
    }
  }

  async function startPlaying() {
    const oneOrTwo = () => (Math.random() > 0.5 ? 1 : 2);
    const threeOrFour = () => (Math.random() > 0.5 ? 3 : 4);
    try {
      const { data } = await ServiceGame.practice(
        level.number - 1 == 1 ? oneOrTwo() : threeOrFour()
      );
      setPlaying(true);
      setQuestion(data);
    } catch (error) {
      console.error(error);
    }
  }
  async function handleAnswer(isCorrect) {
    if (!isCorrect) errorHandler({ message: "Respuesta incorrecta" });
    else {
      setQuestion(null);
      await startPlaying();
    }
  }

  return (
    <div className="PageLessons">
      {lessons.map(lesson => (
        <LessonCard
          key={lesson.title}
          lesson={lesson}
          onSelectedLesson={onSelectedLesson}
        />
      ))}
      <br />
      <br />
      <br />
      <br />
      <Button
        variant="contained"
        color="secondary"
        onClick={() => onFinishedLevel()}
      >
        Terminé Nivel
      </Button>
      <br />
      <br />
      <br />
      <br />
      {level && level.number != 1 ? (
        <Button
          variant="contained"
          color="secondary"
          onClick={() => startPlaying()}
        >
          Practicar
        </Button>
      ) : (
        <></>
      )}
      <br />
      <br />
      <br />
      <br />
      {playing && question ? (
        <Question
          errorHandler={errorHandler}
          question={question}
          onAnswer={handleAnswer}
          block={true}
        />
      ) : (
        <></>
      )}
    </div>
  );
}

function LessonCard({ lesson, onSelectedLesson }) {
  return (
    <Card className="LessonCard">
      <CardContent>
        <Typography variant="h5" component="h2" gutterBottom>
          {lesson.title}
        </Typography>
        <Button
          variant="contained"
          size="small"
          color="secondary"
          onClick={() => onSelectedLesson(lesson.number)}
        >
          Leer
        </Button>
      </CardContent>
    </Card>
  );
}

export default withRouter(PageLessons);
