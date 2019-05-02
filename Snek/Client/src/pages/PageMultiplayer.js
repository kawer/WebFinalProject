import React, { useState, useEffect } from "react";
import _ from "lodash";
import { Timer } from "@material-ui/icons";
import { Typography, Button, CircularProgress } from "@material-ui/core";

import GameGreet from "../components/GameGreet";
import { ENUMS } from "../utils/utils";
import ServiceGame from "../services/ServiceGame";
import "./PageMultiplayer.css";
import Question from "../components/Question";

function PageMultiplayer({ errorHandler }) {
  const [username, setUsername] = useState("fslzrr");
  const [userId, setUserId] = useState("859dcd82-7481-4f09-8a2f-6759bab03536");
  const [gameId, setGameId] = useState(null);
  const [canceledGame, setCanceledGame] = useState(false);

  const [questionNumber, setQuestionNumber] = useState(null);
  const [question, setQuestion] = useState(null);
  const [score, setScore] = useState(0);

  const [oponentScore, setOponentScore] = useState(0);
  const [oponent, setOponent] = useState(null);

  async function startGame() {
    try {
      const { data } = await ServiceGame.startGame(userId, username);
      setGameId(data.gameId);
      setCanceledGame(false);
      setQuestionNumber(0);
    } catch (error) {
      errorHandler(error);
    }
  }

  async function cancelGame() {
    try {
      // TODO: request to cancel game
      // TODO: send request to cancel game
      setGameId(null);
      setQuestionNumber(null);
      setQuestion(null);
      setScore(0);
      setCanceledGame(true);
    } catch (error) {
      errorHandler(error);
    }
  }

  useEffect(() => {
    async function getNextQuestion() {
      const implication = p => p && !canceledGame;

      const waitFallback = () => {
        if (gameId !== null)
          setTimeout(async () => {
            await getNextQuestion();
          }, 1000);
      };

      const questionFallback = ({ payload }) => {
        const { question, result, userScore, username } = payload;
        setQuestion(question);
        setOponentScore(userScore);
        setOponent(username);
        // TODO: game ended winner/looser
      };

      const disconnectFallback = () => {};

      try {
        const { data } = await ServiceGame.requestQuestion(
          userId,
          gameId,
          questionNumber,
          score
        );

        const { type } = data;
        if (implication(type === ENUMS.RES_TYPE.WAIT)) waitFallback();
        else if (implication(type === ENUMS.RES_TYPE.QUESTION))
          questionFallback(data);
        else if (implication(type === ENUMS.RES_TYPE.DISCONNECT))
          disconnectFallback();
        else if (implication(type === ENUMS.RES_TYPE.ERROR)) throw {};
      } catch (error) {
        errorHandler(error);
      }
    }

    if (questionNumber !== null) getNextQuestion();
  }, [questionNumber]);

  function handleAnswer(isCorrect, roundScore) {
    setScore(isCorrect ? score + 1 : score);
    setQuestionNumber(questionNumber + 1);
    setQuestion(null);
  }

  return (
    <div className="PageMultiplayer">
      {gameId === null ? (
        <GameGreet
          title="Bienvenido al modo Desafío"
          description="Aquí jugaras contra otro usuario en vivo y el que obtenga la mayor
          cantidad de puntos al terminar, será el ganador."
          buttonLabel="Empezar"
          onButtonClick={startGame}
        />
      ) : (
        <></>
      )}
      {gameId !== null && question === null ? (
        <WaitingPlayer cancelGame={cancelGame} />
      ) : (
        <></>
      )}
      {gameId !== null && question !== null ? (
        <Question
          errorHandler={errorHandler}
          question={question}
          onAnswer={handleAnswer}
        />
      ) : (
        <></>
      )}
      <Timer className="BackgroundImage" />
    </div>
  );
}

function WaitingPlayer({ cancelGame }) {
  return (
    <div className="WaitingPlayer">
      <CircularProgress color="secondary" />
      <Typography gutterBottom>Esperando oponente</Typography>
      <Button variant="contained" size="small" onClick={() => cancelGame()}>
        Cancelar
      </Button>
    </div>
  );
}

export default PageMultiplayer;
