import React, { useState, useEffect } from "react";
import {
  Card,
  Typography,
  CardContent,
  CardActions,
  Button,
  LinearProgress
} from "@material-ui/core";

import MultipleChoice from "./MultipleChoice";
import { ENUMS } from "../utils/utils";

import "./Question.css";

function Question({ question, onAnswer, block }) {
  // const [seconds, setSeconds] = useState(0);
  // const [timeoutId, setTimeoutId] = useState(null);
  const [response, setResponse] = useState(false);

  // useEffect(() => {
  //   if (seconds === 15) onAnswer(response, 15 - seconds);
  //   else {
  //     setTimeoutId(setTimeout(() => setSeconds(seconds + 1), 1000));
  //     return clearInterval(timeoutId);
  //   }
  // }, [seconds]);

  // TODO: Add missing question types
  function getQuestionComponent(type) {
    if (type === ENUMS.QUESTION_TYPE.MULTIPLE_CHOICE) return MultipleChoice;
  }

  const QuestionType = getQuestionComponent(question.type);
  return (
    <Card className={block ? "" : "Question"}>
      {/* <LinearProgress
        variant="determinate"
        color="secondary"
        value={seconds * 6.666}
      /> */}
      <CardContent>
        <Typography>{question.title}</Typography>
        <QuestionType
          question={question.payload}
          onResponse={response => setResponse(response)}
        />
      </CardContent>
      <CardActions>
        <Button
          variant="contained"
          size="small"
          onClick={() => onAnswer(response, 15 - 1)}
        >
          Siguiente
        </Button>
      </CardActions>
    </Card>
  );
}

export default Question;
