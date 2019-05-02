import React, { useState, useEffect } from "react";
import Lines from "./Lines";
import {
  FormControl,
  RadioGroup,
  FormControlLabel,
  Radio
} from "@material-ui/core";

function MultipleChoice({ question, onResponse }) {
  const [response, setResponse] = useState(null);
  const [options, setOptions] = useState([]);

  useEffect(() => {
    function shuffleArray(arr) {
      return arr.sort(() => Math.random() - 0.5);
    }

    const options = shuffleArray([
      ...question.correctAnswers,
      ...question.wrongAnswers
    ]);

    setOptions(options);
  }, []);

  useEffect(() => {
    if (question && response) {
      const index = question.correctAnswers.findIndex(ans => ans === response);
      onResponse(index !== -1);
    }
  }, [response]);

  const { code } = question;
  return (
    <>
      <Lines code={code} />
      <FormControl>
        <RadioGroup
          value={response}
          onChange={event => setResponse(event.target.value)}
        >
          {options.map((option, index) => (
            <FormControlLabel
              key={index}
              value={option}
              control={<Radio />}
              label={option}
            />
          ))}
        </RadioGroup>
      </FormControl>
    </>
  );
}

export default MultipleChoice;
