import React from "react";
import { Typography, Button } from "@material-ui/core";

import "./GameGreet.css";

function GameGreet({ title, description, buttonLabel, onButtonClick }) {
  return (
    <div className="GameGreet">
      <Typography variant="h5" gutterBottom>
        {title}
      </Typography>
      <Typography gutterBottom>{description}</Typography>
      <Button
        variant="contained"
        color="secondary"
        onClick={() => onButtonClick()}
      >
        {buttonLabel}
      </Button>
    </div>
  );
}

export default GameGreet;
