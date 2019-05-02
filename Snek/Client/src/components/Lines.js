import React from "react";
import { Typography } from "@material-ui/core";
import { ENUMS } from "../utils/utils";

function Line({ line }) {
  return line.map((l, index) =>
    l.type === ENUMS.LINE_TYPE.CODE ? (
      <Typography variant="h6" key={index}>
        {l.value}
      </Typography>
    ) : (
      <Typography variant="h6" key={index}>
        ____
      </Typography>
    )
  );
}

function Lines({ code }) {
  const { lines } = code;
  return (
    <div className="Lines">
      {lines.map((l, index) => (
        <Line key={index} line={l} />
      ))}
    </div>
  );
}

export default Lines;
