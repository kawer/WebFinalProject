import React from "react";
import "./Link.css";

import { Link as NativeLink } from "react-router-dom";

function Link(props) {
  return (
    <NativeLink {...props} className="Link">
      {props.children}
    </NativeLink>
  );
}

export default Link;
