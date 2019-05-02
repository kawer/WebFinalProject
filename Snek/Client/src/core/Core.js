import React, { useState } from "react";
import { Route, Switch, Redirect } from "react-router-dom";
import {
  AppBar,
  Toolbar,
  Typography,
  Snackbar,
  SnackbarContent,
  IconButton
} from "@material-ui/core";
import { Close as CloseIcon } from "@material-ui/icons";

import MainMenu from "./MainMenu";
import ProfileMenu from "./ProfileMenu";

import PageLevels from "../pages/PageLevels";
import PageMultiplayer from "../pages/PageMultiplayer";
import PagePractice from "../pages/PagePractice";

import "./Core.css";
import PageHome from "../pages/PageHome";
import PageLessons from "../pages/PageLessons";
import PageLecture from "../pages/PageLecture";

function Core({ match }) {
  const [title, setTitle] = useState("Inicio");
  const [error, setError] = useState(null);

  // TODO: authenticate user
  const currentPath = match.url;
  return (
    <div className="Core">
      <AppBar>
        <Toolbar className="Toolbar">
          <MainMenu
            match={match}
            onChangePage={pageTitle => setTitle(pageTitle)}
          />
          <Typography className="Title" variant="h6" color="inherit">
            {title}
          </Typography>
          <ProfileMenu />
        </Toolbar>
      </AppBar>
      <PageContainer currentPath={currentPath} errorHandler={setError} />
      <Error error={error} setError={setError} />
    </div>
  );
}

function PageContainer({ currentPath, errorHandler }) {
  const errorProps = { errorHandler };
  const pages = [
    { path: "/home", page: PageHome },
    { path: "/lessons", page: PageLevels },
    { path: "/lessons/:levelId", page: PageLessons },
    { path: "/lessons/:levelId/:lessonNumber", page: PageLecture },
    { path: "/multiplayer", page: PageMultiplayer },
    { path: "/profile", page: PagePractice }
  ];

  return (
    <div className="PageContainer">
      <Switch>
        {pages.map(p => (
          <Route
            exact
            key={p.path}
            path={currentPath + p.path}
            render={() => <p.page {...errorProps} />}
          />
        ))}
        <Route
          render={() => <Redirect to={{ pathname: currentPath + "/home" }} />}
        />
      </Switch>
    </div>
  );
}

function Error({ error, setError }) {
  return (
    <Snackbar
      anchorOrigin={{
        vertical: "bottom",
        horizontal: "right"
      }}
      open={error !== null}
      autoHideDuration={60000}
      onClose={() => setError(null)}
    >
      <SnackbarContent
        className="ErrorSnackbar"
        aria-describedby="client-snackbar"
        message={error && error.message ? error.message : "Algo salió mal"}
        action={[
          <IconButton
            key="close"
            aria-label="Close"
            color="inherit"
            onClick={() => setError(null)}
          >
            <CloseIcon />
          </IconButton>
        ]}
      />
    </Snackbar>
  );
}

export default Core;
