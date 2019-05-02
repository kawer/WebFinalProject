import React, { useState, useEffect } from "react";
import { withRouter } from "react-router-dom";
import { Card, CardContent, Typography, Button } from "@material-ui/core";
import { LocalLibrary } from "@material-ui/icons";

import ServiceLevel from "../services/ServiceLevel";
import "./PageLevels.css";
import StorageHelper from "../utils/StorageHelper";

function PageLevels({ errorHandler, history, match }) {
  const [levels, setLevels] = useState([]);
  const [user, setUser] = useState(null);

  useEffect(() => {
    async function getLevels() {
      try {
        const { data } = await ServiceLevel.getAllLevels();
        setLevels(data);
      } catch (error) {
        errorHandler(error);
      }
    }
    getLevels();
    setUser(StorageHelper.getUser());
  }, []);

  function onSelectedLevel(id) {
    history.push(match.url + "/" + id);
  }

  return (
    <div className="PageLevels">
      {levels.map((level, index) => (
        <LevelCard
          key={level.id}
          level={level}
          userLevel={user === null ? 0 : user.level}
          index={index}
          onSelectedLevel={onSelectedLevel}
        />
      ))}
      <LocalLibrary className="BackgroundImage" />
    </div>
  );
}

function LevelCard({ level, userLevel, index, onSelectedLevel }) {
  const unlocked = userLevel >= index + 1;
  return (
    <Card className="LevelCard">
      <CardContent>
        <Typography color="textSecondary" gutterBottom>
          Nivel {index + 1}
        </Typography>
        <Typography variant="h5" component="h2" gutterBottom>
          {level.title}
        </Typography>
        <Button
          variant="contained"
          size="small"
          color="secondary"
          disabled={!unlocked}
          onClick={() => onSelectedLevel(level.id)}
        >
          {unlocked ? "Aprender" : "Bloqueado"}
        </Button>
      </CardContent>
    </Card>
  );
}

export default withRouter(PageLevels);
