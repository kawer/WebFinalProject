import React, { useState, useEffect } from "react";
import { withRouter } from "react-router-dom";
import ReactMarkdown from "react-markdown";
import ServiceLevel from "../services/ServiceLevel";
import { Typography } from "@material-ui/core";

function PageLecture({ errorHandler, match, history }) {
  const levelId = match.params.levelId;
  const lessonNumber = match.params.lessonNumber;
  const [lesson, setLesson] = useState(null);

  useEffect(() => {
    async function getLessons() {
      try {
        const { data } = await ServiceLevel.getAllLevels();
        const level = data.find(level => level.id === levelId);
        const lesson = level.lessons.find(lesson => {
          return lesson.number == lessonNumber;
        });
        setLesson(lesson);
      } catch (error) {
        errorHandler(error);
      }
    }
    getLessons();
  }, []);

  return (
    <div className="PageLecture">
      <ReactMarkdown source={lesson ? lesson.lecture : "No lecture"} />
    </div>
  );
}

export default withRouter(PageLecture);
