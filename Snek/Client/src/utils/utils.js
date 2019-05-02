const API = {
  URL: "http://192.168.0.7:8080/snek/api/"
};

const ENUMS = {
  RES_TYPE: {
    QUESTION: "Question",
    WAIT: "Wait",
    DISCONNECT: "Disconnect",
    ERROR: "Error"
  },
  QUESTION_TYPE: {
    MULTIPLE_CHOICE: 0,
    FILL_IN: 1,
    SELECTION_CHOICE: 2,
    WORDBANK: 3
  },
  LINE_TYPE: {
    CODE: "code",
    BLANK: "blank"
  }
};

export { API, ENUMS };
