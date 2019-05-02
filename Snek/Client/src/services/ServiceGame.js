import axios from "axios";
import { API } from "../utils/utils";

class ServiceGame {
  static MODULE_URL = API.URL + "game";

  static async startGame(userId, username) {
    console.log(this.MODULE_URL);
    return await axios.post(this.MODULE_URL, null, {
      params: { userId, username }
    });
  }

  static async requestQuestion(userId, gameId, questionNumber, currentScore) {
    return await axios.post(this.MODULE_URL + "/question", null, {
      params: { userId, gameId, qNumber: questionNumber, score: currentScore }
    });
  }

  static async practice(level) {
    return await axios.get(API.URL + "/question", {
      params: { questionTopic: level }
    });
  }
}

export default ServiceGame;
