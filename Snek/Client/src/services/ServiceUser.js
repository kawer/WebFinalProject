import axios from "axios";
import { API } from "../utils/utils";

class ServiceUser {
  static MODULE_URL = API.URL + "user";

  static async registerUser(username, email, password) {
    return axios.post(this.MODULE_URL, { username, email, password });
  }

  static async loginUser(username, password) {
    return axios.post(this.MODULE_URL + "/login", { username, password });
  }

  static async leveledUpUser(userId, userLevel) {
    return axios.put(this.MODULE_URL + `/level/${userId}/${userLevel}`);
  }

  static async getUser(userId) {
    return axios.get(this.MODULE_URL + `/${userId}`);
  }
}

export default ServiceUser;
