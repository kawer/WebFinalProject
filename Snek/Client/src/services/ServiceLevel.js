import axios from "axios";
import { API } from "../utils/utils";

class ServiceLevel {
  static MODULE_URL = API.URL + "level";

  static async getAllLevels() {
    return await axios.get(this.MODULE_URL + "/all");
  }

  static async getLessonsByLevel(levelId) {
    return await axios.get(this.MODULE_URL + `/${levelId}`);
  }
}

export default ServiceLevel;
