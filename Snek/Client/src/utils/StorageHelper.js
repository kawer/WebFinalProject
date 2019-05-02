class StorageHelper {
  static userId = null;
  static username = null;
  static level = null;

  static userLogin(userId, username, level) {
    localStorage.setItem("userId", userId);
    localStorage.setItem("username", username);
    localStorage.setItem("level", level);
    this.userId = userId;
    this.username = username;
    this.level = 1 * level;
  }

  static getUser() {
    if (this.userId == null || this.username == null || this.level == null) {
      this.userId = localStorage.getItem("userId");
      this.username = localStorage.getItem("username");
      this.level = 1 * localStorage.getItem("level");
    }
    return {
      userId: this.userId,
      username: this.username,
      level: this.level
    };
  }

  static userLogout() {
    localStorage.removeItem("userId");
    localStorage.removeItem("username");
    localStorage.removeItem("level");
    this.userId = null;
    this.username = null;
    this.level = null;
  }

  static userLeveledUp() {
    localStorage.removeItem("level");
    this.level = this.level + 1;
    localStorage.setItem("level", this.level);
  }
}

export default StorageHelper;
