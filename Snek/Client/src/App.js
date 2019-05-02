import React, { Component } from "react";
import "./App.css";

import { BrowserRouter, Route } from "react-router-dom";

import PageLanding from "./pages/PageLanding";
import Core from "./core/Core";

class App extends Component {
  render() {
    return (
      <BrowserRouter>
        <div className="App">
          <Route exact path="/" component={PageLanding} />
          <Route path="/app" component={Core} />
        </div>
      </BrowserRouter>
    );
  }
}

export default App;
