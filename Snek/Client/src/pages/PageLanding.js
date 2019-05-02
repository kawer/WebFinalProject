import React, { useState } from "react";
import { withRouter } from "react-router-dom";
import {
  AppBar,
  Toolbar,
  Typography,
  Button,
  IconButton,
  Dialog,
  DialogTitle,
  Grid,
  TextField
} from "@material-ui/core";
import { Menu as MenuIcon } from "@material-ui/icons";

import ServiceUser from "../services/ServiceUser";
import "./PageLanding.css";
import SnekLogo from "../assets/SnekLogo.jpeg";
import StorageHelper from "../utils/StorageHelper";

function LandingText() {
  return (
    <div>
      <img src={SnekLogo} alt="SnekLogo" />
      <h1>
        Snek es una aplicación con el propósito de ayudar a personas a aprender
        python de una manera interactiva!
      </h1>
      <br />
      <h3>
        Snek le permitirá a cualquier persona que hable español, mejorar sus
        habilidades de programación en Python. Python es un lenguaje de
        programación que es muy utilizado en una gama muy amplia de áreas de
        trabajo.
      </h3>
      <h3>
        Su flexibilidad lo hace perfecto para ser aplicado en casos y sistemas
        muy diferentes, desde servidores complejos, hasta simples programas para
        contadores. Además, es un lenguaje relativamente simple sintácticamente
        hablando, lo que lo hace perfecto para personas que apenas están
        comenzando a aprender a programar.
      </h3>
      <h3>
        Por lo anterior, Python es una herramienta que puede ayudarle a
        cualquier persona, sin importar su área de trabajo, a ampliar su
        productividad, nivel de trabajo e impacto en su entorno.
      </h3>
    </div>
  );
}

function PageLanding({ history }) {
  const [isLoginOpened, setIsLoginOpened] = useState(false);
  const [isRegisterOpened, setIsRegisterOpened] = useState(false);

  return (
    <div className="PageLanding">
      <LoginDialog
        open={isLoginOpened}
        onClose={() => setIsLoginOpened(false)}
        history={history}
      />
      <RegisterDialog
        open={isRegisterOpened}
        onClose={() => setIsRegisterOpened(false)}
        onRegister={() => {
          setIsRegisterOpened(false);
          setIsLoginOpened(true);
        }}
      />
      <AppBar>
        <Toolbar className="Toolbar">
          <IconButton className="MenuIcon" color="inherit" aria-label="Menu">
            <MenuIcon />
          </IconButton>
          <Typography className="Title" variant="h6" color="inherit">
            Snek - Aprende a programar en Python!
          </Typography>
          <Button color="inherit" onClick={() => setIsRegisterOpened(true)}>
            Registrarme
          </Button>
          <Button color="inherit" onClick={() => setIsLoginOpened(true)}>
            Iniciar Sesión
          </Button>
        </Toolbar>
      </AppBar>
      <br />
      <br />
      <br />
      <br />
      <br />
      <LandingText />
    </div>
  );
}

function LoginDialog({ history, ...props }) {
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");

  async function login() {
    try {
      const { data } = await ServiceUser.loginUser(username, password);
      StorageHelper.userLogin(data._id, data.username, data.level);
      history.push("/app/home");
    } catch (error) {
      console.error(error);
    }
  }

  return (
    <Dialog {...props}>
      <DialogTitle variant="h6">Credenciales</DialogTitle>
      <form className="LoginDialogForm">
        <Grid container direction="column">
          <TextField
            id="username"
            label="Usuario"
            value={username}
            onChange={event => setUsername(event.target.value)}
          />
          <TextField
            id="password"
            type="password"
            label="Password"
            value={password}
            onChange={event => setPassword(event.target.value)}
            margin="normal"
          />
          <Button variant="contained" color="secondary" onClick={() => login()}>
            Log In
          </Button>
        </Grid>
      </form>
    </Dialog>
  );
}

function RegisterDialog({ onRegister, ...props }) {
  const [username, setUsername] = useState("");
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");

  async function register() {
    try {
      await ServiceUser.registerUser(username, email, password);
      onRegister();
    } catch (error) {
      console.error(error);
    }
  }

  return (
    <Dialog {...props}>
      <DialogTitle variant="h6">Inserta tus Datos</DialogTitle>
      <form className="LoginDialogForm">
        <Grid container direction="column">
          <TextField
            id="username"
            label="Usuario"
            value={username}
            onChange={event => setUsername(event.target.value)}
          />
          <TextField
            id="email"
            type="email"
            label="Correo Electrónico"
            value={email}
            onChange={event => setEmail(event.target.value)}
            margin="normal"
          />
          <TextField
            id="password"
            type="password"
            label="Contraseña"
            value={password}
            onChange={event => setPassword(event.target.value)}
            margin="normal"
          />
          <Button
            variant="contained"
            color="secondary"
            onClick={() => register()}
          >
            Registrar
          </Button>
        </Grid>
      </form>
    </Dialog>
  );
}

export default withRouter(PageLanding);
