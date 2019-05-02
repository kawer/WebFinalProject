import React, { useState } from "react";
import {
  Drawer,
  IconButton,
  List,
  ListItem,
  ListItemIcon,
  ListItemText
} from "@material-ui/core";
import { Menu, LocalLibrary, Timer, Edit, Home } from "@material-ui/icons";
import Link from "../components/Link";

function MainMenu({ match, onChangePage }) {
  const [isOpen, setIsOpen] = useState(false);

  return (
    <>
      <Drawer
        className="MainMenu"
        open={isOpen}
        onClose={() => setIsOpen(false)}
      >
        <MainMenuPages {...{ match, onChangePage, setIsOpen }} />
      </Drawer>
      <IconButton
        className="MenuIcon"
        color="inherit"
        aria-label="Menu"
        onClick={() => setIsOpen(true)}
      >
        <Menu />
      </IconButton>
    </>
  );
}

function MainMenuPages({ match, onChangePage, setIsOpen }) {
  const pages = [
    { name: "Inicio", path: "/home", icon: <Home /> },
    { name: "Lecciones", path: "/lessons", icon: <LocalLibrary /> },
    { name: "Desafío", path: "/multiplayer", icon: <Timer /> },
    { name: "Perfil", path: "/profile", icon: <Edit /> }
  ];

  return (
    <List>
      {pages.map((page, index) => (
        <Link
          to={`${match.url + page.path}`}
          onClick={() => {
            onChangePage(page.name);
            setIsOpen(false);
          }}
          key={index}
        >
          <ListItem button>
            <ListItemIcon>{page.icon}</ListItemIcon>
            <ListItemText primary={page.name} />
          </ListItem>
        </Link>
      ))}
    </List>
  );
}

export default MainMenu;
