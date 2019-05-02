import React, { useEffect, useState } from "react";
import "./PagePractice.css";
import StorageHelper from "../utils/StorageHelper";
import ServiceUser from "../services/ServiceUser";

function PagePractice({ errorHandler }) {
  const [userId, setUserId] = useState("");
  const [user, setUser] = useState(null);

  useEffect(() => {
    function getUser() {
      const user = StorageHelper.getUser();
      console.log(user);
      setUserId(user.userId);
    }

    getUser();
  }, []);

  useEffect(() => {
    async function requestUser() {
      try {
        const { data } = await ServiceUser.getUser(userId);
        setUser(data);
      } catch (error) {
        errorHandler(error);
      }
    }

    if (userId !== "") requestUser();
  }, [userId]);

  return (
    <div>
      <h4>Perfil</h4>
      <p>Usuario: {user === null ? "" : user.username}</p>
      <p>Correo Electrónico: {user === null ? "" : user.email}</p>
      <p>Nivel: {user === null ? "" : user.level}</p>
      <h5>Partidas</h5>
      <p>Ganadas: {user === null ? "" : user.games_won}</p>
      <p>Empatadas: {user === null ? "" : user.games_tied}</p>
      <p>Perdidas: {user === null ? "" : user.games_lost}</p>
    </div>
  );
}

export default PagePractice;
