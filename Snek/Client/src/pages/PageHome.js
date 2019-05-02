import React from "react";
import SnekLogo from "../assets/SnekLogo.jpeg";

function PageHome() {
  return (
    <div>
      <br />
      <br />
      <img src={SnekLogo} alt="SnekLogo" />
      <h1>
        Escoge entre modo lección para aprender o modo desafío para retar a
        otras personas!
      </h1>
      <br />
      <br />
      <br />
      <h2>
        Snek es una aplicación con el propósito de ayudar a personas a aprender
        python de una manera interactiva!
      </h2>
      <br />
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

export default PageHome;
