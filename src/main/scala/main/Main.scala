package main

import scala.annotation.tailrec
import scala.io.StdIn

object Main {

  def main(args: Array[String]): Unit = {
    InicializarJuego()
  }

  @tailrec
  def Jugar(tablero: Tablero): Unit = {
    if (tablero.IsEmpty()){
      println("¡Enhorabuena ganaste!")
      println(s"Puntuación final: ${tablero.GetPuntuacion()}")

      System.exit(0)
    }

    if (tablero.GetNumVidas() == 0) {
      println("Te quedaste sin vidas")
      println(s"Puntuación final: ${tablero.GetPuntuacion()}")

      System.exit(0)
    }

    println(s"Puntuación: ${tablero.GetPuntuacion()}")
    println(s"Vidas: ${tablero.GetNumVidas()}")

    tablero.Imprimir()

    val mejorJugada = IA.GetJugadaParalela(tablero)
    val puntos = mejorJugada._3

    if (puntos >= 0) println(s"Mejor jugada: ${mejorJugada._1}, ${mejorJugada._2}, puntuacion: $puntos")
    else println("El algoritmo no fue capaz de encontrar una jugada válida.")

    println("Elige la pieza")

    val coordenadas = PedirPieza(tablero)

    Jugar(tablero.Eliminar(coordenadas._1, coordenadas._2))
  }

  def InicializarJuego() : Unit = {
    PedirDificultad() match {
      case 1 => Jugar(new Tablero(9, 11, 8, 2, 2))
      case 2 => Jugar(new Tablero(12, 16, 10, 5, 3))
      case 3 => Jugar(new Tablero(25, 15, 15, 7, 5))
    }
  }

  @tailrec
  def PedirDificultad(): Int = {
    println("Elige una dificultad")

    try {
      StdIn.readLine().toInt
    }
    catch {
      case e: Exception =>
        println("Dificultad erronea")
        PedirDificultad()
    }
  }


  def PedirPieza(tablero: Tablero): (Int, Int) = {

    val tempx = StdIn.readLine()
    val tempy = StdIn.readLine()

    try {
      val x = tempx.toInt
      val y = tempy.toInt
      if (!tablero.CoordenadaValida(x,y)){
        println("Coordenadas Inválidas")
      }
      if (!tablero.CoordenadaValida(x, y) || !tablero.SePuedeEliminar(x, y)) {
        return PedirPieza(tablero)
      }

      (x, y)
    }
    catch {
      case e: Exception =>
        println ("Elige una posición válida")
        PedirPieza (tablero)
    }
  }

}
