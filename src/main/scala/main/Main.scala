package main

import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    Jugar(new Tablero(6, 6, 3))
  }

  def Jugar(tablero: Tablero): Unit = {
    println(s"Puntuación: ${tablero.GetPuntuacion()}")
    println(s"Vidas: ${tablero.GetNumVidas()}")
    tablero.Imprimir()

    println("Elige la pieza")

    val coordenadas = PedirPieza(tablero)

    Jugar(tablero.Eliminar(coordenadas._1, coordenadas._2))
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

      return (x, y)
    }
    catch {
      case e: Exception =>
        println ("Fuck you")
        PedirPieza (tablero)
    }
  }

}
