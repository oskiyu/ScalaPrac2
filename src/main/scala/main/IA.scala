package main

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object IA {

  private val rng = scala.util.Random

  // Versión paralelizada

  /**Calcula la mejor jugada posible usando un algoritmo de
   * Monte Carlo con 10 intentos aleatorios.
   * Calcula los intentos de forma paralela.
   */
  def GetJugadaThreaded(tablero: Tablero): (Int, Int, Int) = {
    ProcesarFuturos(GetJugadaParalela(tablero, 10))
  }

  /** Procesa la lista de futuros, recolectando sus resultados y devolviendo el mejor. */
  private def ProcesarFuturos(lista: List[Future[(Int, Int, Int)]]): (Int, Int, Int) = {
    if (lista.isEmpty) return (0, 0, -1)

    val result = Await.result(lista.head, Duration.Inf)

    val resultRec = ProcesarFuturos(lista.tail)

    if (resultRec._3 >= result._3) resultRec
    else result
  }

  /** Futuro que calcula una jugada. */
  def GetJugadaParalela(tablero: Tablero): Future[(Int, Int, Int)] = Future {
    val x = rng.nextInt(tablero.GetNumColumnasNoVacias())
    val y = tablero.GetHeight() - rng.nextInt(tablero.GetHeight(x))

    val puntuacion = if (tablero.SePuedeEliminar(x, y)) tablero.Eliminar(x, y).GetPuntuacion()
    else -1

    (x, y, puntuacion)
  }

  /** Devuelve una lista con los futuros que calculan las jugadas. */
  def GetJugadaParalela(tablero: Tablero, numRestantes: Int): List[Future[(Int, Int, Int)]] = {
    val output = GetJugadaParalela(tablero)

    if (numRestantes == 1) List(output)
    else output::GetJugadaParalela(tablero, numRestantes - 1)
  }

  // Versión no paralelizada.

  /** Calcula la mejor jugada posible usando un algoritmo de
   * Monte Carlo con 10 intentos aleatorios.
   *
   * @param tablero Tablero sobre el que se va a calcular la jugada.
   * @return (posX, posY): posición de la ficha con la mejor jugada de entre las intentadas.
   */
  def GetJugada(tablero: Tablero): (Int, Int, Int) = {
    val jugada = GetJugada(tablero, 10)

    (jugada._1, jugada._2, jugada._3)
  }

  /**
   * Algoritmo Monte Carlo:
   * Escoge una posición aleatoria y calcula la puntuación obtenida
   * al eliminar esa ficha.
   *
   * Después hace una llamada recursiva para conseguir otro intento.
   *
   * Finalmente, compara el primer intento con el valor dev
   * @param tablero Tablero sobre el que se va a calcular la jugada.
   * @param numRestantes Número de llamadas recursivas (intentos aleatorios) restantes.
   * @return (posX, posY, puntuacionDelIntento): intento aleatorio junto a su puntuación.
   */
  private def GetJugada(tablero: Tablero, numRestantes: Int): (Int, Int, Int) = {
    if (numRestantes == 0) return (0, 0, -1)

    // Únicamente escoge posiciones que tengan fichas.
    val x = rng.nextInt(tablero.GetNumColumnasNoVacias())
    val y = tablero.GetHeight() - rng.nextInt(tablero.GetHeight(x))

    val puntuacion = if (tablero.SePuedeEliminar(x, y)) tablero.Eliminar(x, y).GetPuntuacion()
                     else -1

    val rec = GetJugada(tablero, numRestantes - 1)

    if (puntuacion >= rec._3) (x, y, puntuacion)
    else rec
  }

}
