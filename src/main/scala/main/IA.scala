package main

object IA {

  private val rng = scala.util.Random

  /** Calcula la mejor jugada posible usando un algoritmo de
   * Monte Carlo con 5 intentos aleatorios.
   *
   * @param tablero Tablero sobre el que se va a calcular la jugada.
   * @return (posX, posY): posición de la ficha con la mejor jugada de entre las intentadas.
   */
  def GetJugada(tablero: Tablero): (Int, Int) = {
    val jugada = GetJugada(tablero, 10)

    (jugada._1, jugada._2)
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

    val x = rng.nextInt(tablero.GetWidht())
    val y = rng.nextInt(tablero.GetHeight())

    val puntuacion = if (tablero.SePuedeEliminar(x, y)) tablero.Eliminar(x, y).GetPuntuacion()
                     else -1

    val rec = GetJugada(tablero, numRestantes - 1)

    if (puntuacion >= rec._3) (x, y, puntuacion)
    else rec
  }

}
