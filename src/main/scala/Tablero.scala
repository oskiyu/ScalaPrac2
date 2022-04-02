/**
 * Representa un estado del tablero.
 *
 * @param data Array de arrays con las fichas.
 */
class Tablero(data: List[List[Int]]) {

  type Ficha = Int;
  type FichasTablero = List[List[Ficha]]

  final val VALOR_FICHA_VACIA: Ficha = 0
  final val VALOR_FICHA_BOMBA: Ficha = 8
  final val VALOR_FICHA_MARCADA: Ficha = VALOR_FICHA_VACIA
  final val NUM_FICHAS_CONTIGUAS: Int = 3

  private val rng = scala.util.Random

  private def GetElemIndex(posX: Int, posY: Int): Int = {
    posY * GetWidht() + posX
  }

  //def GenerarTableroAleatorio(sizeX: Int, sizeY: Int): FichasTablero = { }

  def GenerarColumnaAleatoria(numElemRestantes: Int): List[Ficha] = {
    if (numElemRestantes == 0) List()
    else (rng.nextInt(8) + 1)::GenerarColumnaAleatoria(numElemRestantes - 1)
  }
  def GenerarTableroAleatorio(numColumnasRestantes: Int, numElementosPorColumna: Int): FichasTablero = {
    if (numColumnasRestantes == 1) List(GenerarColumnaAleatoria(numElementosPorColumna))
    else GenerarColumnaAleatoria(numElementosPorColumna)::GenerarTableroAleatorio(numColumnasRestantes - 1, numElementosPorColumna)
  }

  /*def this(width: Int, height: Int) {
    this()
  }*/

  /** Número de columnas. */
  def GetWidht(): Int = data.length
  /** Número de filas. */
  def GetHeight(): Int = data(0).length

  /** Comprueba si las coordenadas están dentro del tablero. */
  def CoordenadaValida(posX: Int, posY: Int): Boolean = posX > 0 && posX < GetWidht() && posY > 0 && posY < GetHeight()

  //Imprime la fila parametro del tablero parametro.
  def ImprimirFila(tablero: FichasTablero,fila: Int): Unit = {
    if (tablero.length > 0) {
      print(tablero.head(fila))
      ImprimirFila(tablero.tail, fila)
    }
  }

  //Imprime el tablero parametro recursivamente por filas.
  def ImprimirTablero(tablero: FichasTablero, fila:Int): Unit ={
    if (fila < tablero.head.length){
      ImprimirFila(tablero,fila)
      print("\n")
      ImprimirTablero(tablero,fila+1)
    }
  }

  /**
   * Cuenta el número de fichas contiguas del mismo tipo.
   * Es una función recursiva. Obtiene una copia del tablero original, y hace lko siguiente:
   *    Si la ficha no coincide con la que buscamos, devuelve 0.
   *    Si la ficha sí es la que buscamos: sustituimos dicha ficha con un 0 en nuestro tablero local,
   *    y hacemos llamada recursiva con este nuevo tablero. De esta manera podemos hacer fácilmente la
   *    recursividad sin bucles infinitos, ya que las casillas previamente visitadas no volverán a ser contadas ya que
   *    ahora valen 0. Estos cambios en el tablero no se ven reflejados en el tablero original.
   *
   * @note Sólo tiene en cuenta las fichas que están a una distancia de 1,
   *       sin tener en cuenta las diagonales.
   * @note Las coordenadas deben ser válidas.
   *
   * @param posX Columna de la ficha.
   * @param posY Fila de la ficha.
   * @param ficha Ficha que estamos contando.
   * @param tablero Tablero anterior.
   * @return Número de fichas del mismo tipo.
   */
  private def NumElementosContiguos(posX: Int, posY: Int, ficha: Ficha, tablero: Tablero): Int = {
    tablero.GetElem(posX, posY) match {
      case ficha => {
        val nuevoTab = tablero.SetElem(posX, posY, VALOR_FICHA_MARCADA)

        (1 + NumElementosContiguos(posX + 1, posY, ficha, nuevoTab) + NumElementosContiguos(posX - 1, posY, ficha, nuevoTab)
          + NumElementosContiguos(posX, posY + 1, ficha, nuevoTab) + NumElementosContiguos(posX, posY - 1, ficha, nuevoTab))
      }

      case _ => 0
    }
  }

  /**
   * Comprueba si se puede marcar la ficha seleccionada.
   *
   * @note Las coordenadas deben ser válidas.
   *
   * @param posX Columna en la que se encunetra la ficha.
   * @param posY Fila en la que se encuentra la ficha.
   * @return Número de fichas del mismo tipo contiguas (sin contar diagonales).
   */
  def SePuedeMarcar(posX: Int, posY: Int): Boolean = {
    NumElementosContiguos(posX, posY, GetElem(posX, posY), this) >= NUM_FICHAS_CONTIGUAS
  }

  /**
   * Marca todas las fichas que van a ser eliminadas.
   * Función recursiva: marca una ficha cada iteración.
   *
   * @param posX Columna de la ficha a marcar.
   * @param posY Fila de la ficha a marcar.
   * @param fichaMarcada Ficha que queremos marcar.
   * @return Tablero con todas las fichas marcadas.
   */
  private def Marcar(posX: Int, posY: Int, fichaMarcada: Ficha): Tablero = {
    GetElem(posX, posY) match {
      case x if (x == fichaMarcada) => {
        val nuevoTab = SetElem(posX, posY, VALOR_FICHA_MARCADA)
        nuevoTab.Marcar(posX + 1, posY, fichaMarcada).Marcar(posX - 1, posY, fichaMarcada)
          .Marcar(posX, posY + 1, fichaMarcada).Marcar(posX, posY - 1, fichaMarcada)
      }

      case _ => this
    }
  }

  /**
   * Se eliminan las piezas correspondientes cuando el jugador toca en la pieza indicada.
   * Devuelve un nuevo tablero con las fichas ya eliminadas.
   *
   * @note La posición pasada debe corresponder a una ficha válida.
   *
   * @param posX Columna de la ficha.
   * @param posY Fila de la ficha.
   * @return Nuevo tablero con las fichas eliminadas.
   */
  def Eliminar(posX: Int, posY: Int): Tablero = {
    if (!SePuedeEliminar(posX, posY)) throw new Exception("No se puede eliminar la ficha seleccionada.")

    SePuedeMarcar(posX, posY) match {
      case true =>
        Marcar(posX, posY, GetElem(posX, posY))
        // TODO: eliminar fichas.
        // TODO: mover fichas
      case false =>
        SetElem(posX, posY, VALOR_FICHA_VACIA)
        // TODO: quitar vidas
    }

  }

  /** Comprueba si la ficha seleccionada se puede eliminar. */
  def SePuedeEliminar(posX: Int, posY: Int): Boolean = {
    CoordenadaValida(posX, posY) && SePuedeEliminar(GetElem(posX, posY))
  }

  /** Comprueba si la ficha seleccionada se puede eliminar. */
  def SePuedeEliminar(ficha: Ficha): Boolean = {
    ficha > 0 && ficha <= 8
  }

  /**
   * Devuelve la ficha en la posición dada.
   *
   * @note Deben pasarse coordenadas válidas.
   *
   * @param x Columna.
   * @param y Fila.
   */
  def GetElem(x: Int, y: Int): Ficha = {
    if (!CoordenadaValida(x, y)) throw new Exception("Coordenada inválida.")

    data(x)(y)
  }

  /**
   * Devuelve un nuevo tablero donde se ha sustituido la ficha en la posición dada
   * por la ficha dada.
   *
   * @note Deben pasarse coordenadas válidas.
   *
   * @param x Columna de la ficha que se va a sustituir.
   * @param y Fila de la ficha que se va a sustituir.
   * @param valor Nueva ficha.
   * @return Tablero con la ficha sustituida.
   */
  def SetElem(x: Int, y: Int, valor: Ficha): Tablero = {
    if (!CoordenadaValida(x, y)) throw new Exception("Coordenada inválida.")

    val nuevoTab = data
    //nuevoTab(GetElemIndex(x, y)) = valor

    new Tablero(nuevoTab)
  }

}
