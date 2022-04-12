package main

/**
 * Objeto estático con las funciones para crear un tablero aleatorio.
 * Ya que la función debe ejecutarse antes que el propio constructor, no podemos
 * usar una función de esa clase: debemos sacarla a un objeto singleton.
 */
object Tablero {

  /** De momento, la ficha se representa por un número. */
  type Ficha = Int;
  /** El tablero es una lista de listas (lista de columnas). */
  type FichasTablero = List[List[Ficha]]

  private val rng = scala.util.Random

  /**
   * Genera una columna (una lista) de números aleatorios.
   * Los números estarán dentro de los límites de las fichas (1 - 8),
   * ambos incluidos.
   *
   * @param numElemRestantes Número de elementos añadidos a la lista.
   * @return Lista aleatoria.
   */
  private def GenerarColumnaAleatoria(numElemRestantes: Int): List[Ficha] = {
    if (numElemRestantes == 0) List()
    else (rng.nextInt(8) + 1)::GenerarColumnaAleatoria(numElemRestantes - 1)
  }

  /**
   * Genera un tablero con números aleatorios.
   * Los números estarán dentro de los límites de las fichas (1 - 8),
   * ambos incluidos.
   *
   * @see GenerarColumnaAleatoria
   * @param numColumnasRestantes Número de columnas (tamaño en X).
   * @param numElementosPorColumna Número de filas (tamaño en Y).
   * @return Fichas aleatorias del tablero.
   */
  private def GenerarTableroAleatorio(numColumnasRestantes: Int, numElementosPorColumna: Int): FichasTablero = {
    if (numColumnasRestantes == 1) List(GenerarColumnaAleatoria(numElementosPorColumna))
    else GenerarColumnaAleatoria(numElementosPorColumna)::GenerarTableroAleatorio(numColumnasRestantes - 1, numElementosPorColumna)
  }

}

/**
 * Representa un estado del tablero.
 *
 * @param data Array de arrays con las fichas.
 */
class Tablero(data: List[List[Int]], puntuacion: Int, vidas: Int) {

  import Tablero._

  final val VALOR_FICHA_VACIA: Ficha = 0
  final val VALOR_FICHA_BOMBA: Ficha = 8
  final val VALOR_FICHA_MARCADA: Ficha = VALOR_FICHA_VACIA
  final val NUM_FICHAS_CONTIGUAS: Int = 3

  /**
   * Crea un nuevo tablero con fichas aleatorias.
   *
   * @param width Número de columnas.
   * @param height Número de filas.
   */
  def this(width: Int, height: Int, vidas: Int) = {
    this(Tablero.GenerarTableroAleatorio(width, height), puntuacion = 0, vidas = vidas)
  }

  /** Número de columnas. */
  def GetWidht(): Int = data.length
  /** Número de filas. */
  def GetHeight(): Int = data(0).length

  /** Comprueba si las coordenadas están dentro del tablero. */
  def CoordenadaValida(posX: Int, posY: Int): Boolean = posX > 0 && posX < GetWidht() && posY > 0 && posY < GetHeight()


  /** Imprime una fila del tablero. */
  private def ImprimirFila(tablero: FichasTablero, fila: Int): Unit = {
    if (tablero.length > 0) {
      print(tablero.head(fila))
      ImprimirFila(tablero.tail, fila)
    }
  }

  /**
   * Imprime el tablero de manera recursiva (por filas).
   *
   * @param fila Fila a imprimir (empieza en 0 y va subiendo).
   */
  private def ImprimirTablero(fila:Int): Unit ={
    if (fila < 0) throw new Exception(s"Se ha intentado imprimir la fila {$fila} menor que 0.")
    if (fila >= GetHeight()) throw new Exception(s"Se ha intentado imprimir la fila {$fila}, pero solo hay {$GetHeight()} filas.")

    ImprimirFila(data, fila)
    print("\n")

    if (fila + 1 < GetHeight())
      ImprimirTablero(fila + 1)
  }

  /** Imprime el tablero. */
  def Imprimir(): Unit = { ImprimirTablero(0) }

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
   * @param tablero main.Tablero anterior.
   * @return Número de fichas del mismo tipo.
   */
  private def NumElementosContiguos(posX: Int, posY: Int, ficha: Ficha, tablero: Tablero): Int = {
    tablero.GetElem(posX, posY) match {
      case x if (x != 0) => {
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
   * @return main.Tablero con todas las fichas marcadas.
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
        // TODO: mover fichas
        // TODO: puntuación
      case false =>
        SetElem(posX, posY, VALOR_FICHA_VACIA)
        // TODO: mover fichas
        // TODO: puntuación
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
   * @return main.Tablero con la ficha sustituida.
   */
  def SetElem(x: Int, y: Int, valor: Ficha): Tablero = {
    if (!CoordenadaValida(x, y)) throw new Exception("Coordenada inválida.")

    new Tablero(Listas.SetElem(x, Listas.SetElem(y, valor, data(x)), data))
  }
  /**
   * Elimina recursivamente los elementos iguales a 0 de una lista, desplazando la columna
   * @param lista Columna a eliminar los elementos
   * @return La lista sin elementos 0, desplazando los elementos, o una lista vacía si solo había elementos 0
   */
  def elimina0(lista: List[Int]): List[Int] = {
    lista.length match {
      case 1 => {
        lista.head match {
          case 0 => {
            List()
          }
          case _ => {
            lista.head :: List()
          }

        }

      }
      case _ => {
        lista.head match {
          case 0 => {
            elimina0(lista.tail)
          }
          case _ => {
            lista.head :: elimina0(lista.tail)
          }

        }
      }
    }
  }
  /**
   * Desplaza las columnas de un tablero, hacia la izquierda si se quedan vacias y desplaza los elementos de las propias columnas hacia abajo
   * @param data Los datos del tablero a desplazar
   * @return Los datos nuevos o una lista vacia si todos los elemntos son 0
   */
  def desplazarColumnas(data:List[List[Int]]): List[List[Int]] ={
    data.length match {
      case 1 =>{
        val columnaNueva = elimina0(data.head)
        columnaNueva match{
          case List() => {
            List()
          }
          case _ =>{
            columnaNueva :: List()
          }
        }
      }
      case _ =>{
        val columnaNueva = elimina0(data.head)
        columnaNueva match{
          case List() => {
            desplazarColumnas(data.tail)
          }
          case _ =>{
            columnaNueva :: desplazarColumnas(data.tail)
          }
        }
      }
    }
  }
}
