package main

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq
/**
 * Objeto estático con las funciones para crear un tablero aleatorio.
 * Ya que la función debe ejecutarse antes que el propio constructor, no podemos
 * usar una función de esa clase: debemos sacarla a un objeto singleton.
 */
object Tablero {

  /** De momento, la ficha se representa por un número. */
  type Ficha = Int

  /** El tablero es una lista de listas (lista de columnas). */
  type FichasTablero = ParSeq[ParSeq[Ficha]]

  private val rng = scala.util.Random


  // Para el constructor que genera un tablero aleatorio.

  /**
   * Genera una columna (una lista) de números aleatorios.
   * Los números estarán dentro de los límites de las fichas (1 - numLimite),
   * ambos incluidos.
   *
   * @param numElemRestantes Número de elementos añadidos a la lista.
   * @param numColores Número de colores a introducir
   *
   * @return ParSeqa aleatoria.
   */
  private def GenerarColumnaAleatoria(numElemRestantes: Int, numColores: Int): ParSeq[Ficha] = {
    if (numElemRestantes == 0) ParSeq()
    else (((rng.nextInt(numColores) + 1)::GenerarColumnaAleatoria(numElemRestantes - 1, numColores).toList)).par
  }

  /**
   * Genera tantas bombas en el tablero como indique el parámetro nBombas
   * @param width Longitud del tablero
   * @param height Altura del tablero
   * @param nBombas Numero de bombas a introducir
   * @param data Datos del tablero
   * @return El tablero con nBombas
   */
  @tailrec
  def GenerarBombas(width: Int, height: Int, nBombas: Int, data: ParSeq[ParSeq[Int]]): ParSeq[ParSeq[Int]] = {
    nBombas match {
      case 0 =>
        data

      case 1 =>
        val x = rng.nextInt(width)
        val y = rng.nextInt(height)

        Listas.SetElem(x, Listas.SetElem(y, 8, data(x)), data)

      case _ =>
        val x = rng.nextInt(width)
        val y = rng.nextInt(height)

        GenerarBombas(width,height,nBombas - 1, Listas.SetElem(x, Listas.SetElem(y, 8, data(x)), data))
    }

  }

  /**
   * Genera un tablero con números aleatorios.
   * Los números estarán dentro de los límites de las fichas (1 - numLimite),
   * ambos incluidos.
   *
   * @see GenerarColumnaAleatoria
   * @param numColumnasRestantes Número de columnas (tamaño en X).
   * @param numElementosPorColumna Número de filas (tamaño en Y).
   * @param numColores numero de colores del tablero.
   *
   * @return Fichas aleatorias del tablero.
   */
  private def GenerarTableroAleatorio(numColumnasRestantes: Int, numElementosPorColumna: Int, numColores: Int): FichasTablero = {
    if (numColumnasRestantes == 1) ParSeq(GenerarColumnaAleatoria(numElementosPorColumna, numColores))
    else (GenerarColumnaAleatoria(numElementosPorColumna, numColores) ::GenerarTableroAleatorio(numColumnasRestantes - 1, numElementosPorColumna, numColores).toList).par
  }


  // Para la manipulación del tablero.

  /**
   * Cuenta el número de elementos de la columna que son 0:
   * es decir, el número de fichas vacías.
   *
   * @param columna Columna.
   * @return Número de fichas vacías.
   */
  private def NumFichasVacias(columna: ParSeq[Ficha]): Int = {
    columna.length match {
      case 0 => 0
      case _ =>
        val extra = if (columna.head == 0) 1 else 0
        extra + NumFichasVacias(columna.tail)
    }
  }

  /**
   * Desplaza las columnas de un tablero, hacia la izquierda si se quedan vacias y desplaza los elementos
   * de las propias columnas hacia abajo
   * @param data Los datos del tablero a desplazar
   * @return Los datos nuevos o una lista vacia si todos los elemntos son 0
   */
  def EliminarColumnasVacias(data: ParSeq[ParSeq[Int]]): ParSeq[ParSeq[Int]] ={
    if (data.isEmpty) return List().par

    NumFichasVacias(data.head) == data.head.length match {
      case true => List().par
      case false => (data.head :: EliminarColumnasVacias(data.tail).toList).par
    }
  }

  /**
   * Comprueba si una columna está vacía: es decir, si todas sus fichas son 0.
   * @param list Columna
   */
  @tailrec
  private def EsColumnaVacia(list: ParSeq[Ficha]): Boolean = {
    list.length match {
      case 0 => true
      case _ =>
        if (list.head == 0) EsColumnaVacia(list.tail)
        else false
    }
  }

  /**
   * Desplaza las fichas de una columna: de tal manera que las fichas vacías quedan en
   * la parte superior mientras que las fichas con valor quedan en la parte inferior.
   *
   * Su funcionamiento es el siguiente:
   *
   * - Primero contamos el número de fichas vacías que contiene la columna.
   *
   * - Después, generamos una lista con un número de 0 equivalente a la cantidad
   * de fichas vacías de la columna original.
   *
   * - Por último, añadimos a esta lista de ceros todas las fichas no vacías (distintas de 0)
   * de la columna original.
   *
   * De esta manera, nos aseguramos de que las fichas vacías estén al principio de la columna (en la parte superior).
   *
   * @param col Columna a desplazar.
   * @return Nueva columna con las fichas desplazadas.
   */
  private def DesplazarColumna(col: ParSeq[Ficha]): ParSeq[Ficha] = {
    val numCeros = NumFichasVacias(col)
    val fichasSuperiores = Listas.GenerarParSeqa(0, numCeros)

    (fichasSuperiores.toList ::: FichasNoVacias(col).toList).par
  }

  /**
   * Devuelve una lista en la que únicamente se encuentran las fichas no vacías de la columna dada.
   * @param col Columna.
   * @return ParSeqa con las fichas no vacías de la columna.
   */
  private def FichasNoVacias(col: ParSeq[Ficha]): ParSeq[Ficha] = {
    col.length match {
      case 0 => ParSeq()
      case _ =>
        if (col.head == 0) FichasNoVacias(col.tail)
        else (col.head ::FichasNoVacias(col.tail).toList).par
    }
  }

  /**
   * Devuelve el número de columnas vacías (es decir, que tienen todas sus fichas = 0) del tablero.
   * @param columnas Tablero.
   */
  private def ContarColumnasVacias(columnas: ParSeq[ParSeq[Ficha]]): Int = {
    columnas.length match {
      case 0 => 0
      case _ =>
        if (EsColumnaVacia(columnas.head)) 1 + ContarColumnasVacias(columnas.tail)
        else 0 + ContarColumnasVacias(columnas.tail)
    }
  }

  /**
   * Desplaza todas las columnas del tablero.
   * @param tab Tablero.
   * @return Tablero con todas las columnas desplazadas.
   */
  final def GenerarColumnasDespazadas(tab: ParSeq[ParSeq[Ficha]]): ParSeq[ParSeq[Ficha]] = {
    tab.length match {
      case 0 => ParSeq()
      case _ => (DesplazarColumna(tab.head) :: GenerarColumnasDespazadas(tab.tail).toList).par
    }
  }

  /**
   * Genera un tablero completamente desplazado: todas las fichas de cada columna han sido desplazadas,
   * y todas las columnas en sí han sido desplazadas, de manera que las columnas vacías queden a la derecha.
   * Funciona muy similar a DesplazarColumna().
   *
   * @param data Tablero.
   * @return Tablero desplazado
   */
  def TableroDesplazado(data: ParSeq[ParSeq[Ficha]]): ParSeq[ParSeq[Ficha]] = {
    val tab = GenerarColumnasDespazadas(data)

    val numColumnasVacias = ContarColumnasVacias(data)

    (ColumnasNoVacias(tab).toList :::Listas.GenerarParSeqa(Listas.GenerarParSeqa(0, data.head.length), numColumnasVacias).toList).par
  }

  /**
   * Devuelve todas las columnas no vacías del tablero.
   * @param columnas Tablero.
   * @return ParSeqa con todas las columnas no vacías del tablero.
   */
  private def ColumnasNoVacias(columnas: ParSeq[ParSeq[Ficha]]): ParSeq[ParSeq[Ficha]] = {
    columnas.length match {
      case 0 => ParSeq()
      case _ =>
        if (EsColumnaVacia(columnas.head)) ColumnasNoVacias(columnas.tail)
        else (columnas.head::ColumnasNoVacias(columnas.tail).toList).par
    }
  }

}

/**
 * Representa un estado del tablero.
 *
 * @param data Array de arrays con las fichas.
 */
class Tablero(data: ParSeq[ParSeq[Int]], puntuacion: Int, vidas: Int) {

  import Tablero._

  final val VALOR_FICHA_VACIA: Ficha = 0
  final val VALOR_FICHA_BOMBA: Ficha = 8
  final val VALOR_FICHA_MARCADA: Ficha = VALOR_FICHA_VACIA
  final val NUM_FICHAS_CONTIGUAS: Int = 3

  /**
   * Crea un nuevo tablero con fichas aleatorias con el numero de colores seleccionado y las bombas seleccionadas.
   *
   * @param width Número de columnas.
   * @param height Número de filas.
   * @param vidas Número de vidas.
   * @param nColores Número de colores.
   * @param nBombas Número de bombas.
   */
  def this(width: Int, height: Int, vidas: Int,nColores:Int,nBombas:Int) = {
    this(Tablero.GenerarBombas(width,height,nBombas,Tablero.GenerarTableroAleatorio(width, height,nColores)), puntuacion = 0, vidas = vidas)

  }

  private def GetData() = data

  /** Número de columnas. */
  def GetWidht(): Int = data.length
  /** Número de filas. */
  def GetHeight(): Int = data.head.length

  /** Número de colúmnas con al menos una ficha. */
  def GetNumColumnasNoVacias(): Int = GetWidht() - Tablero.ContarColumnasVacias(GetData())
  /** Devuelve el número de fichas no vacías de una columna. */
  def GetHeight(posX: Int): Int = Listas.Count[Int](x => x > 0, data(posX))
  /** Comprueba si las coordenadas están dentro del tablero. */
  def CoordenadaValida(posX: Int, posY: Int): Boolean = posX >= 0 && posX < GetWidht() && posY >= 0 && posY < GetHeight()


  /** Imprime una fila del tablero. */
  @tailrec
  private def ImprimirFila(tablero: FichasTablero, fila: Int): Unit = {
    if (tablero.isEmpty) return

    print(tablero.head(fila) + "   ")

    ImprimirFila(tablero.tail, fila)
  }

  /**
   * Imprime el tablero de manera recursiva (por filas).
   *
   * @param fila Fila a imprimir (empieza en 0 y va subiendo).
   */
  @tailrec
  private def ImprimirTablero(fila: Int): Unit ={
    if (fila < 0) throw new Exception(s"Se ha intentado imprimir la fila {$fila} menor que 0.")
    if (fila >= GetHeight()) throw new Exception(s"Se ha intentado imprimir la fila {$fila}, pero solo hay {$this->GetHeight()} filas.")

    ImprimirFila(data, fila)
    print("\n")

    if (fila + 1 < GetHeight())
      ImprimirTablero(fila + 1)
  }

  /**
   * Devuelve verdadero si todas las columnas del tablero están vacias
   * @return true si el tablero esta vacio, false si no lo está
   */
  def IsEmpty(): Boolean = {
      ContarColumnasVacias(data)== this.GetWidht()
  }
  /** Imprime el tablero. */
  def Imprimir(): Unit = { ImprimirTablero(0) }

  /**
   * Cuenta el número de fichas contiguas del mismo tipo.
   * Es una función recursiva. Obtiene una copia del tablero original, y hace lko siguiente:
   *    Si la ficha no coincide con la que buscamos, devuelve 0 y el tablero actual.
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
   * @return Número de fichas del mismo tipo y tablero resultante tras la cuenta.
   */
  private def NumElementosContiguos(posX: Int, posY: Int, ficha: Ficha, tablero: Tablero): (Int,Tablero) = {
    if (!CoordenadaValida(posX, posY)) return (0 , tablero)

    tablero.GetElem(posX, posY) match {
      case x if x == ficha =>
        val nuevoTab = tablero.SetElem(posX, posY, VALOR_FICHA_MARCADA)
        val puntTab2 = NumElementosContiguos(posX + 1, posY, ficha, nuevoTab)
        val puntTab3 = NumElementosContiguos(posX - 1, posY, ficha, puntTab2._2)
        val puntTab4 = NumElementosContiguos(posX, posY +1, ficha, puntTab3._2)
        val puntTab5 = NumElementosContiguos(posX, posY - 1, ficha, puntTab4._2)
        (1 + puntTab2._1 + puntTab3._1 + puntTab4._1 + puntTab5._1,puntTab5._2)

      case _ => (0 , tablero)
    }
  }

  /**
   * Comprueba si se puede marcar la ficha seleccionada.
   * Se puede marcar si hay al menos 3 fichas del mismo tipo juntas.
   *
   * @note Las coordenadas deben ser válidas.
   *
   * @param posX Columna en la que se encunetra la ficha.
   * @param posY Fila en la que se encuentra la ficha.
   */
  def SePuedeMarcar(posX: Int, posY: Int): Boolean = {
    NumElementosContiguos(posX, posY, GetElem(posX, posY), this)._1 >= NUM_FICHAS_CONTIGUAS
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
    if (!CoordenadaValida(posX, posY)) return this

    GetElem(posX, posY) match {
      case x if x == fichaMarcada =>
        val nuevoTab = SetElem(posX, posY, VALOR_FICHA_MARCADA)

        nuevoTab.Marcar(posX + 1, posY, fichaMarcada).Marcar(posX - 1, posY, fichaMarcada)
          .Marcar(posX, posY + 1, fichaMarcada).Marcar(posX, posY - 1, fichaMarcada)

      case _ => this
    }
  }

  /**
   * Realiza el efecto de marcado de una bomba, poniendo al valor de la ficha marcada los elementos contiguos a la ficha, devolviendo un tablero
   * con los elementos marcados a 0
   * @param posX Coordenada X de la bomba
   * @param posY Coordenada Y de la bomba
   * @return Tablero tras explosión de bomba
   */
  def MarcadoBomba(posX:Int,posY:Int):Tablero = {
    SetElem(posX, posY, VALOR_FICHA_MARCADA).SetElemBomba(posX -1 , posY, VALOR_FICHA_MARCADA)
      .SetElemBomba(posX, posY - 1, VALOR_FICHA_MARCADA).SetElemBomba(posX + 1, posY, VALOR_FICHA_MARCADA)
      .SetElemBomba(posX, posY + 1, VALOR_FICHA_MARCADA).SetElemBomba(posX + 1, posY + 1, VALOR_FICHA_MARCADA)
      .SetElemBomba(posX - 1, posY - 1, VALOR_FICHA_MARCADA).SetElemBomba(posX + 1, posY - 1, VALOR_FICHA_MARCADA)
      .SetElemBomba(posX - 1, posY + 1, VALOR_FICHA_MARCADA)
  }

  /**
   * Devuelve un tablero en el que se ha sustituido una ficha por 0, a menos que sea una bomba, en cuyo caso se propaga la explosion invocando
   * al método MarcadoBomba para esa posición.
   * @param x Coordenada X del elemento parte de la explosión
   * @param y Coordenada Y del elemento parte de la explosión
   * @param valor Nueva Ficha
   * @return Tablero tras la sustitución
   */
  def SetElemBomba(x: Int, y: Int, valor: Ficha): Tablero = {
    if (!CoordenadaValida(x, y)) return this

    GetElem(x,y) match{
      case VALOR_FICHA_BOMBA =>
        MarcadoBomba(x,y)
      case _ =>
        new Tablero(Listas.SetElem(x, Listas.SetElem(y, valor, data(x)), data), puntuacion = puntuacion , vidas = vidas)
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
    if (!SePuedeEliminar(posX, posY)) throw new Exception(s"No se puede eliminar la ficha seleccionada. {$posX, $posY}")

    SePuedeMarcar(posX, posY) match {
      case true =>
        val tableroMarcado = Marcar(posX, posY, GetElem(posX, posY)).GetData()
        val tableroConFichasDesplazadas = TableroDesplazado(tableroMarcado)
        val numFichasMarcadas = NumElementosContiguos(posX, posY, GetElem(posX, posY), this)._1

        val nuevaPuntuacion = numFichasMarcadas * 10

        new Tablero(tableroConFichasDesplazadas, vidas=vidas, puntuacion= GetPuntuacion() + nuevaPuntuacion)

      case false =>
        if (GetElem(posX, posY) == VALOR_FICHA_BOMBA){
          val tableroMarcado = MarcadoBomba(posX,posY).GetData()
          val tableroConFichasDesplazadas = TableroDesplazado(tableroMarcado)

          new Tablero(tableroConFichasDesplazadas, vidas=vidas, puntuacion=puntuacion + 90)
        }
        else {
          val tableroMarcado = SetElem(posX, posY, VALOR_FICHA_VACIA).GetData()
          val tableroConFichasDesplazadas = TableroDesplazado(tableroMarcado)

          new Tablero(tableroConFichasDesplazadas, vidas = vidas - 1, puntuacion = puntuacion)
        }
    }

  }

  /** Comprueba si la ficha seleccionada se puede eliminar. */
  def SePuedeEliminar(posX: Int, posY: Int): Boolean = {
    CoordenadaValida(posX, posY) && SePuedeEliminar(GetElem(posX, posY))
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

  /** Comprueba si la ficha seleccionada se puede eliminar. */
  private def SePuedeEliminar(ficha: Ficha): Boolean = {
    ficha > 0 && ficha <= 8
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

    new Tablero(Listas.SetElem(x, Listas.SetElem(y, valor, data(x)), data), puntuacion=puntuacion, vidas = vidas)
  }

  def GetPuntuacion(): Int = puntuacion
  def GetNumVidas(): Int = vidas

}
