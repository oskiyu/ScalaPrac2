type Ficha = Int;

class Tablero(data: Array[Array[Ficha]]) {

  /*def this(width: Int, height: Int) {
    this()
  }*/

  def GetWidht(): Int = data.length
  def GetHeight(): Int = data(0).length

  def CoordenadaValida(posX: Int, posY: Int): Boolean = posX > 0 && posX < GetWidht() && posY > 0 && posY < GetHeight()

  def NumElementsosContiguos(posX: Int, posY: Int, ficha: Ficha, tablero: Tablero): Int = {
    tablero.GetElem(posX, posY) match {
      case ficha => {
        val nuevoTab = tablero.SetElem(posX, posY, 0)

        (1 + NumElementosContiguos(posX + 1, posY, ficha, nuevoTab) + NumElementosContiguos(posX - 1, posY, ficha, nuevoTab)
          + NumElementosContiguos(posX, posY + 1, ficha, nuevoTab) + NumElementosContiguos(posX, posY - 1, ficha, nuevoTab))
      }

      case _ => 0
    }
  }

    def SePuedeMarcar(posX: Int, posY: Int): Boolean = {
      NumElementosContiguos(posX, posY, GetElem(posX, posY), this) >= 3
    }

  def Marcar(posX: Int, posY: Int, fichaMarcada: Ficha): Tablero = {
    GetElem(posX, posY) match {
      case x if (x == fichaMarcada) => {
        val nuevoTab = SetElem(posX, posY, 0)
        nuevoTab.Marcar(posX + 1, posY, fichaMarcada).Marcar(posX - 1, posY, fichaMarcada)
          .Marcar(posX, posY + 1, fichaMarcada).Marcar(posX, posY - 1, fichaMarcada)
      }

      case _ => this
    }
  }

  def Marcar(posX: Int, posY: Int): Tablero = {
    SePuedeMarcar(posX, posY) match {
      case true =>
        Marcar(posX, posY, GetElem(posX, posY))
      case false =>
        SetElem(posX, posY, 0)
        // TODO: quitar vidas
    }

  }

  def GetElem(x: Int, y: Int): Ficha = {
    data(x)(y)
  }

  def SetElem(x: Int, y: Int, valor: Ficha): Tablero = {
    val nuevoTab = data
    nuevoTab(x)(y) = valor

    new Tablero(nuevoTab)
  }

}
