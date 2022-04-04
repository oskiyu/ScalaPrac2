package main

object Main {
  def main(args: Array[String]) = {

    val tabOriginal = List(
      List(1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1)
    )

    val tab = new Tablero(tabOriginal).SetElem(2, 1, 0)
    tab.Imprimir()
  }
}
