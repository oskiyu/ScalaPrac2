package main

object Main {
  def main(args: Array[String]) = {

    val tabOriginal = List(
      List(1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1),
      List(1, 1, 2, 1, 1),
      List(1, 1, 0, 1, 1),
      List(1, 1, 0, 3, 1)
    )

    val tab = new Tablero(tabOriginal, 1, 1);
    val newTab = tab.Eliminar(1, 1)
    tab.Imprimir()
    println("--------")
    newTab.Imprimir()
  }
}
