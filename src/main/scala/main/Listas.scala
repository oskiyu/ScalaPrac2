package main

/** Objeto con funciones básicas para listas. */
object Listas {

  /**
   * Cambia el valor del elemento en la posición dada.
   *
   * @throws Exception, si la posición no está dentro de los límites de la lista.
   * @param pos Posición del elemento (empieza en 0).
   * @param elem Nuevo valor.
   * @param lista Lista original.
   * @tparam T Tipo.
   * @return Nueva lista con el valor cambiado.
   */
  final def SetElem[T](pos: Int, elem: T, lista: List[T]): List[T] = {
     pos match {
       case x if (x < -1) => throw new Exception(s"SetElem: Índice {$x} por debajo de 0.")
       case x if (x >= lista.length) => throw new Exception(s"SetElem: Índice {$x} por encima del tamaño de la lista (${lista.length}.")

       case 0 => elem :: lista.tail
       case _ => lista.head :: SetElem(pos - 1, elem, lista.tail)
     }
  }

}
