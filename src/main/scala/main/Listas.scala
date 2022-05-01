package main

import scala.annotation.tailrec

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
       case x if x < -1 => throw new Exception(s"SetElem: Índice {$x} por debajo de 0.")
       case x if x >= lista.length => throw new Exception(s"SetElem: Índice {$x} por encima del tamaño de la lista (${lista.length}.")

       case 0 => elem :: lista.tail
       case _ => lista.head :: SetElem(pos - 1, elem, lista.tail)
     }
  }

  /**
   * Devuelve el elemento de la lista en la posición dada.
   *
   * @throws Exception, si la posición no está dentro de los límites de la lista.
   * @param lista Lista.
   * @param pos Posición del elemento.
   * @tparam T Tipo.
   * @return Elemento en la posición dada.
   */
  @tailrec
  final def GetElem[T](lista: List[T], pos: Int): T = {
    pos match {
      case x if x < 0 => throw new Exception(s"GetElem: Índice {$x} por debajo de 0.")
      case x if x >= lista.length => throw new Exception(s"GetElem: Índice {$x} por encima del tamaño de la lista (${lista.length}.")

      case 0 => lista.head
      case _ => GetElem(lista.tail, pos - 1)
    }
  }

  /**
   * Crea y devuelve una nueva lista con todos sus elementos asignados el
   * valor dado.
   *
   * @throws Exception, si el número de elmentos es menor que 0.
   * @param value Valor de todos los elementos de la lista.
   * @param length Número de elementos de la lista.
   * @tparam T Tipo.
   * @return Lista con 'length' elementos con el valor 'value'.
   */
  final def GenerarLista[T](value: T, length: Int): List[T] = {
    length match {
      case x if x < 0 => throw new Exception(s"GenerarLista: número de elementos {$x} por debajo de 0.")

      case 0 => List()
      case _ => value::GenerarLista(value, length - 1)
    }
  }

  /**
   * Devuelve la posición en la lista del primer elemento que cumpla la condición dada.
   *
   * @param func Condición que debe cumplir el elemento.
   * @param list Lista.
   * @return -1 si no hay ningún elemento que cumpla los requisitos.
   */
  final def FindIndex[T](func: T => Boolean, list: List[T]): Int = {
    if (list.isEmpty) return -1

    if (func(list.head)) 0
    else 1 + FindIndex(func, list.tail)
  }

  /** Cuenta el número de elementos de la lista que cumplen la condición dada. */
  final def Count[T](func: T => Boolean, list: List[T]): Int = {
    if (list.isEmpty) return 0

    if (func(list.head)) 1 + Count(func, list.tail)
    else Count(func, list.tail)
  }

}
