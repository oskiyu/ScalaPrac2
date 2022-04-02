package main

import org.scalatest.FunSuite

class ListasTest extends FunSuite {
  test("testSetElem") {
    assert(Listas.SetElem(1, 0, List(0, 1, 2)).equals(List(0, 0, 2)))
    assertThrows[Exception](Listas.SetElem(-1, 0, List(0)))
    assertThrows[Exception](Listas.SetElem(1, 0, List(0)))
  }

}
