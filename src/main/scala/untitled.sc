def Reverse[T](lista: List[T]): List[T] = {
  if (lista.isEmpty) List[T]()
  else lista.head::Reverse(lista.tail)
}

def Append[T](lista1: List[T], lista2: List[T]) = List[T] = {

}

Reverse(List(0, 1, 2, 3))