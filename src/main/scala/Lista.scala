class Lista[T](listaNativa: List[T]) {

  def GetElem(pos: Int): T = listaNativa(pos)
  def Reverse(): Lista[T] = {
    new Lista[T](GetHead()::listaNativa.tail)
  }

  def GetHead(): T = listaNativa.head
  def GetTail(): Lista[T] = new Lista[T](listaNativa.tail)

}

def Reverse[T](lista: List[T]): List[T] = {
  if (lista.isEmpty) List[T]()
  else lista.head::Reverse(lista.tail)
}
