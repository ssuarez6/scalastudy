package co.com.scalatraining.collections

import org.scalatest.FunSuite

class ListSuite extends FunSuite {

  test("Una List se debe poder construir") {

    val lista: List[Int] = List(1, 2, 3, 4)
    val lista2 = 1 :: 2 :: 3 :: 4 :: Nil
    assert(lista == lista2)
  }

  test("Una List se debe poder transformar") {

    def f(s:String):String = s+"prueba"

    val lista = List("1", "2", "3")
    val lista2 = lista.map(dato => dato + "prueba")
    val lista3 = lista.map(dato => f(dato))

    assert(lista2.head == "1prueba")
    assert(lista != lista2)
    assert(lista2 == lista3)
  }

  test("Una lista de tuplas se debe poder mapear"){
    val lt = List[(Int, String)] ((1, "one"), (2, "two"), (3, "three"), (4, "four"))
    def swap(ti: (Int, String)): (String, Int) = (ti._2, ti._1)
    //println(lt.map(swap))
  }

  test("Una lista se debe poder acumular") {
    val lista = List(1, 2, 3, 4)
    assertResult(10) {
      lista.fold(0) { (acumulado, item) =>
        acumulado + item
      }
    }
  }

  test("fold sobre una List de objetos"){
    case class MyCaseClass(i:Int, var s:String)
    val lista: List[MyCaseClass] = List( MyCaseClass(1,"1"),  MyCaseClass(2, "2"))

    assertResult("12"){
      lista.map(x=>x.s).fold(""){(acc,item)=>acc+item}
    }
  }

  test("test dificil") {
    val lista = List(1, 2, 3, 4, 6, 7, 8, 9, 10)
    assert(true)
  }

  test("Una lista se debe poder acumular en una direccion determinada (izquierda)") {
    val lista = List("Andres", "Felipe", "Juan", "Carlos")
    assertResult("1.Andres,2.Felipe,3.Juan,4.Carlos,") {
      var cont = 0
      lista.foldLeft("") { (resultado, item) =>
        cont = cont + 1
        resultado + cont + "." + item + ","
      }
    }
  }

  test("Una lista se debe poder acumular en una direccion determinada (derecha)") {
    val lista = List("Andres", "Felipe", "Juan", "Carlos")
    assertResult("1.Carlos,2.Juan,3.Felipe,4.Andres,") {
      var cont = 0
      lista.foldRight("") { (item, resultado) =>
        cont = cont + 1
        resultado + cont + "." + item + ","
      }
    }
  }



  test("Se debe poder consultar el primer elemento de una lista de forma insegura") {
    val lista = List(1, 2, 3, 4)
    assertResult(1) {
      lista.head
    }
  }


  test("Que pasa si hacemos head a una List()") {
    val lista = List()
    assertThrows[NoSuchElementException] {
      lista.head
    }
  }

  test("Se debe poder acceder al primer elemento de List() de forma segura") {
    val lista = List()
    val result = lista.headOption
    assert(result == None)
  }

  test("Acceder al primer elemento de List() de forma segura con elementos"){
    val l = List(1,2,3,4,5,6,7)
    var result = l.headOption
    assert(result == Some(1))
  }

  test("Se debe poder obtener todos los elementos de una lista sin el primer elemento") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(2, 3, 4)) {
      lista.tail
    }
  }

  test("Se debe poder sacar el segundo elemento con head y tail"){
    assert(List(1, 2, 3).tail.headOption.get == 2)
  }

  test("Que pasa si hacemos tail a un List()") {
    val lista = List()
    assertThrows[UnsupportedOperationException] {
      val res =  lista.tail
    }
  }

  test("Quiero probar tuplas"){
    val tupla = (1, 2,"3", List(1, 2, 3))
    assert(tupla._2 == 2)
    assert(tupla._4.tail.head == 2)
  }

  test("Una lista se debe poder dividir") {
    val lista = List(1, 2, 3, 4)
    val t = lista.splitAt(2)
    assert(t._1 == List(1, 2) && t._2 == List(3, 4))
  }

  test("splitAt al final de una lista debe entregar como segundo valor de la tupla una lista vacía"){
    assert(List(1,2,3).splitAt(3)._2 == Nil)
  }

  test("Se puede hacer una lista de un solo tipo"){
    assertDoesNotCompile( "val l = List[String](\"1\", \"2\", 3)")
  }

  test("Una lista se debe poder reversar") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(4, 3, 2, 1)) {
      lista.reverse
    }
  }

  test("A una lista se le debe poder eliminar elementos con drop") {
    val lista = List(1, 2, 3, 4)
    val dropped =lista.drop(2)

    assertResult(List(3, 4)) {
      dropped
    }
  }

  test("drop hasta posición"){
    assert(List(1.1,7.2,3.3,4.4).drop(2) == List(3.3, 4.4))
  }

  test("A una lista se le pueden descartar elementos en una direccion determinada (right)") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(1, 2)) {
      lista.dropRight(2)
    }
  }


  test("Una lista se debe poder filtrar con una hof") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(2, 4)) {
      lista.filter(x =>
        x % 2 == 0
      )
    }
  }

  test("Sintaxis diferente para filtros en listas"){
    var l = List(1,2,3,4,5,6)
    def esPar(n: Int): Boolean = n%2 == 0
    var f1 = l.filter(x => x%2 == 0)
    var f2 = l.filter(x => esPar(x))
    var f3 = l.filter(_%2 == 0)
    var f4 = l.filter(esPar)
    assert(f1 == f2 && f2 == f3 && f3 == f4)
  }

  test("Una lista se debe poder recorrer imperativamente") {
    val lista = List(1, 2, 3, 4)
    assertResult(10) {
      var sum = 0
      lista.foreach((x) =>
        sum += x
      )
      sum
    }
  }

  test("Una lista se debe poder serializar") {
    val lista = List(1, 2, 3, 4)
    assertResult("1&2&3&4") {
      lista.mkString("&")
    }
  }

  test("Se pueden poder sumar los elementos de una lista") {
    val lista = List(1, 2, 3, 4)
    assertResult(10) {
      lista.sum
    }
  }

  test("Penultimo elemento con tail y head"){
    val l = 1 to 100 toList
    val pen = l.reverse.tail.head
    assert(pen == 99)
  }

  test("Ultimo elemento con fold"){
    val l = 1 to 100 toList
    val last = l.fold(l.head)((_, item) => item)
    assert(last == 100)
  }

  test("Implementando map con fold"){
    def _map(lista:List[Int], fn:(Int) => Int): List[Int] = {
      //:+ para hacer append
      lista.fold(Nil)( (acc, item) =>  acc.asInstanceOf[List[Int]] :+ fn(item.asInstanceOf[Int])).asInstanceOf[List[Int]]
    }
    val l = List(1,2,3)
    val l2 = _map(l, x => x*2)
    assert(l2 == List(2,4,6))
  }

  test("Map recursivo"){
    def _map(l: List[Int], fn:(Int) => Int): List[Int] = {
      l match{
        case x :: xs => _map(xs, fn) :+ fn(x)
        case _ => Nil
      }
    }
    val l = List[Int](1,2,3)
    val l2 = _map(l, x => x*2).reverse //machete!
    assert(l2 == List(2,4,6))
  }
}