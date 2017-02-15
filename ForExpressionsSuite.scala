package co.com.scalatraining.collections

import org.scalatest.FunSuite

class ForExpressionsSuite extends FunSuite{

  test("Smoke test"){
    assert(true)
  }

  test("for-comp imperativo 1"){
    val l = 1 to 10

    var cont = 0
    for{
      a <- l
    } cont+=1

    assert(cont==10)
  }

  test("for-comp imperativo 2"){
    case class CC()

    assertDoesNotCompile {
      "for{ " +
        " a <- CC() " +
        "} println(a)"
    }

  }

  test("for-comp para mapeo"){
    def _map[T, S](l: List[T], fn: T => S): List[S] =  for(i <- l) yield fn(i) //yield produce una lista
    assert(_map[Int, Int](List(1,2,3), x => x*2) == List(2,4,6))
  }

  test("Ultimo elemento de una lista"){
    def ultimo[T](l: List[T]): T = {
      var x: Any = 0
      for(i <- l) x = i
      x.asInstanceOf[T]
    }
    assert(ultimo(List(1.2, 2.3, 3.4, 4.5)) == 4.5)
  }

  test("Factorial imperativo"){
    def fact(n: Int): Int = {
      val r = 1 to n
      var mult = 1
      for {
        num <- r
      } mult *= num
      mult
    }
    assert(fact(5) == 120)
  }

  test("Tail imperativo"){
    def tail[A](l: List[A]): List[A] = {
      if(l.length == 0 || l.length == 1){
        Nil
      }else{
        var cont = 0
        for{
          i <- l
          if (cont != 0) || {cont+=1; false}
        } yield i
      }
    }
    assert(tail(List(1,2,3,4)) == List(2, 3, 4))
  }

  test("Yield test"){
    val xValues = 1 to 4
    val yValues = 1 to 2
    val coordinates = for {
      x ← xValues
      y ← yValues
    } yield (x, y)
    assert(coordinates(4) == (3,1))
  }

  test("Clases"){
    assert(classOf[String].getCanonicalName == "java.lang.String")
    assert(classOf[String].getSimpleName == "String")
  }
}