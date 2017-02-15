package co.com.scalatraining.collections

import org.scalatest.FunSuite

class MapSuite extends FunSuite {

  test ("Creacion vacia") {
      val mapa1 = Map()
      val mapa2 = Map.empty
      assert(mapa1.isEmpty)
      assert(mapa2.isEmpty)
  }

  test("Un Map se debe poder operar en un for-comp"){
    val mapa = Map(1->"uno", 2->"dos")

    val res = for{
      i <- mapa
      if i._1 == 1
    } yield(i)

    //println(res)
    assert(res.keys.size === 1)
    assert(res.keys.head === 1)
    assert(res.get(mapa.keys.head).get === "uno")
  }

  test("mapValue en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("1" -> 1, "2" -> 4, "3" -> 9)) {
      map.mapValues(valor => valor * valor)
    }
  }

  test("head en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult("1" -> 1) {
      map.head
    }
  }


  test("tail en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("2" -> 2, "3" -> 3)) {
      map.tail
    }
  }

  test("split en un Map") {
    val map: Map[String, Int] = Map("1" -> 1, "2" -> 2, "3" -> 3)
    val (map2, map3) = map.splitAt(2)
    assert(map2 == Map("1" -> 1, "2" -> 2) && map3 == Map("3" -> 3))
  }

  test("crear nuevo Map con un item mas") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4)) {
      map + ("4" -> 4)
    }
  }


  test("drop en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("2" -> 2, "3" -> 3)) {
      map.drop(1)
    }
  }

  test("dropRight en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("1" -> 1, "2" -> 2)) {
      map.dropRight(1)
    }
  }


  test("filter en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4)
    assertResult(Map("2" -> 2, "4" -> 4)) {
      map.filter(dato =>
        dato._2 % 2 == 0
      )
    }
  }

  test("foreach en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(6) {
      var sum = 0
      map.foreach((x) =>
        sum += x._2
      )
      sum
    }
  }

  test("Fold en un Map"){
    val map: Map[String, Int] = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(6) {
      map.foldLeft(0)((acc, item) => acc + item._2)
    }
  }

  test("En una lista del 1 al 100, deben haber 50 pares y 50 impares"){
    val l = 1 to 100 toList
    val m: Map[String, List[Int]] = l.groupBy(x => if(x%2==0) "Pares" else "Impares")
    val pares: List[Int] = m.get("Pares").get
    val impares: List[Int] = m.get("Impares").get
    assert(pares.length == 50 && impares.length==50)
  }

  test("Contando elementos con condiciones en un mapa"){
    val m = Map("uno"-> 1, "dos"->2, "tres"->3, "cuatro" -> 4)
    val res = m.count(x => x._2 % 2 == 0)
    assert(res == 2) //deben haber dos elementos pares
  }

  test("Borrar todos los que no cumplen una condicion"){
    val m = Map("uno"-> 1, "dos"->2, "tres"->3, "cuatro" -> 4, "menos uno"-> -1, "veinte" -> 20)
    //println(m)
    val m2 = m.filterNot(x => x._2 < 3)
    //println(m2)
    assert(m2.size == 3)
  }
}
