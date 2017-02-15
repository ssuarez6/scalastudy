package co.com.scalatraining.collections

import org.scalatest.FunSuite

class SetSuite extends FunSuite {

  test("map en un set") {
    val set = Set("1", "2", "3")
    val set2 = set.map(dato => dato + "prueba")
    assert(set != set2)
  }

  test("head en un set") {
    val set = Set(1, 2, 3, 4)
    assertResult(1) {
      set.head
    }
  }


  test("tail en un set") {
    val set = Set(1, 2, 3, 4)
    assertResult(Set(2, 3, 4)) {
      set.tail
    }
  }

  test("split en un set") {
    val set = Set(1, 2, 3, 4)
    val (set2, set3) = set.splitAt(2)
    assert(set2 == Set(1, 2) && set3 == Set(3, 4))
  }

  test("crear nuevo set con un item mas") {
    val set = Set(1, 2, 3, 4)
    assertResult(Set(1, 2, 3, 4, 5)) {
      set + 5
    }
  }

  test("apply en set") {
    val set = Set(1, 2, 3, 4)
    assertResult(true) {
      set.apply(4)
    }
  }

  test("drop en set") {
    val set = Set(1, 2, 3, 4)
    assertResult(Set(3, 4)) {
      set.drop(2)
    }
  }

  test("dropRight set") {
    val set = Set(1, 2, 3, 4)
    assertResult(Set(1, 2)) {
      set.dropRight(2)
    }
  }


  test("filter en set") {
    val set = Set(1, 2, 3, 4)
    assertResult(Set(2, 4)) {
      set.filter(x =>
        x % 2 == 0
      )
    }
  }

  test("foreach en set") {
    val set = Set(1, 2, 3, 4)
    assertResult(10) {
      var sum = 0
      set.foreach((x) =>
        sum += x
      )
      sum
    }
  }

  test("mkString en set") {
    val set = Set(1, 2, 3, 4)
    assertResult("1&2&3&4") {
      set.mkString("&")
    }
  }

  test("sum en set") {
    val set = Set(1, 2, 3, 4)
    assertResult(10) {
      set.sum
    }
  }

  test("Un set debe saber si un elemento aplica un predicado o no"){
    val set = Set(1,2,3,5,7,9)
    assertResult(true){
      set.exists(x => x%2==0)
    } //debe verificar que al menos uno cumpla la condicion
  }

  test("Un set debe saber si todos los elementos aplican un predicado"){
    val set = Set(2,4,6,8)
    assertResult(true){
      set.forall((x=>x%2==0))
    }
  }

  test("intersección de dos conjuntos"){
    val s1 = Set(2,4,5,6,7)
    val s2 = Set(2,4,10,11,12)
    val s3 = s1 intersect s2
    assert(s3 == Set(2,4))
  }

  test("unión de dos conjuntos"){
    val s1 = Set(1,2,3,4,5)
    val s2 = Set(1,2,3,4,5,6)
    val s3 = s1 union s2
    assert(s3 == s2)
  }

  test("Diferencia entre dos conjuntos"){
    val s1 = Set(1,2,3,4,5)
    val s2 = Set(1,2,3,6,7)
    val s3 = s1 diff s2
    assert(s3 == Set(4,5,6,7))
  }
}