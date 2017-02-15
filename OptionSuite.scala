package co.com.scalatraining.monads

import org.scalatest.FunSuite

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class OptionSuite extends FunSuite {

  test("Se debe poder crear un Option con valor"){
    val s = Option{
      1
    }
    assert(s == Some(1))
  }

  test("Se debe poder crear un Option para denotar que no hay valor"){
    val s = None
    assert(s == None)
  }

  test("Es inseguro acceder al valor de un Option con get"){
    val s = None
    assertThrows[NoSuchElementException]{
      val r = s.get
    }
  }

  test("Se debe poder hacer pattern match sobre un Option") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(1)
    var res = ""
    nombre match {
      case Some(nom) => res = nom
      case None => res = "NONAME"
    }
    assert(res == "NONAME")
  }

  test("Se debe poder saber si un Option tiene valor con isDefined") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)
    assert(nombre.isDefined)
  }

  test("Se debe poder acceder al valor de un Option de forma segura con getOrElse") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(1)
    val res = nombre.getOrElse("NONAME")
    assert(res == "NONAME")
  }

  test("Se debe poder acceder al valor de un Option de forma segura con getOrElse (2)") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre: Option[String] = lista(2)
    val res: String = nombre.getOrElse("NONAME")
    assert(res == "Luis")
  }

  test("Un Option se debe poder transformar con un map") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista.head
    val nombreCompleto = nombre.map(s => s + " Felipe")
    assert(nombreCompleto.getOrElse("NONAME") == "Andres Felipe")
  }

  test("Un Option se debe poder transformar con flatMap en otro Option") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista.head

    val resultado = nombre.flatMap(s => Option(s.toUpperCase))
    resultado.map( s => assert( s == "ANDRES"))
  }

  test("No se debe poder hacer un flatmap con una funcion que no retorne el mismo efecto"){
    def foo(i: Int) = Future(1)
    val o = Option(2)
    assertDoesNotCompile("res.flatmap(foo)")
  }

  test("Map puede evaluar a cualquier cosa, flatmap debe ser del mismo efecto de quien se invoque"){
    def foo(i: Int) = "HOLA"
    val o2 = Option(2)
    assertCompiles("o2.map(foo)")
    assertDoesNotCompile("o2.flatmap(foo)")
  }

  test("Un map sobre una lista de Options puede modificar sus options adentro y retornar una lista diferente"){
    def foo(i: Option[Int]): Option[Int] = i.map(x => x+1)
    val l = List(Option(1), None)
    val res: List[Option[Int]] = l.map(foo)
    //println(res)
  }

  test("Combinando maps con flatmaps"){
    def foo(i: Int): Option[Int] = Option(i)
    def bar(i: Int): Option[Int] = Option(i)
    def sumar(i: Int, j: Int) = i+j
    val o1 = Option(1)
    val o2 = Option(2)
    val r1: Option[Option[Int]] = o1.map(vo1 => o2.map(vo2 => sumar(vo1, vo2)))
    val r2: Option[Int] = o1.flatMap(vo1 => o2.map(vo2 => sumar(vo1, vo2)))
    val r3: Option[Int] = o1.flatMap(vo1 => o2.flatMap(vo2 => Option(sumar(vo1, vo2))))
    assert(r2 == r3 && r1.get.get == r2.get)
  }

  test("Un Option se debe poder filtrar con una hof con filter") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val option0 = lista.head
    val option1 = lista(1)
    val res0 = option0.filter(_>10)
    val res1 = option1.filter(_>10)
    val res2 = option0.filter(_>3)

    assert(res0.isEmpty)
    assert(res1.isEmpty)
    assert(res2.contains(5))
  }

  test("for comprehensions en Option") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val s1 = lista(0)
    val s2 = lista(2)
    val resultado = for {
      x <- s1
      y <- s2
    } yield x+y
    assert(resultado == Some(45))
  }

  test("for comprehesions None en Option") {
    val consultarNombre = Some("Andres")
    val consultarApellido = Some("Estrada")
    val consultarEdad = None
    val consultarSexo = Some("M")

    val resultado = for {
      nom <- consultarNombre
      ape <- consultarApellido
      eda <- consultarEdad
      sex <- consultarSexo
    //} yield (nom+","+ape+","+eda+","+sex)
    } yield s"$nom $ape, $eda, $sex"

    assert(resultado.isEmpty)
  }

  test("for comprehensions None en Option") {

    def consultarNombre(dni:String): Option[String] = Some("Felix")
    def consultarApellido(dni:String): Option[String] = Some("Vergara")
    def consultarEdad(dni:String): Option[String] = None
    def consultarSexo(dni:String): Option[String] = Some("M")

    val dni = "8027133"
    val resultado = for {
      nom <- consultarNombre(dni)
      ape <- consultarApellido(dni)
      eda <- consultarEdad(dni)
      sex <- consultarSexo(dni)
    //} yield (nom+","+ape+","+eda+","+sex)
    } yield s"$nom $ape, $eda,$sex"

    assert(resultado.isEmpty)
  }

  test("For comp con invocaciones andidadas para Options"){
    def foo(i: Int): Option[Int] = Some(i + 1)
    def bar(i: Int): Option[Int] = Some(i - 1)
    def sum(i: Int, j: Int) = i+j
    val res = for {
      a <- foo(1)
      b <- bar(a)
    } yield sum(a, b)
    assert(res.getOrElse(-1) == 3)
  }

  test("Prueba de ejecución cuando el for comp da None"){
    def a = Some(1)
    def b = None
    def c = {
      println("Soy C y me estoy ejecutando")
      Some(3)
    }
    val res = for {
      x <- a
      y <- b
      z <- c
    } yield x+y+z
    assert(res.isEmpty)
  }
}

