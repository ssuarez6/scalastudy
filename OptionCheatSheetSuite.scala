package co.com.scalatraining.monads

import org.scalatest.FunSuite
import scalaz._
import Scalaz._
import scala.collection.immutable.IndexedSeq

class OptionCheatSheetSuite extends FunSuite{
  test("Usando flatMap"){
    def hacerPar(i: Int): Option[Int] = if(i%2!=0) Some(i*2) else None
    val x: Option[Int] = Some(3)
    val res = x match {
      case None => None
      case Some(x) => hacerPar(x)
    }
    assert(x.flatMap(hacerPar) == res)
  }
  
  test("Quitando el efecto de proteccion sobre la existencia"){
    val x = Option(Option(2))
    val res = x match {
      case None =>
      case Some(x) => x
    }
    assert(x.flatten == res)
  }

  test("Usando map para reemplazar el pattern matching"){
    def bar(x: Int) = s"Soy la variable X y mi valor es $x"
    val x = Option(5)
    val res = x match {
      case None => None
      case Some(x) => Some(bar(x))
    }
    assert(x.map(bar) == res)
  }

  test("Usando foreach para Options"){
    def notModifyingFunc(x: Int) = s"Este es mi valor: $x"
    val x = Option(10)
    val res: Unit = x match {
      case None => {}
      case Some(x) => notModifyingFunc(x)
    }
    assertCompiles("x.foreach(notModifyingFunc) ; res")
  }

  test("Verificando si un option tiene un valor o no"){
    val x = Option(None)
    val res = x match {
      case None => false
      case Some(_) => true
    }
    assert(res == x.isDefined)
  }

  test("Verificando si un option está vacio"){
    val x = Option(None)
    val res = x match {
      case None => true
      case Some(_) => false
    }
    assert(res == x.isEmpty)
  }

  test("Usando el método forall para saber si el valor del option sea cual sea cumple un predicado"){
    def esPar(i: Int): Boolean = i%2==0
    def esImpar(i: Int): Boolean = !esPar(i)
    val x = Option(6)
    def y: Option[Int] = {
      if(1>2) Option(1) else None
    }
    val res = x match {
      case None => true
      case Some(x) => esPar(x)
    }
    assert(x.forall(esPar) == res)
    assert(y.forall(esPar) == y.forall(esImpar))
  }

  test("Usando exists para saber si solamente el valor y no el none cumple un predicado"){
    def isBig(i: Int) = i > 100
    val o = Option(100)
    val res = o match {
      case None => false
      case Some(x) => isBig(x)
    }
    assert(res == o.exists(isBig))
  }

  test("Usando orElse para asignar un valor en caso de que sea None"){
    val foo = Some("YOOOOO SOY NONEEEEEE")
    val o = Option(None)
    val res = o match{
      case None => foo
      case Some(x) => Some(x)
    }
    assertCompiles("o.orElse(foo); println(res)")
    assert(res == o.orElse(foo))
  }

  test("Implementando getOrElse para obtener el valor y asignar uno estándar"){
    val o = Option(5)
    val res: Int = o match {
      case None => 6
      case Some(x) => x
    }
    assert(o.getOrElse(6) == res)
  }

  test("Convirtiendo un Option a Lista"){
    val o = Option(true)
    val res = o match {
      case None => Nil
      case Some(x) => x :: Nil
    }
    assert(o.toList == res)
  }

  test("Probando coflat al importar scalaz"){
    val o = Option(5)
    def foo(i: Option[Int]) = s"Hola, soy 'i' y valgo ${i.getOrElse(-1)}"
    val res = o match {
      case None => None
      case Some(_) => Some(foo(o))
    }
    assert(res == o.coflatMap(foo))
  }

  test("Duplicate como pattern matching y como cojoin en scalaz"){
    val o = Option("HOLA C:")
    val res = o match {
      case None => None
      case Some(_) => Some(o)
    }
    assert(res == o.cojoin)
  }

  test("Haciendo fold sobre Option"){
    val o = Option(6)
    val res: Int = o.fold{
      -1
    }{
      valor => valor
    }
    //println(res)
    val res2 = o.getOrElse(-1)
    assert(res == res2)
  }

  test("Haciendo reduce sobre Option"){
    val o = Some(("Uno", "Dos"))
    val res = o.reduce((s1, s2) => ("valor estandar", "otro valor"))
    //s"S1: $s1 \nS2: $s2"
    //println(res)
    //???
  }

  test("Caso de prueba"){
    /* dadas las condiciones actuales,
     * devolver una lista de String con los niveles de seguridad no vacíos
     */
    case class Lab(code: Int, name: String, secLevel: Option[String])
    val labs = Map(1 -> Lab(1, "S4N", Some("High")), 2 -> Lab(2, "Ceivil", None))
    def getAllSecLvls(ls: Map[Int, Lab]): List[String] = {
      ls.values
        .map(l => l.secLevel)
        .filter(secLevel => secLevel.isDefined)
        .map(os => os.get)
        .toList
    }
    def _get(ls: Map[Int, Lab]): List[String] = {
      ls.values
        .flatMap(l => l.secLevel)
        .toList
    }
    assert(_get(labs) == getAllSecLvls(labs))
  }

  test("Segundo caso de prueba"){
    /* dada una lista de options del 1 al 100
     devolver un option con el promedio de los pares
     */
    val evenOpts: IndexedSeq[Option[Int]] = (1 to 100)
      .map(Option(_))
      .filter(numOpt => numOpt.exists(x => x%2==0))

    val avg: Option[Double] = Some(evenOpts.flatten.sum / evenOpts.length.toDouble)
    println(avg)
  }
}
