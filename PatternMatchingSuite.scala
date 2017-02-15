package co.com.scalatraining.patternmatching

import org.scalatest.FunSuite

class PatternMatchingSuite extends FunSuite{
  test("Pattern matching para factorial"){
    def factorial(n: Int): Int = {
      n match {
        case 0 => 1
        case n => n * factorial(n-1)
      }

    }
    assert(factorial(5) == 120)
  }

  test("Pattern matching implementando tail"){
    def tail2(l: List[Int]) = {
      l match {
        case _ :: xs => xs
        case Nil => Nil
      }
    }
    val l = List(1,2,3)
    assert(tail2(l) == l.tail)
  }

  test("Implementando reverse"){
    def reverse2(l:List[Int]): List[Int] ={
      l match {
        case Nil => Nil
        case x :: xs => reverse2(xs) :+ x
      }
    }
    val l = List(1,2,3,4,5)
    assert(reverse2(l) == l.reverse)
  }

  test("Implementando length"){
    def len(l: List[Any]): Int = {
      l match {
        case Nil => 0
        case _ :: xs => 1+len(xs)
      }
    }
    val l = List(1,2,3,4,5)
    assert(len(l) == l.length)
  }

  test("Implementando fibonacci"){
    def fib(n: Int):Int = {
      n match{
        case 0|1 => n
        case x => fib(x-1) + fib(x-2)
      }
    }
    assert(fib(5)==5 && fib(6)==8)
  }

  test("Pattern Matching case classes"){
    case class CC(a: String, b: Int)
    val x = CC("Santiago", 23)
    val res = x match{
      case CC("Andres", _) => "No es a quien busco"
      case CC("Santiago", _) => "Relevancia del orden de cases" //primero entra aqui
      case CC("Santiago", 23) => "Correct!"
      case CC("Santiago", 20) => "THE PERFECT MAN!"
      case _ => "Más perdido que bolsillo en la espalda"
    }
    assert(res == "Relevancia del orden de cases")
  }

  test("Strings como listas para pattern matching"){
    def empiezaCon(s: String, pref: String): Boolean ={
      val l = pref.toList //machete
      s.toList match {
        case l :: _ => true
        case _ => false
      }
    }
    assert(empiezaCon("Un String", "Un"))
  }

  test("Map recursivo"){
    def _map(l: List[Int], fn:(Int) => Int): List[Int] = {
      l match{
        case x :: xs => List(fn(x)) ++ _map(xs, fn)
        case _ => Nil
      }
    }
    val l = List[Int](1,2,3)
    val l2 = _map(l, x => x*2)
    assert(l2 == List(2,4,6))
  }

  test("Ultimo elemento de una lista"){
    def ultimo(l: List[Int]): Option[Int] = {
      l match {
        case Nil => None
        case x :: Nil => Some(x)
        case x :: xs => ultimo(xs)
      }
    }
    val l = List(1,2,3,4,5)
    assert(ultimo(l).get == l.last)
  }

  test("Case classes anidadas"){
    case class Profesor(nom: String, especialidad: String, edad: Int)
    case class Estudiante(nom: String, edad: Int)
    case class Curso(tema: String, estudiantes: List[Estudiante] , p: Profesor)
    val profe = Profesor("Juan Pablo", "Scala", 12)
    val otroProfe = Profesor("DSFGS", "Java", 12)
    val ests = List(Estudiante("abc", 20), Estudiante("def", 21), Estudiante("ghi", 22))
    val ests2 = List(Estudiante("abc", 20), Estudiante("xyz", 21))
    val c = Curso("Scala", ests, profe)
    val c2 = Curso("Scala", Nil, profe)
    val res = c match {
      case Curso(y, _, Profesor(_, z, _)) if y==z => "El profesor esta especializado en el tema del curso"
      case Curso(y, _, Profesor(_, z, _)) if y!=z => "El profesor no esta especializado en el tema del curso"
      case Curso("Scala", Nil, _) => "Tiene una lista vacía"
      case Curso("Scala", lista, _) if lista == ests => "Tiene la primera lista de estudiantes"
      case _ => "No cumple ninguna de las listas de Estudiantes"
    }
    assert(res == "El profesor esta especializado en el tema del curso")
  }

  test("Implementacion de traits en caliente para patrones"){
    trait A{
      val lugar = "S4N"
    }
    trait B{
      val sitio = "Salon"
    }
    case class Curso (tema:String, profesor: String)
    object c1 extends Curso("Scala", "JP") with A
    object c2 extends Curso("Scala", "Zulu") with B
    def test(x: Curso): String = {
      x match{
        case z:Curso with A => z.lugar
        case z:Curso with B => z.sitio
      }
    }
    assert(test(c1) == "S4N" && test(c2) == "Salon")
  }
}