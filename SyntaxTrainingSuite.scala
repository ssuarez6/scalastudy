package co.com.scalatraining.syntax

import org.scalatest.FunSuite

class SyntaxTrainingSuite extends FunSuite{
  test("Un objeto sencillo se crea"){
    object testObj{
      var x: Int = 2 //definiendo el tipo
      var y = "50" //infiriendo, siempre tipos fuertes
      val z: Boolean = true //inmutable con tipo definido
      val w = 45.7 //inmutable infiriendo
      def foo(factor: Int) = { //igual debido a que se evalúa una exp
        x = x*factor
        var z = x%2==0
        if (z) y*x else w+2.0 //combinando tipos...
      }
    }
    val res = testObj.foo(3)
    assert(res == "505050505050")
  }

  test("Usar objetos entre sí"){
    object o1 {
      var z = 5
      def power2 = z*z+o2.x
    }

    object o2 {
      var x = 10
      def doSomething(n: Int): Int = o1.power2 + (n+x)
    }

    val res = o2.doSomething(5)
    assert(res == 50)
  }

  test("Usar objetos en clases"){
    object usable {
      var x = 10
      val y = 5
      def mult = x*y
    }

    class Punto(var x: Int, var y: Int){ //hacer mutables los parametros constructores
      def pendiente = {
        x = x + 10
        y = y + 5
        (x-y) * usable.mult
      }
    }
    var p = new Punto(usable.x, usable.y)
    val pend = p.pendiente
    assert(pend == 500)
  }

  test("Objetos que heredan de clases"){
    class Convertidor(factor: Double){
      def convertir(valor: Double) = valor * factor
    }

    object librasAGramos extends Convertidor(500.0){
      var libsIniciales = 3
      def hacerConversion = convertir(libsIniciales)
    }

    val gr = librasAGramos.hacerConversion

    assert(gr == 1500)
  }

  test("Clases y objetos dentro de clases"){
    class Exterior{

      class Interior(x: Int, y: Int){
        def calcular = (x+y)*x
        def fact(num: Int): Int = {
          if(num <= 1){
            1
          }else{
            num * fact(num-1)
          }
        }
      }

      object ob1 extends Interior(2,3) {} //esta es otra forma de instanciar clases

      var c = ob1.calcular
      var f = ob1.fact(5)
    }

    object ob2 extends Exterior {}

    assert(ob2.c == 10)
    assert(ob2.f == 120) //se puede más de un assert? no tendría sentido...
  }

  test("Traits, Clases y objetos dentro de objetos"){
    object principal {
      trait Cantor{
        def cantar: String
      }

      class Animal(nombre: String){
        def comer = println("Estoy comiendo!!!")
      }

      class Pajarito(nombre: String) extends Animal(nombre) with Cantor{
        override def cantar = "Piu piu piu"
      }

      object lorito extends Animal("Lucho") with Cantor{ //INSTANCIA!!
        override def cantar = "QUIERE CACAO?!"

        override def comer: Unit = println("Soy un loro comelon! gulp gulp gulp!!")
      }

      var p = new Pajarito("Manolo")
    }

    assert(principal.p.cantar == "Piu piu piu" && principal.lorito.cantar == "QUIERE CACAO?!")
  }

  test("Objetos y clases dentro de traits"){
    trait comportamiento1 {
      class Arbol(tipo: String, var edad: Int){
        def crecer ={
          edad = edad + 5
          //println(s"Antes tenía ${edad-5}, ahora tengo ${edad} de edad")
        }
        def decirEstado = s"Soy de tipo: ${tipo}\nY estoy creciendo"
      }
      object roble extends Arbol("roble", 1) {}
    }

    object ejecutor extends comportamiento1 {}

    val x = new ejecutor.Arbol("pino", 2)
    x.crecer

    ejecutor.roble.crecer

    assert(ejecutor.roble.edad == 6)
  }

  test("Mezclar traits en instancias"){
    class X
    trait A{
      val a = "a"
      def foo = true
    }
    val x = new X with A
    assert(x.a == "a" && x.foo)
  }

  test("Companion objects"){
    class Persona(n: String){
      private val valor = "VALOR"
    }

    object Persona {
      def hacerAlgo(p: Persona) ={
        p.valor
      }
      def apply(n: String): Persona = {
        new Persona(n)
      }
    }
    val p = new Persona("adsfasd")
    assert(Persona.hacerAlgo(p) == "VALOR")
  }

  test("Implicits"){
    import java.math.BigInteger
    implicit def Int2BigIntegerConvert(value: Int): BigInteger = new BigInteger(value toString)
    def add(a: BigInteger, b: BigInteger) = a.add(b)
    var x: Boolean = true

    x = add(Int2BigIntegerConvert(3), Int2BigIntegerConvert(6)) == Int2BigIntegerConvert(9)
    assert(x)
    x = add(3, 6) == 9
    assert(!x)
    x = add(3, 6) == Int2BigIntegerConvert(9)
    assert(x)
    x = add(3, 6) == (9: BigInteger)
    assert(x)
    x = add(3, 6).intValue() == 9
    assert(x)
  }

  test("Iterables"){
    val list = List(3, 5, 9, 11, 15, 19, 21, 24, 32)
    val it = list sliding 3 //se desliza de a uno y toma tres
    assert(it.next() == List(3,5,9))
    assert(it.next() == List(5, 9, 11))
    assert(it.next == List(9, 11, 15))
  }

  test("Traversables"){
    val set = Set(1,2,3,5)
    val list = List(5,6,7,8)
    val r1 = set ++ list
    assert(r1.size == 7)
    val r2 = list ++ set
    assert(r2.size == 8)

    val l = List(1,2,3,4)
    assert(l.slice(1,3) == List(2,3))

    import Stream.cons
    def streamer(v: Int): Stream[Int] = cons(v, streamer(v+1))
    val a = streamer(2)
    assert(((a drop 6) take 3 toList) == List(8,9,10))

    val b = List("Do", "Re", "Mi", "Fa", "Sol", "La", "Si", "Do")
    val x = b.reduceRight(_+_)
    assert(x == "DoReMiFaSolLaSiDo")

    val MAX_SIZE = 1000000
    val reduceLeftStartTime = new java.util.Date
    (1 to MAX_SIZE) reduceLeft (_ + _)
    val reduceLeftEndTime = new java.util.Date

    val reduceRightStartTime = new java.util.Date
    (1 to MAX_SIZE) reduceRight (_ + _)
    val reduceRightEndTime = new java.util.Date

    val totalReduceLeftTime = reduceLeftEndTime.getTime - reduceLeftStartTime.getTime
    val totalReduceRightTime = reduceRightEndTime.getTime - reduceRightStartTime.getTime

    assert(totalReduceLeftTime < totalReduceRightTime)

    val list2 = List(List(1), List(4))
    assert(list2.transpose == List(List(1,4)))
  }

  test("Repeated parameters"){
    def repeatedParameterMethod(x: Int, y: String, z: Any*) = {
      "%d %ss can give you %s".format(x, y, z.mkString(", "))
    }
    assert(repeatedParameterMethod(3, "egg", List("a delicious sandwich", "protein", "high cholesterol")) ==
      "3 eggs can give you List(a delicious sandwich, protein, high cholesterol)")
  }

  test("Enums"){
    object Planets extends Enumeration {
      val Mercury = Value
      val Venus = Value
      val Earth = Value
      val Mars = Value
      val Jupiter = Value
      val Saturn = Value
      val Uranus = Value
      val Neptune = Value
      val Pluto = Value
    }

    assert(Planets.Mercury.toString == "Mercury")
    assert(Planets.Earth == Planets.Earth)
    assert(Planets.Mercury.id == 0)
  }
}