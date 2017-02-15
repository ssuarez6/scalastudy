package co.com.scalatraining.monads

import org.scalatest._

import scala.util.{Failure, Success, Try}

class TrySuite extends FunSuite with Matchers {

  def estallar(): Int ={
    2/0
  }

  def computar(): Int ={
    1
  }

  def f: Try[Int] = Try{estallar}
  def s: Try[Int] = Try{computar}

  test("Tratando el Try como un bloque try-catch"){
    val res: Try[Int] = Try{
      1
      2
      3
      2/0
      4
    }

    assert(res.isFailure)
  }
  test("Se debe poder hacer pattern match sobre un Try que es Failure"){
    f match {
      case Success(valor) => assert(false)
      case Failure(e) => assert(true)
    }
  }

  test("Se debe poder hacer pattern match sobre un Try que es Success"){
    s match {
      case Success(valor) => assert(true)
      case Failure(e) => assert(false)
    }
  }

  test("Un Failure se debe poder map"){
    val res = f.map(x=>"HOLA")
    assert(res.isFailure)
  }
  
  test("Ensayando map sobre un Try"){
    case class ExcepcionSinSentido(mensaje: String) extends Exception
    val t = Try(10>2)
    val res = t.map(valor =>
        if(valor) "10 es mayor que 2" 
        else new ExcepcionSinSentido("No es posible que 2 sea mayor que 10")
    )
    assert(res.isSuccess)
  }

  test("Ensayando flatmap con un Try"){
    val t = Try(24.5)
    val res = t.flatMap(valor => Success(valor))
    assert(res.isSuccess)
  }

  test("Un Success se debe poder map [assert con for-comp]"){
    val res = s.map(x=>"HOLA")
    assert(res.isSuccess)

    val x: Try[Assertion] = for{
      ss <- res
    }yield{
      assert(ss == "HOLA")
    }

  }

  test("Un Success se debe poder map [assert con flatmap]"){
    val res = s.map(x=>"HOLA")

    res.flatMap(x=> Success(assert(x=="HOLA")))

  }

  test("Un Failure se debe poder recuperar con recover"){

    val res = f.map(x=>"HOLA")
                .recover{case e: Exception => {
                "HOLA ME HE RECUPERADO"
              }}

    res.flatMap(s => Try(assert(s == "HOLA ME HE RECUPERADO")) )

  }

  test("Un Failure se debe poder recuperar con recoverWith"){

    val res = f.map(x=>"HOLA")
      .recoverWith{case e: Exception => {
      s
    }}

    res.flatMap(x => Try(assert(x == 1)) )

  }

  test("Un Try se debe poder convertior en Option"){
    val res = s.toOption

    for{
      v <- res
    } yield assert(v == 1)

    assert(res == Some(1))
  }

  test("Un Failure se debe poder convertior en Option"){
    val res = f.toOption
    assert(res == None)
  }

  test("Un Try se debe poder invertir"){
    case class MiExcepcion(mensaje: String) extends Exception
    val t: Try[Double] = Success(10.754)
    val t2: Try[String] = Failure(new MiExcepcion("Mensaje personalizado"))
    assert(t.failed.isFailure)
    assert(t2.failed.isSuccess)
  }

  test("Un Try se debe poder filtrar"){
    val t: Try[Double] = Success(25.0)
    val x: Try[Double] = t.filter(value => value>35.0)
    assert(x.isFailure)
  }

  test("Un Try se debe poder aplanar"){
    val t: Try[Try[Int]] = Try(Success(10))
    assert(t.flatten.isSuccess)
  }

  test("Se debe poder aplicar foreach a un Try en caso de Success"){
    case class NumeroNegativoException(msg: String) extends Exception
    case class ExcepcionCualquiera(msg: String) extends Exception
    def hacerAlgo(x: Int = -1): Unit = {
      if(x<0) throw new NumeroNegativoException("El numero es negativo")
      else x*x
    }
    val t: Try[Int] = Success(-2)
    val t2: Try[Int] = Failure(new ExcepcionCualquiera("blah blah blah"))
    val t3: Try[Int] = Success(25)
    assertThrows[NumeroNegativoException](t.foreach(hacerAlgo))
    assert(t2.foreach(hacerAlgo).getClass.getSimpleName == 
      t3.foreach(hacerAlgo).getClass.getSimpleName)
  }

  /*
  Este test demuestra como Try encadena con for-comp
  y como es evidente que es Success biased lo que significa que solo continua
  con el encadenamiento si el resultado de cada paso es Success
   */
  test("Try for-com. A chain of Success is a Success"){

    val res = for{
      x <- s
      y <- s
      z <- s
    } yield x + y + z


    assert(res == Success(3))

    res.flatMap(x => {
      assert(x == 3)
      Success(":)")
    })

  }

  /*
  Este test demuestra como Try encadena con for-comp
  y como ante una falla en la cadena, el computo resultante es una falla
   */
  test("Try for-com. A chain of Tries with a Failure is a Failure"){

    val res = for{
      x <- s
      y <- f
      z <- s
    } yield x + y + z

    assert(res.isFailure)

  }

  test("Se debe poder asignar un valor por defecto si el try es failure"){
    val t: Try[Int] = Failure(new Exception("Un mensaje cualquiera"))
    val res: Try[Int] = t.orElse(Try(25))
    assert(res.isSuccess)
  }

  test("Se debe poder transformar un Try así tenga cualquier valor"){
    def enCasodeExito(value: Int): Try[Int] = Success(value*2 - 4)
    def enCasodeFalla(ex: Throwable): Try[Int] = Success(16)
    case class ExPersonalizada(msj: String) extends Exception
    
    val t1 = Try(21)
    
    val t2 = Try{
      if(45>44) throw new ExPersonalizada("45 es mayor que 44 D:")
      else 29
    }
    val t3 = Try("hola")
    val res1 = t1.transform(enCasodeExito, enCasodeFalla)
    val res2 = t2.transform(enCasodeExito, enCasodeFalla)
    assertDoesNotCompile("val res3 = t3.transform(enCasodeExito,enCasodeFalla)")
    assert(res2.isSuccess)
    assert(res1.isSuccess)
    assert(res2.get == 16)
    assert(res1.get == 38)
  }
  
  test("WithFilter debe permitir hacer lazy evaluation"){
    val t = Try(1)
    val res = t.withFilter(x => x>5) //se hace un withFilter cuando la fn sea costosa para evaluar
    assert(res.getClass.getSimpleName == "WithFilter")
    val y = res.map(x => {x})
    assert(y.isFailure)
  }

  test("recuperar un Try puede devolver un failure?"){
    case class ExcepcionDorian(msj: String) extends Exception
    val t = f.map(z => "lo que sea")
      .recover{case e:Exception =>{
        throw new ExcepcionDorian("mensaje de error")
      }}
    assert(t.isFailure)
  }

  test("Try no se puede convertir a seq, Option sí"){
    val l = List(1, 2, 3, 0)
    val newl = l.map(x => Try(6/x).recover{case e: Exception => 0})
    assert(newl.flatMap(x=>x.toOption) == List(6, 3, 2, 0))
    assertDoesNotCompile("newl.flatMap(x => x)")
  }

  /*
  Monad laws
  1. Associativity
  monad.flatMap(f).flatMap(g) == monad.flatMap(v => f(v).flatMap(g))

  2. Left unit
  unit(x).flatMap(f) == f(x)

  3. Right unit
  monad.flatMap(unit) == monad

   */
  test("Try monad verification. Is Try associative?"){

    def fi(i: Int): Try[Int] = Try{i+1}
    def gi(i: Int): Try[Int] = Try{i+2}

    //monad.flatMap(f).flatMap(g) == monad.flatMap(v => f(v).flatMap(g))
    assert(s.flatMap(fi(_)).flatMap(gi(_)) == s.flatMap(v => fi(v).flatMap(gi(_))))

  }

  /*
  La unidad por derecha se cumple pues la excepcion (que es el peor caso)
  siempre está protegida y la verificación evalua a true tanto para Success como para Failure
   */
  test("Try monad verification. Does Try comply right unit?"){

    def fi(i: Int): Try[Int] = Try{i+1}
    def gi(i: Int): Try[Int] = Try{i+2}

    //monad.flatMap(unit) == monad
    assert(s.flatMap(x => Success(1)) == s)

    // La siguiente verificacion hace fallar el test aunque los dos Failure sean por el mismo motivo
    //assert(f.flatMap(x => Try(2/0)) == f)

  }

  /*
  Cuando el unit es un Success **sí**  se cumple la ley de unidad por izquierda
  pues el tanto el lado izquierdo como el derecho de la verificación evalúan al mismo valor
   */
  test("Try monad verification. Does Try comply left unit (1)?"){

    def fi(i: Int): Try[Int] = Try{i+1}
    def gi(i: Int): Try[Int] = Try{i+2}

    def unit = Try(1)

    //unit(x).flatMap(f) == f(x)
    assert(unit.flatMap(fi) == fi(1))

    assert(true)

  }

  /*
  Cuando el unit es un Failure **no** se cumple la ley de unidad por izquierda
  pues el lado izquierdo de la verificación sí puede manejar la excepcion
  mientras que el lado derecho de la verificación estalla con excepcion
  y por ende no hay igualddad.
   */
  ignore("Try monad verification. Does Try comply left unit (2)?"){

    def fi(i: Int): Try[Int] = Try{i+1}
    def gi(i: Int): Try[Int] = Try{i+2}

    //unit(x).flatMap(f) == f(x)

    def unit= Try(2/0)

    val left = unit.flatMap(fi)
    println(s"Ok tengo un left: $left")
    val right = fi(2/0)
    println(s"Ok tengo un right: $right")
    assert(left == right)

  }



}
