package co.com.scalatraining.monads

import java.util.Random
import java.util.concurrent.Executors

import org.scalatest.FunSuite
import scala.language.postfixOps
import scala.util.{Failure, Success}
import scala.concurrent.{ExecutionContext, Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class FutureSuite extends FunSuite {

  test("Un futuro se puede crear") {

    val hiloPpal = Thread.currentThread().getName

    var hiloFuture = ""
    val saludo = Future {
      hiloFuture = Thread.currentThread().getName
      Thread.sleep(500)
      "Hola"
    }
    val resultado = Await.result(saludo, Duration.Inf) //solamente en codigo de testing
    assert(resultado == "Hola")
    assert(hiloPpal != hiloFuture)
  }

  test("map en Future") {

    val saludo = Future {

      Thread.sleep(500)
      "Hola"
    }
    val saludoCompleto: Future[String] = saludo.map(mensaje => {

      mensaje + " muchachos"
    })
    val resultado = Await.result(saludoCompleto, 10 seconds)
    assert(resultado == "Hola muchachos")
  }

  test("Se debe poder encadenar Future con for-comp") {
    val f1 = Future {
      Thread.sleep(200)
      1
    }

    val f2 = Future {
      Thread.sleep(200)
      2
    }

    val f3 = for {
      res1 <- f1
      res2 <- f2
    } yield res1 + res2

    val res = Await.result(f3, 10 seconds)

    assert(res == 3)

  }

  test("Se debe poder manejar el error de un Future de forma imperativa") {
    val divisionCero = Future {
      Thread.sleep(100)
      10 / 0
    }
    var error = false

    val r = divisionCero.onFailure {
      case e: Exception => error = true
    }

    Thread.sleep(1000)

    assert(error == true)
  }

  test("Se debe poder manejar el exito de un Future de forma imperativa") {

    val division = Future {
      5
    }

    var r = 0

    val f = division.onComplete {
      case Success(res) => r = res
      case Failure(e) => r = 1
    }

    Thread.sleep(150)

    val res = Await.result(division, 10 seconds)

    assert(r == 5)
  }

  test("Se debe poder manejar el error de un Future de forma funcional sincronicamente") {

    var threadName1 = ""
    var threadName2 = ""

    val divisionPorCero = Future {
      threadName1 = Thread.currentThread().getName
      Thread.sleep(100)
      10 / 0
    }.recover {
      case e: ArithmeticException => {
        threadName2 = Thread.currentThread().getName
        "No es posible dividir por cero"
      }
    }

    val res = Await.result(divisionPorCero, 10 seconds)

    assert(threadName1 == threadName2)
    assert(res == "No es posible dividir por cero")

  }
   
  test("Se debe poder manejar el error de un Future de forma funcional asincronamente") {

    var threadName1 = ""
    var threadName2 = ""

    implicit val ecParaPrimerHilo = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

    val f1 = Future {
      threadName1 = Thread.currentThread().getName
      2/0
    }(ecParaPrimerHilo)
    .recoverWith {
      case e: ArithmeticException => {

        implicit val ecParaRecuperacion = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

        Future{
          threadName2 = Thread.currentThread().getName
          1
        }(ecParaRecuperacion)
      }
    }

    val res = Await.result(f1, 10 seconds)

    println(threadName1)
    println(threadName2)

    assert(threadName1 != threadName2)
  }

  test("Los future **iniciados** fuera de un for-comp deben iniciar al mismo tiempo") {

    val timeForf1 = 100
    val timeForf2 = 100
    val timeForf3 = 100

    val additionalTime = 50D

    val estimatedElapsed = (Math.max(Math.max(timeForf1, timeForf2), timeForf3) + additionalTime)/1000

    val f1 = Future {
      Thread.sleep(timeForf1)
      1
    }
    val f2 = Future {
      Thread.sleep(timeForf2)
      2
    }
    val f3 = Future {
      Thread.sleep(timeForf3)
      3
    }

    val t1 = System.nanoTime()

    val resultado = for {
      a <- f1
      b <- f2
      c <- f3
    } yield (a+b+c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    assert(elapsed <= estimatedElapsed)
    assert(res == 6)

  }

  test("Los future **definidos** fuera de un for-comp deben iniciar secuencialmente") {

    val timeForf1 = 100
    val timeForf2 = 100
    val timeForf3 = 100

    val estimatedElapsed = (timeForf1 + timeForf2 + timeForf3)/1000

    def f1 = Future {
      Thread.sleep(timeForf1)
      1
    }
    def f2 = Future {
      Thread.sleep(timeForf2)
      2
    }
    def f3 = Future {
      Thread.sleep(timeForf3)
      3
    }

    val t1 = System.nanoTime()

    val resultado = for {
      a <- f1
      b <- f2
      c <- f3
    } yield (a+b+c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    assert(elapsed >= estimatedElapsed)
    assert(res == 6)

  }

  test("Los future declarados dentro de un for-comp deben iniciar secuencialmente") {

    val t1 = System.nanoTime()

    val timeForf1 = 100
    val timeForf2 = 100
    val timeForf3 = 100

    val estimatedElapsed = (timeForf1 + timeForf2 + timeForf3)/1000

    val resultado = for {
      a <- Future {
        Thread.sleep(timeForf1)
        1
      }
      b <- Future {
        Thread.sleep(timeForf2)
        2
      }
      c <- Future {
        Thread.sleep(timeForf3)
        3
      }
    } yield (a+b+c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    assert(elapsed >= estimatedElapsed)
    assert(res == 6)
  }

  test("Un Future puede tener varios callbacks, que pueden o no ejecurase sincrónicamente"){
    @volatile var totalA = 0 //variable compartida
    val text = Future{
      "na" * 16 + " BATMAN!!!"
    }

    text onSuccess {
      case txt => totalA += txt.count(_ == 'a')
    }

    text onSuccess {
      case txt => totalA += txt.count(_ == 'A')
    }
    Thread.sleep(1500)
    assert(totalA > 1)
  }

  test("Se pueden tener Futures como estrategia de recuperación"){
    val f1 = Future{throw new Exception("Este future falló")}
    val f2 = Future{"Este future sí funciono"}
    val f3 = f1 fallbackTo f2 
    //assert(f3.get)
  }

  test("Dos Future se pueden comprimir en una tupla"){
    def f1 = Future((1 to 1000).toList.sum)
    def f2 = Future("na"*16 + " BATMAN!")
    def f3 = f1 zip f2
    for{
      t <- f3 //la compresion resulta como una 2Tuple
    } assert(t._1 == 500500)
  }

  test("Dos Future se pueden comprimir usando una función especifica"){
    def f1 = Future((1 to 1000).sum)
    def f2 = Future("Un String")
    def delUnoAlOtro(a: Int, b: String): Double = a.toDouble+b.length.toDouble
    val f3 = f1.zipWith(f2)(delUnoAlOtro)
    for{
      res <- f3
    } assert(res == 500509.0)
  }
}
