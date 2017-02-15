package co.com.scalatraining.collections

import org.scalatest.FunSuite
class VectorSuite extends FunSuite{
  test("Los vectores son indexados"){
    val v = Vector(1,2,3)
    assert(v(0) == 1 && v(0) == v.apply(0))
  }

  test("Pueden modificarse los elementos de un vector"){
    val v = Vector("hola", "mundo", "como", "estas")
    val v2 = v.updated(0, "Hola,").updated(1, "mundo.").updated(2, "¿Cómo").updated(3, "estás?")
    assert(v2.mkString(" ") == "Hola, mundo. ¿Cómo estás?")
  }

  test("Pueden hacerse operaciones sobre indices"){
    val v = Vector("palabralarga", "palabracorta", "palabraunpocomaslarga", "palabrita", "lapalabramaslargadetodas")
    val i = v.indexWhere(x => x.endsWith("larga"))
    val i2 = v.indexWhere(x => x.endsWith("todas"))
    val i3 = v.indexWhere(x => x.startsWith("palabri"))
    assert(i == 0 && i2 == 4 && i3 == 3)
  }

  test("Se debe saber si un vector llega hasta cierta posicion en algun momento"){
    val v = Vector(0,1,2,3,4)
    assert(! v.isDefinedAt(8))
  }

  test("Un vector puede generarse como tuplas con sus respectivos índices"){
    val v = Vector(10.1, 10.5, 10.7, 10.9)
    val v2 = v.zipWithIndex
    assert(v2(0)._2 == 0 && v2(3)._1 == 10.9)
  }

  test("Se deben poder hallar ocurrencias en un Vector por medio de sus índices"){
    val v = Vector(1,2,3,4,2,3)
    val i = v.indexOf(3,3) //encuentre el elemento 3 desde el índice 3
    assert(i == v.length-1)
  }

  test("Mapeando en Vector"){
    val v = Vector(1,2,3,4,5,6)
    def transform(x:Int): Int = {
      (x * 2) + (55 / x)
    }
    val v2 = v.map(transform)
    assert(v2(0) == 57)
  }

  test("Fold en vector"){
    val v = Vector(1,2,3,4)
    val len = v.fold(0)((acc, _) => acc+1)
    val sum = v.fold(0)((acc, item) => acc+item)
    assert(sum/len == 10/4)
  }
}
