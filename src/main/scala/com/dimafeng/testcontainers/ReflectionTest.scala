package com.dimafeng.testcontainers

trait Foo

trait Foo2 extends Foo

case object Foo3 extends Foo2

case class Foo4(a: Int) extends Foo

class ReflectionTest {

//  val a: String = "a"
//  lazy val b: Int = 5
//  lazy val c: Double = 15.55
//  var d: Option[Nothing] = None

  val aa = new Foo {}
  var bb = new Foo2 {}
  lazy val cc = Foo3
  val dd = Foo3
  lazy val ee = Foo4(5)


  def doStuff(): Unit = {
    import scala.reflect.runtime.universe._

    val mirror = scala.reflect.runtime.currentMirror
    val thisMirror = mirror.reflect(this)
    val terms = thisMirror.symbol.toType.members.sorted.collect {
      case x if x.isTerm => x.asTerm
    }
    val fields = terms.filter {
      case term if term.typeSignature.resultType <:< typeOf[Foo] =>
        if (term.isLazy && term.isGetter) {
          // lazy vals should be initialized
          val lazyInitializer = thisMirror.reflectMethod(term.asMethod)
          lazyInitializer()
          true
        } else {
          term.isVal || term.isVar
        }
      case _ =>
      false
    }

    print(fields)

    def print(fields: Seq[TermSymbol]): Unit = {
      val fieldMirrors = fields.map { field =>
        thisMirror.reflectField(field)
      }
      fieldMirrors.foreach { x =>
        println(s"$x: ${x.get}")
      }
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val instance = new ReflectionTest

    instance.doStuff()
  }
}
