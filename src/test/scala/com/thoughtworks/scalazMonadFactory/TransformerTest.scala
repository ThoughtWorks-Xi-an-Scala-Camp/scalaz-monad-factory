package com.thoughtworks.scalazMonadFactory

import org.junit.Test
import scalaz.Monad
import scalaz.Bind
import scalaz.Applicative
import scalaz.effect.MonadCatchIO
import scalaz.effect.IO
import org.junit.Assert

final class TransformerTest {

  @Test
  def testWhile(): Unit = {
    import scalaz.std.option._

    val transformer = new Transformer[Option]
    import transformer._

    def s = Some("123")
    var count = 0
    async {
      var i = 0
      while (i < 4) {
        i += 1
        count += s.length
      }
    }

    Assert.assertEquals(Some(12), count)
  }

  @Test
  def testDefDef(): Unit = {
    import scalaz.std.option._

    val transformer = new Transformer[Option]
    import transformer._

    val lengthOption = async {
      def s = Some(Nil)
      s.length
    }

    Assert.assertEquals(Monad[Option].map {
      def s = Some(Nil)
      s
    }(_.length), lengthOption)
  }

  @Test
  def testSomeNilLength(): Unit = {
    import scalaz.std.option._

    val transformer = new Transformer[Option]
    import transformer._
    val s = Some(Nil)

    val lengthOption = async {
      s.length
    }

    Assert.assertEquals(Monad[Option].map(s)(_.length), lengthOption)

  }

  @Test
  def testNoneLength(): Unit = {
    import scalaz.std.option._

    val transformer = new Transformer[Option]
    import transformer._
    val s: Option[Seq[_]] = None

    val lengthOption = async {
      s.length
    }

    Assert.assertEquals(Monad[Option].map(s)(_.length), lengthOption)

  }

  @Test
  def testNewByOption(): Unit = {
    import scalaz.std.option._
    val transformer = new Transformer[Option]
    import transformer._
    val newS = async {
      new String("a string")
    }

    Assert.assertEquals(Monad[Option].pure(new String("a string")), newS)
    Assert.assertEquals(Some(new String("a string")), newS)
  }

  @Test
  def testNewByList(): Unit = {
    import scalaz.std.list._
    val transformer = new Transformer[List]
    import transformer._
    val newS = async {
      new String("a string")
    }

    Assert.assertEquals(Monad[List].pure(new String("a string")), newS)
    Assert.assertEquals(List(new String("a string")), newS)
  }

  @Test
  def testConcatList = {
    import scalaz.std.list._
    val transformer = new Transformer[List]
    import transformer._

    val list1 = List("foo", "bar", "baz")
    val list2 = List("Hello", "World!")
    val concatList = async {
      list1.substring(0, 2) + " " + list2.substring(1, 4)
    }

    Assert.assertEquals(List("fo ell", "ba ell", "ba ell", "fo orl", "ba orl", "ba orl"), concatList)
  }

  /* Legacy tests for debugging

  @Test
  def test(): Unit = {

    import scalaz.std.option._

    val transformer = Transformer[Option]
    import transformer._
    val s = Some(Nil)

    async {
      {
        val a = s.length
        val b = a + 1
        a + b + Some(b)
      }
      val b = 2
      1 + b
    }
  }

  def test2(): Unit = {

    import scalaz.std.option._

    val transformer = Transformer[Option]
    import transformer._
    val s = Some(Nil)
    val some2 = async {
      if (s.length != 0) {
        val nothing = s(2)
        val nothing2 = (s(2): Int) + (nothing: Int)
        s.length + s.length + (None: Option[Nil.type]).length
      }
      val nothing = s(2)
    }

  }

  def testCatch3(): Unit = {
    implicit def aaaa: MonadCatchIO[Option] = ???

    val transformer = Transformer[Option]
    val s = Some(Nil)

    Bind[Option].bind(
      Bind[Option].ifM(Bind[Option].map(s)(
        ((parameter$macro$12: scala.collection.immutable.Nil.type) => parameter$macro$12.length.$bang$eq(0))),
        MonadCatchIO.ensuring(
          Bind[Option].map(s)(((parameter$macro$13: scala.collection.immutable.Nil.type) =>
            parameter$macro$13.apply(2))),
          Bind[Option].map(s)(((parameter$macro$14: scala.collection.immutable.Nil.type) =>
            parameter$macro$14.apply(3)))),
        Applicative[Option].point(())))(((parameter$macro$15: Unit) => {
        parameter$macro$15;
        Bind[Option].bind(s)(((parameter$macro$16: scala.collection.immutable.Nil.type) => {
          val nothing: Nothing = parameter$macro$16.apply(2); Applicative[Option].point(())
        }))
      }))
  }

  */

  /*
@Test
def testCatch(): Unit = {
  var count = 0
  val io = MonadCatchIO.ensuring(MonadCatchIO.catchSome[IO, Int, IO[Int]]({
    count += 1
    MonadCatchIO[IO].point(???)
  })({(e: Throwable) => e match {
    case (e@(_: Error)) => Some({
      count += 1
      MonadCatchIO[IO].point(100)
    })
    case _ => None
  }}, identity), MonadCatchIO[IO].point(count += 1))
  Assert.assertEquals(100, io.unsafePerformIO())
  Assert.assertEquals(3, count)
}
*/

  @Test
  def testBlock(): Unit = {
    val transformer = new Transformer[IO]
    import transformer._
    var count = 0
    val io = async {
      count += 1
      count += 1
      count
    }
    Assert.assertEquals(0, count)
    Assert.assertEquals(2, io.unsafePerformIO())
    Assert.assertEquals(2, count)

  }

  @Test
  def testCatch(): Unit = {
    val transformer = new Transformer[IO]
    import transformer._
    var count = 0
    val io = async {
      try {
        count += 1
        (null: Array[Int])(0)
      } catch {
        case e: NullPointerException => {
          count += 1
          100
        }
      } finally {
        count += 1
      }
    }
    Assert.assertEquals(0, count)
    Assert.assertEquals(100, io.unsafePerformIO())
    Assert.assertEquals(3, count)
  }

  @Test
  def testThrowCatch(): Unit = {
    val transformer = new Transformer[IO]
    import transformer._
    var count = 0
    val io = async {
      try {
        count += 1
        throw new Exception
      } catch {
        case e: Exception => {
          count += 1
          100
        }
      } finally {
        count += 1
      }
    }
    Assert.assertEquals(0, count)
    Assert.assertEquals(100, io.unsafePerformIO())
    Assert.assertEquals(3, count)
  }


  /* Disable since it is not implemented yet
  def testWhile(): Unit = {

    val transformer = Transformer[Option]
    import transformer._
    val s = Some(0.5)
    val some2 = async {
      while(math.random < s) {
        println("Hello, World!")
      }
    }
  }
  */

}

