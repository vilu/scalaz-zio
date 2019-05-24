package scalaz.zio.internal

import org.scalacheck.Gen
import org.specs2.{ScalaCheck, Specification}

import scala.util.Random

class StackBoolSpec extends Specification with ScalaCheck {
  def is =
    "StackBoolSpec".title ^ s2"""
        Size tracking                 $e0
        From/to list small identity   $e1
        From/to list large identity   $e2
        Small push/pop example        $e3
        Large push/pop example        $e4
        Peek/pop identity             $e5
        From/to list generators1       $e6
        From/to list generators2       $e7
        From/to list generators3       $e8
    """

  def e0 = {
    val list = List.fill(100)(true)

    StackBool(list: _*).size must_== list.length
  }

  def e1 = {
    val list = (1 to 20).map(_ % 2 == 0).toList

    val stack = StackBool(list: _*)
    println(s"List: ${list.map(b => if(b) "1" else "0").mkString}, stack: ${stack.printBits()}")

    stack.toList must_=== list
  }

  def e2 = {
    val list = (1 to 400).map(_ % 2 == 0).toList

    StackBool(list: _*).toList must_=== list
  }

  def e3 = {
    val stack = StackBool()

    stack.push(true)
    stack.push(true)
    stack.push(false)
//    stack.push(false)
//    stack.push(true)

    val v1 = stack.popOrElse(false)
    val v2 = stack.popOrElse(true)
    val v3 = stack.popOrElse(true)
//    val v4 = stack.popOrElse(false)
//    val v5 = stack.popOrElse(false)

    (v1 must_=== false) and
      (v2 must_=== true) and
      (v3 must_=== true)
//      (v3 must_=== true) and
//      (v4 must_=== true) and
//      (v5 must_=== true)
  }

  def e4 = {
    val stack = StackBool()

    val list = (1 to 400).map(_ % 2 == 0).toList

    list.foreach(stack.push(_))

    list.reverse.foldLeft(true must_=== true) {
      case (result, flag) =>
        result and (stack.popOrElse(!flag) must_=== flag)
    }
  }

  def e5 = {
    val stack = StackBool()

    val list = (1 to 400).map(_ % 2 == 0).toList

    list.foreach(stack.push(_))

    list.reverse.foldLeft(true must_=== true) {
      case (result, flag) =>
        val peeked = stack.peekOrElse(!flag)
        val popped = stack.popOrElse(!flag)

        result and (peeked must_=== popped)
    }
  }

  val boolGen = Gen.oneOf(List(true, false))



//  def e6 = prop { list: List[Boolean] =>
//
//
//    val stack = StackBool(list: _*)
//
//    stack.toList must_=== list
//  }.setGen(Gen.listOfN(65, boolGen))

  def e6 = {
    println("---------------6")

    val r = new Random(43L)


    val list =  List.fill(33)(r.nextBoolean())

    val stack = StackBool(list: _*)

    println(s"List: ${list.map(b => if(b) "1" else "0").mkString}, stack: ${stack.printBits()}")
    val s = stack.toList
    s must_=== list

  }


  def e7 = {
    println("---------------7")

//    val r = new Random(43L)
//
//
//    val list =  List.fill(3)(r.nextBoolean())
val list = List(true, false, true)
    val stack = StackBool(list: _*)

    println(list)
    val s = stack.toList
    println(s"List: ${list}, stack: $s")
    s must_=== list

  }


  def e8 = {
    println("---------------8")
//    val r = new Random(43L)
//
//
//    val list =  List.fill(4)(r.nextBoolean())
    val list = List(
      true,
      false,
      true,
      false
    )

    val stack = StackBool(list: _*)

    println(list)
    val s = stack.toList
    println(s"List: ${list}, stack: $s")
    s must_=== list

  }
}
