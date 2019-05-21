/*
 * Copyright 2017-2019 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scalaz.zio.internal

/**
  * A very fast, hand-optimized stack designed just for booleans.
  * In the common case (size < 64), achieves zero allocations.
  */
private[zio] final class StackBool private () {
  import StackBool.Entry

  private[this] var head  = new Entry(null)
  private[this] var _size = 0L

  val MAX_SIZE = 4L

  val MAX = MAX_SIZE - 1L

  final def getOrElse(index: Int, b: Boolean): Boolean = {
    var j   = index.toLong
    var cur = head

    val entry = ((_size - 1) - index) / MAX_SIZE
    val totalEntries = _size  / MAX_SIZE
    var entriesToMove = totalEntries - entry
    println(s"Initial: j=$j, entriesToMove=$entriesToMove")

    while (entriesToMove > 0 && (cur.next ne null)) {
      cur = cur.next
      j = MAX_SIZE - j
      entriesToMove = entriesToMove - 1
      println(s"Updating: j=$j, entriesToMove=$entriesToMove")
    }

    assert(j < MAX_SIZE && j >= 0)
    if (cur eq null) b
    else {
      val mask = 1L << j
      println(s"Size: ${_size}")
      println(s"Mask: ${mask.toBinaryString}")
      println(s"Bits: ${cur.bits.toBinaryString}")
      (cur.bits & mask) != 0L
    }
  }

  final def getOrElse2(index: Int, b: Boolean): Boolean = {
    var j   = index.toLong
    var cur = head
//    val firstHead = _size % MAX_SIZE
//    var moreThan1 = false
////    if (j >= firstHead) {
////      cur = cur.next
////      j = j - firstHead
////      moreThan1 = true
////    }

    while (j >= 4L && (cur.next ne null)) {
      j -= 4L
      cur = cur.next
    }
    assert(j < 4L && j >= 0)
    if (cur eq null) b
    else {
      val mask = 1L << j

      (cur.bits & mask) != 0L
    }
  }

  final def size = _size

  final def push(flag: Boolean): Unit = {
    val index = _size & MAX // Long & Long

    if (flag) head.bits = head.bits | (1L << index)
    else head.bits = head.bits & (~(1L << index))

    if (index == MAX) head =
      new Entry(head)

    _size += 1L
  }

//  def padWith0s(str:String):String = {
//    if (str.length < MAX_SIZE) padWith0s("x" + str) else str
//  }
//
//  def printBits(): Unit = {
//    var str = padWith0s(head.bits.toBinaryString)
//    println(s"Entry: ${head.bits}")
//    var pointer = head.next
//    while (pointer ne null) {
//      println(s"Entry: ${pointer.bits}")
//      str = s"$str${padWith0s(pointer.bits.toBinaryString)}"
//      pointer = pointer.next
//    }
//    println(str)
//    println(str.length)
//  }

  final def popOrElse(b: Boolean): Boolean =
    if (_size == 0L) b
    else {
      _size -= 1L
      val index = _size & MAX

      if (index == 0L && head.next != null) head = head.next

      ((1L << index) & head.bits) != 0L
    }

  final def peekOrElse(b: Boolean): Boolean =
    if (_size == 0L) b
    else {
      val size  = _size - 1L
      val index = size & MAX
      val entry =
        if (index == 0L && head.next != null) head.next else head

      ((1L << index) & entry.bits) != 0L
    }

  final def popDrop[A](a: A): A = { popOrElse(false); a }

  final def toList: List[Boolean] =
    (0 until _size.toInt).map(getOrElse2(_, false)).toList.reverse

  final override def toString: String =
    "StackBool(" + toList.mkString(", ") + ")"

  final override def equals(that: Any) = that match {
    case that: StackBool => toList == that.toList
  }

  final override def hashCode = toList.hashCode
}
private[zio] object StackBool {
  def apply(): StackBool = new StackBool

  def apply(bools: Boolean*): StackBool = {
    val stack = StackBool()

    bools.reverse.foreach(stack.push(_))

    stack
  }

  private class Entry(val next: Entry) {
    var bits: Long = 0L
  }
}

//object Test extends App{
//
//  val list = List(
//    true,
//    false,
//    true,
//    true,
//    false,
//    false,
//    true,
//    true,
//    true,
//  )
//
//  val stack = StackBool(list: _*)
//  println(list)
//  println(s"List size: ${list.size}")
////  println(stack.printBits())
//
//  (0 until list.size).foreach(i => println(s"$i: ${stack.getOrElse(i, false)}"))
//
//}
