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

  val MAX_SIZE = 32L

  val MAX = MAX_SIZE - 1L

  final def getOrElseWorking(index: Int, b: Boolean): Boolean = {
    var j = index.toLong
    var cur = head
    println(s"GetOrElse2: j = $j head: ${padWith0s(cur.bits.toBinaryString)} size: ${_size}")
    val firstHead = _size % MAX_SIZE
    var moreThan1 = false
    if (j >= firstHead && _size >= MAX_SIZE) {
      j = j - firstHead
      cur = cur.next
      moreThan1 = true
      println(s"Fupdat: j = $j head: ${padWith0s(cur.bits.toBinaryString)}")
    }

    while (j >= MAX_SIZE && (cur.next ne null)) {
      j -= MAX_SIZE
      cur = cur.next
      println(s"Updating: j = $j head: ${padWith0s(cur.bits.toBinaryString)}")
    }
    assert(j < MAX_SIZE && j >= 0)
    if (cur eq null) {
      println(s"Not found! Returning: $b")
      b
    }
    else {
      val mask = if (moreThan1) 1L << (MAX - j) else 1L << ((firstHead - 1) - j)

      println(s"Found     : j = $j, mask: ${padWith0s(mask.toBinaryString)}, head: ${padWith0s(cur.bits.toBinaryString)}, result: ${(cur.bits & mask) != 0L} (${(cur.bits & mask)})")
      (cur.bits & mask) != 0L
    }
  }

  final def getOrElse2(index: Int, b: Boolean): Boolean = {
    // Fetch starting point from size and block size
//    (1 to 9) // size
//      .toList
//      .reverse
//      .map(x => (x - 1) % 4) // starting point
//      .map(y => 1 << y) // mask (without considering provided index)

    val i0 = (_size - 1) % MAX_SIZE
    var ib = (i0 + MAX) / MAX_SIZE
    var j = index.toLong
    var cur = head
    println(s"getOrElseNew: starting index: $i0, blockIndex: $ib, pointer: $j, cur: ${padWith0s(cur.bits.toBinaryString)}")
    while (ib > 0) {
      ib -= 1
      j -= MAX_SIZE
      cur = cur.next
      println(s"Updating: blockIndex: $ib, pointer: $j, cur: ${padWith0s(cur.bits.toBinaryString)}")
    }

    if (cur eq null) {
      println(s"Not found! Returning: $b")
      b
    }
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
//    println(s"Push:       ${padWith0s(head.bits.toBinaryString)}, size: ${_size + 1}")
    if (index == MAX) head =
      new Entry(head)

    _size += 1L
  }

  def padWith0s(str:String):String = {
    if (str.length < MAX_SIZE) padWith0s("0" + str) else str
  }

  def printBits(): String = {
    var str = padWith0s(head.bits.toBinaryString)
    println(s"Entry: ${head.bits}")
    var pointer = head.next
    while (pointer ne null) {
      println(s"Entry: ${pointer.bits}")
      str = s"$str${padWith0s(pointer.bits.toBinaryString)}"
      pointer = pointer.next
    }
    str
  }

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
    (0 until _size.toInt).map(getOrElse2(_, false)).toList

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
