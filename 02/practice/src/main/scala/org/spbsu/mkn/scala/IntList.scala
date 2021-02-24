package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList = new ::(elem, this)
}

case class ::(override val head: Int, override val tail: IntList) extends IntList {
  override def drop(n: Int): IntList = {
    if (n < 0) undef
    n match {
      case 0 => this
      case _ => tail.drop(n - 1)
    }
  }
  override def take(n: Int): IntList = {
    if (n < 0) undef
    n match {
      case 0 => IntNil
      case _ => head :: tail.take(n - 1)
    }
  }
  override def map(f: Int => Int): IntList = f(head) :: tail.map(f)
}

case object IntNil extends IntList {
  override def head: Int = undef
  override def tail: IntList = IntNil
  override def drop(n: Int): IntList = if (n == 0) IntNil else undef
  override def take(n: Int): IntList = if (n == 0) IntNil else undef
  override def map(f: Int => Int): IntList = IntNil
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = if (seq.isEmpty) IntNil else seq.head :: fromSeq(seq.drop(1))
  def sum(intList: IntList): Int      = intList match {
    case IntNil => undef
    case x :: IntNil => x
    case x :: xs => x + sum(xs)
  }
  def size(intList: IntList): Int     = intList match {
    case IntNil => 0
    case _ :: xs => 1 + size(xs)
  }
  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}
