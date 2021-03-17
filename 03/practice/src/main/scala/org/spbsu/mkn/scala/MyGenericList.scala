package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[NT](f: T => NT): MyGenericList[NT]
  def ::[ST >: T](elem: ST): MyGenericList[ST] = new ::(elem, this)
}

case class ::[T](head: T, tail: MyGenericList[T]) extends MyGenericList[T] {
  override def drop(n: Int): MyGenericList[T] = {
    if (n < 0) undef
    n match {
      case 0 => this
      case _ => tail.drop(n - 1)
    }
  }
  override def take(n: Int): MyGenericList[T] = {
    if (n < 0) undef
    n match {
      case 0 => MyNil
      case _ => head :: tail.take(n - 1)
    }
  }
  override def map[NT](f: T => NT): MyGenericList[NT] = f(head) :: tail.map(f)
}

case object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef
  override def tail: MyGenericList[Nothing] = MyNil
  override def drop(n: Int): MyGenericList[Nothing] = if (n == 0) MyNil else undef
  override def take(n: Int): MyGenericList[Nothing] = if (n == 0) MyNil else undef
  override def map[NT](f: Nothing => NT): MyGenericList[Nothing] = MyNil
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T](seq: Seq[T]): MyGenericList[T] = if (seq.isEmpty) MyNil else seq.head :: fromSeq(seq.drop(1))
  def sum(myGenericList: MyGenericList[Int]): Int      = myGenericList match {
    case MyNil => undef
    case x :: MyNil => x
    case x :: xs => x + sum(xs)
  }
  def size[T](myGenericList: MyGenericList[T]): Int     = myGenericList match {
    case MyNil => 0
    case _ :: xs => 1 + size(xs)
  }
  // extra task: implement sum using foldLeft
  // def foldLeft(???)(???): ??? = ???
}