package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala
import org.spbsu.mkn.scala.MyGenericList.{fromSeq, size, sort, sum}

class MyGenericListTest extends AnyFunSuite {


  test("head") {
    assert(fromSeq(Seq(1,2,3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1,2,3)).tail == fromSeq(Seq(2,3)))
    assert(fromSeq(Seq(1)).tail == MyNil)
  }

  test("drop") {
    assert(fromSeq(Seq(1,2,3)).drop(0) == fromSeq(Seq(1,2,3)))
    assert(fromSeq(Seq(1,2,3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1,2,3)).drop(3) == MyNil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).drop(10))
  }

  test("take") {
    assert(fromSeq(Seq(1,2,3)).take(0) == MyNil)
    assert(fromSeq(Seq(1,2,3)).take(2) == fromSeq(Seq(1,2)))
    assert(fromSeq(Seq(1,2,3)).take(3) == fromSeq(Seq(1,2,3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).take(10))
  }

  test("map") {
    assert((MyNil : MyGenericList[Int]).map(_ * 2) == MyNil)
    assert(fromSeq(Seq(1,2,3)).map(_ * 2) == fromSeq(Seq(2,4,6)))
    assert(fromSeq(Seq(1,2,3)).map(identity) == fromSeq(Seq(1,2,3)))
  }

  test("size") {
    assert(size(MyNil) == 0)
    assert(size(fromSeq(Seq(1,2,3))) == 3)
  }

  test("sum") {
    assertThrows[UnsupportedOperationException](sum(MyNil))
    assert(sum(fromSeq(Seq(1,2,3))) == 6)
    assert(sum(fromSeq(Seq(1))) == 1)
  }

  test("++") {
    assert(fromSeq(Seq(1, 2)) ++ fromSeq(Seq(3, 4)) == fromSeq(Seq(1, 2, 3, 4)))
    assert(fromSeq(Seq(1, 2)) ++ (MyNil: MyGenericList[Int]) == fromSeq(Seq(1, 2)))
    assert((MyNil: MyGenericList[Int]) ++ fromSeq(Seq(3, 4)) == fromSeq(Seq(3, 4)))
  }

  test("filter") {
    assert(fromSeq(Seq(1, 2, 3, 4)).filter(x => x > 2) == fromSeq(Seq(3, 4)))
    assert(fromSeq(Seq(1, 2, 3, 4)).filter(x => x % 2 == 0) == fromSeq(Seq(2, 4)))
    assert((MyNil: MyGenericList[Int]).filter(x => x > 2) == (MyNil: MyGenericList[Int]))
  }

  test("sort") {
    assert(sort(fromSeq(Seq(3, 4, 1, 2))) == fromSeq(Seq(1, 2, 3, 4)))
    assert(sort(fromSeq(Seq(3, 4, 1, 2)))(Ordering.Int.reverse) == fromSeq(Seq(4, 3, 2, 1)))
    assert(sort(fromSeq(Seq(3, 2, 2, 1, 2))) == fromSeq(Seq(1, 2, 2, 2, 3)))
    assert(sort(fromSeq(Seq(1))) == fromSeq(Seq(1)))
    assert(sort(MyNil: MyGenericList[Int]) == (MyNil: MyGenericList[Int]))
  }

}
