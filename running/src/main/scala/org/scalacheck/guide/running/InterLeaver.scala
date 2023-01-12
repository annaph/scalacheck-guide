package org.scalacheck.guide.running

object InterLeaver {

  def interleave[T](l1: List[T], l2: List[T]): List[T] =
    (l1, l2) match {
      case (Nil, _) =>
        l2
      case (_, Nil) =>
        l1
      case (head1 :: tail1, head2 :: tail2) =>
        // Intentional bug, should be: head1 :: head2 :: interleave(tail1, tail2)
        head1 :: head2 :: interleave(tail2, tail1)
    }

}
