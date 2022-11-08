import scala.annotation.tailrec

def position[A](elem:A, list:List[A]):List[Int] ={
  @tailrec
  def position_rec[A](element: A, remaining: List[A], i: Int, result: List[Int]): List[Int] = {
    if remaining.isEmpty then result
    else {
      val check = remaining.head;
      check match
        case check if check==element => position_rec(element, remaining.tail, i + 1, i :: result)
        case _ => position_rec(element, remaining.tail, i + 1, result)
    }
  }

  position_rec(elem, list, 0, List()).reverse;
}

position(1, List(1,3,4,1,2,1,6,8,9));
position(10, List(1,3,4,1,2,1,6,8,9));
position(1, List());