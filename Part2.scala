import scala.math;

case class OInt (i: Int) extends Ordered[OInt] {
  val data: Int = i;

  //def compare(that: OInt) = this.data - that.data;
  def compare(otherOInt: OInt) = otherOInt match {
    case OInt(otherI) => i compare otherI
  }
}

class KInt (i: Int) {
  val data: Int = i;

}

// Definition of what the Abstract Class it
abstract class OTree[T] extends Ordered[OTree[T]] 

case class ONode[T <: Ordered[T]] (ls: List[OTree[T]]) extends OTree[T]
{
  def compare(otherTree: OTree[T]) = otherTree match {
    case OLeaf(_) => 1;
    case ONode(x::xs) =>
      var currentCompare: Int = ls.head compare x;
      if ( currentCompare == 0)
      {
        currentCompare = ONode(ls.tail) compare ONode(xs);
      }
      currentCompare;
    /*case ONode(other_List) => 
      val arrayLength: Int = math.abs(ls.length - other_List.length);
      var comparingInt: Int = 0;
      for (x <- 0 to arrayLength - 1)
      {
        comparingInt = ls(x) compare other_List(x);
        if (comparingInt != 0)
        {
          return comparingInt;
        }
      }
      if (ls.length > other_List.length)
      {
        return 1;
      }
      else if(ls.length < other_List.length)
      {
        return -1;
      }
      else
      {
        return 0;
      }
      val headCompare: Int = ls.head compare other_List.head;
      if (headCompare == 0)
      {
        headCompare = ls(1) compare 
      }
      headCompare*/
    case _ => 1
  }
}

case class OLeaf[T <: Ordered[T]] (i: T) extends OTree[T] 
{
  def compare(otherTree: OTree[T]) = otherTree match{
    case OLeaf(other_leaf) => i compare (other_leaf);
    case ONode(_) => -1;
    case _ => 1;
  }
}

object Part2 {
  def main(args: Array[String]) = {
    //println("BUDDY");
    var foo = (new OInt(5)).compare(new OInt(6));
    println("Should return -1: " + foo);
  }
  def compareTrees[T](Tree1: OTree[T], Tree2: OTree[T])
  {
    println("OH HI");
  }
}
