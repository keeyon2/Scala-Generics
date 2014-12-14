case class OInt (i: Int) extends Ordered[OInt] {
  val data: Int = i;

  def compare(otherOInt: OInt) = otherOInt match {
    case OInt(otherI) => i compare otherI
  }
}

// Definition of what the Abstract Class it
abstract class OTree[T] extends Ordered[OTree[T]] 

case class ONode[T <: Ordered[T]] (ls: List[OTree[T]]) extends OTree[T]
{
  def compare(otherTree: OTree[T]) = otherTree match {
    case OLeaf(_) => 1;
    case ONode(x::xs) =>
      var currentCompare: Int = 0;
      if (ls.isEmpty)
      {
        currentCompare = -1;
      }
      else
      {
        currentCompare = ls.head compare x;
      }
      if ( currentCompare == 0)
      {
        if (!(ls.tail.isEmpty && xs.isEmpty))
        {
          currentCompare = ONode(ls.tail) compare ONode(xs);
        }
      }
      currentCompare;
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
    test();
  }
  def compareTrees[T](Tree1: OTree[T], Tree2: OTree[T])
  {
    val testInt: Int = Tree1 compare Tree2;
    if (testInt < 0)
    {
      println("Less");
    }
    else if (testInt > 0)
    {
      println("Greater");
    }
    else
    {
      println("Equal");
    }
  }
  def test() {

    val tree1 = ONode(List(OLeaf(new OInt(6))))

    val tree2 = ONode(List(OLeaf(new OInt(3)),
         OLeaf(new OInt(4)), 
         ONode(List(OLeaf(new OInt(5)))), 
         ONode(List(OLeaf(new OInt(6)), 
              OLeaf(new OInt(7))))));

    val treeTree1: OTree[OTree[OInt]] = 
      ONode(List(OLeaf(OLeaf(new OInt(1)))))

    val treeTree2: OTree[OTree[OInt]] = 
      ONode(List(OLeaf(OLeaf(new OInt(1))),
     OLeaf(ONode(List(OLeaf(new OInt(2)), 
          OLeaf(new OInt(2)))))))


    print("tree1: ")
    println(tree1)
    print("tree2: ")
    println(tree2)
    print("treeTree1: ")
    println(treeTree1)
    print("treeTree2: ")
    println(treeTree2)
    print("Comparing tree1 and tree2: ")
    compareTrees(tree1, tree2)
    print("Comparing tree2 and tree2: ")
    compareTrees(tree2, tree2)
    print("Comparing tree2 and tree1: ")
    compareTrees(tree2, tree1)
    print("Comparing treeTree1 and treeTree2: ")
    compareTrees(treeTree1, treeTree2)
    print("Comparing treeTree2 and treeTree2: ")
    compareTrees(treeTree2, treeTree2)
    print("Comparing treeTree2 and treeTree1: ")
    compareTrees(treeTree2, treeTree1)
  }
}
