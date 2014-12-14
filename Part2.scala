case class OInt (i: Int) extends Ordered[OInt] {
  val data: Int = i;

  def get() = this.data;

  def compare(that: OInt) = this.data - that.data;
}

object Part2 {
  def main(args: Array[String]) = {
    println("BUDDY");
    //var foo = (new OInt(5)).compare(new OInt(6));
    //println("Should return -1: " + foo);
  }
}
