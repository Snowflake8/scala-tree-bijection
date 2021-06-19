class TreesTest2 {
  abstract class NaturalTree
  case class NBranch(l : NaturalTree, r : NaturalTree) extends NaturalTree
  case class Leaf(a : Int) extends NaturalTree

  abstract class EvenOddTree
  case class Branch(l : EvenOddTree, r : EvenOddTree) extends EvenOddTree
  case class EvenLeaf(a : Int) extends EvenOddTree
  case class OddLeaf(a : Int) extends EvenOddTree

  def g(x : NaturalTree): EvenOddTree = {
      x match {
        case Leaf(a) => if (a % 2 == 0) {
                                EvenLeaf(a)
                            } else {
                                OddLeaf(a)
                            }
                        
        case NBranch(l, r) => Branch(g(l), g(r)) 
    }
  }

  // TODO : find (This function should not be marked as bijective)
  def g_1(y : EvenOddTree) : NaturalTree = {
        y match {
        case EvenLeaf(a) => Leaf(a)
        case OddLeaf(a) => Leaf(a)
        case Branch(l, r) => NBranch(g_1(l), g_1(r)) 
    }
  }

}
