object TreesTest {
    abstract class ATree
    case class ALeaf(a : Int) extends ATree
    case class ABranch( l: ATree, r: ATree) extends ATree

    abstract class BTree
    case class BLeaf(a : Int) extends BTree
    case class BBranch( l: BTree, r: BTree) extends BTree
    case class BBLeaf(a: Int, b : Int) extends BTree
    
    def f( x : ATree) : BTree = {
        x match {
            case ABranch(ALeaf(a), ALeaf(b)) => BBLeaf(a,b)
            case ABranch(l, r) => BBranch(f(l), f(r)) 
            case ALeaf(a) => BLeaf(a)
        }
    }
    val f1 : ATree => BTree = ( x : ATree) => {
        x match {
            case ABranch(ALeaf(a), ALeaf(b)) => BBLeaf(a,b)
            case ABranch(l, r) => BBranch(f(l), f(r)) 
            case ALeaf(a) => BLeaf(a)
        }
    }

  
    // TODO : find equivalent of:
    def f_1(y: BTree) : ATree = {
            y match {
            case BBLeaf(a, b) => ABranch(ALeaf(a), ALeaf(b))
            case BBranch(l, r) => ABranch(f_1(l), f_1(l))
            case BLeaf(a) => ALeaf(a)
        }
    }
}
