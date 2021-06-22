import TreesTest.*
import scala.tasty.inspector.*
@main 
def hello: Unit = {

    abstract class W 
    case class Wranch(l : W, r : W) extends W
    case class Weaf(a : Int) extends W

    abstract class V
    case class Vranch(l : V, r : V) extends V
    case class Veaf(a : Int) extends V
    case class VVeaf(a : Int, b : Int) extends V

    def f( x : W) : V = {
        x match {
            case Wranch(TreesTest.ALeaf(a), TreesTest.ALeaf(b)) => VVeaf(a,b)
            case Wranch(l, r) => Vranch(f(l), f(r)) 
            case Weaf(a) => Veaf(a)
        }
    }

    abstract class Atest
    case class AFoo(a: Int, b : Int) extends Atest
    abstract class Btest
    case class BBar(a: Int, b : Int) extends Btest
    def d(e: Atest): Btest = e match{
        case AFoo(a,b) => BBar(b, a)
    }
    
    abstract class A 
    case class Foo(a: Int) extends A 
    abstract class B 
    case class Bar(b : Int) extends B
    def h (a : A) : B ={
        a match {
            //case _ => Bar(9)
            case Foo(aa) => Bar(aa)
        }
    }

    def j(a: A): A = 
        a match {
            case Foo(_) => a
        }
        
    abstract class T
    case class Branch(l:T, r:T) extends T
    case object Nope extends T

    abstract class S 
    case object Yope extends S 

    def s(simple : S): T = simple match {
        case Yope => Nope
    }

    def t(tr: T):T = tr match
        case Branch(l, r) => Branch(t(l), t(r))
        case Nope => Nope
        
        

    def t2(tr: T):T = tr match
        case Branch(l, r) => Branch(t2(l), t2(r))
        case Nope => Branch(Nope, Nope)

    abstract class L 
    case class LBranch(l : L) extends L 
    case object Maybe extends L

    def b(l : L): L = l match 
        case LBranch(l) => LBranch(b(l)) 

    
    // Succeed
    // Macros.inverse(b)
    // Macros.inverse(h)
    // Macros.inverse(s)
    // Macros.inverse(t)
    // Macros.inverse(t2)

    // Should (but shouldn't) fail 
    // This is due to a case not being covered
    // Macros.inverse(f)
    
    // Should (and does) fail 
    // Macros.inverse(j)
}

def msg = "I was compiled by Scala 3. :)"
