import TreesTest.*
import scala.tasty.inspector.*
@main 
def hello: Unit = {
    def f( x : TreesTest.ATree) : TreesTest.BTree = {
        x match {
            case TreesTest.ABranch(TreesTest.ALeaf(a), TreesTest.ALeaf(b)) => TreesTest.BBLeaf(a,b)
            case TreesTest.ABranch(l, r) => TreesTest.BBranch(f(l), f(r)) 
            case TreesTest.ALeaf(a) => TreesTest.BLeaf(a)
        }
    }
     val f1 : TreesTest.ATree => TreesTest.BTree = ( x : TreesTest.ATree) => {
        x match {
            case TreesTest.ABranch(TreesTest.ALeaf(a), TreesTest.ALeaf(b)) => TreesTest.BBLeaf(a,b)
            case TreesTest.ABranch(l, r) => TreesTest.BBranch(f(l), f(r)) 
            case TreesTest.ALeaf(a) => TreesTest.BLeaf(a)
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
        case Nope => Nope
        case Branch(l, r) => Branch(t(l), t(r))
        

    def t2(tr: T):T = tr match
        case Branch(l, r) => Branch(t(l), t(r))
        case Nope => Branch(Nope, Nope)

    abstract class L 
    case class LBranch(l : L) extends L 
    case object Maybe extends L

    def b(l : L): L = l match 
        case Maybe => Maybe
        case LBranch(l) => LBranch(b(l))
    /*
    Macros.show(( x : TreesTest.ATree) => {
        x match {
            case TreesTest.ABranch(TreesTest.ALeaf(a), TreesTest.ALeaf(b)) => TreesTest.BBLeaf(a,b)
            case TreesTest.ABranch(l, r) => TreesTest.BBranch(f(l), f(r)) 
            case TreesTest.ALeaf(a) => TreesTest.BLeaf(a)
        }}
    )
    */
    //Macros.show(f)
    //Macros.show(h)
    val f2 = {def s1(simple : S): T = simple match {
                case Yope => Nope
            }
            s1
        }
    //val sTest = Macros.inverse(s)
    //Macros.inverse(f)
    //def s1(notSimple : T): S = Macros.inverseBody(s, s1, notSimple)
    //Macros.inverse(b)
    //Macros.inverse(h)
    //Macros.inverse(s)
    //Macros.inverse(t)
    Macros.inverse(d)
    //println(Macros.inverse(s)(s(Yope)))
    //Macros.show()
    //Macros.show(s)
    //Macros.show(t) 
    //Macros.show(j)
    /*
    print("=================")
    Macros.show(( x : TreesTest.ATree) => {
        x match {
            case ALeaf(a) => BLeaf(a)
        }}
    )
    /*
    println("===============")
    Macros.show(f)
    Macros.show(f1)
    println("===============")
    Macros.show(TreesTest.f)
    Macros.show(TreesTest.f1)
    println("===============")
    
    Macros.show((x:ATree)=>{
        x match {
            case TreesTest.ABranch(TreesTest.ALeaf(a), TreesTest.ALeaf(b)) => TreesTest.BBLeaf(a,b)
            case TreesTest.ABranch(l, r) => TreesTest.BBranch(f(l), f(r)) 
            case TreesTest.ALeaf(a) => TreesTest.BLeaf(a)
        }}:BTree)
    */
    //val tastyFiles = List("target/scala-3.0.0-RC2/classes/TreesTest.tasty")*/
    //TastyInspector.inspectTastyFiles(tastyFiles)(new MyInspector)*/
}

def msg = "I was compiled by Scala 3. :)"
