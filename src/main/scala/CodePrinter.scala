import scala.quoted.*
import scala.reflect.*
import TreesTest as T
import scala.annotation.tailrec
import dotty.tools.dotc.reporting.Diagnostic.Warning
import org.scalajs.ir.Types.NothingType
import dotty.tools.dotc.ast.Trees.EmptyTree

//Remains of my printed code
object CodePrinter {
    def showExpr[A,B](expr: Expr[A => B]) (using Quotes): Expr[String] =
        val code: String = expr.show
        Expr(code)
    
    def showExpr2[A,B](expr: Expr[A=>B])(using Quotes): Expr[String] =
        import quotes.reflect.*

        def treePrint(tree: Tree, level: Int): Unit = {
            val pre = "  " * level

            def listPrint(trees : List[Tree]): Unit = {
                trees.zipWithIndex.map(
                            (tree, index) =>  
                                treePrint(tree, level +1)
                                if trees.size > 1 && index < trees.size -1  then 
                                    // Used to seperate list of parameters
                                    println(pre + ",")
                            )
            }

            tree match {

                // Terms
                //case body : Term => 
                //    body match{
                case Typed(expr, tpt) =>
                    //println(tree.getClass())
                    //TODO: tpt can be complex 
                    println(pre + s"Typed with ${tpt}:")
                    treePrint(expr , level + 1)

                case Block(statements, expr) =>
                    println(pre + "Block:{")
                    statements.map(stat => stat match{
                        case term: Term => treePrint(term, level + 1)
                        case deff: Definition =>
                            println(pre + "Definition statement")
                            treePrint(deff, level + 1)
                        case _ =>
                            println(pre + "Non-term statement")
                            println(stat.show(using Printer.TreeStructure))
                        })
                    treePrint(expr, level + 1)
                    println(pre + "}")

                case Match(scrutinee, cases) =>
                    println(pre + "Match:")
                    treePrint(scrutinee, level + 1)
                    println(pre + "with")
                    cases.map(treePrint(_, level +1))

                case Select(qualifier, name) => 
                        println(pre + s"Select $name as")
                        treePrint(qualifier, level + 1)

                case Ident(name) =>
                    println(pre + s"Identifier(${name})")

                case Apply(fun, args) =>
                    println(pre + "Apply:")
                    treePrint(fun, level + 1)
                    if !args.isEmpty then
                        println(pre + "with arguments")
                        listPrint(args)

                    //}

                //Other Trees
                case Typed(expr, tpt) =>
                    //println(tree.getClass())
                    //TODO: tpt can be complex 
                    println(pre + s"Typed2 with ${tpt}:")
                    treePrint(expr , level + 1)

                case Unapply(fun, implicits, pattern) => 
                    println(pre + "Unapply:") 
                    treePrint(fun , level + 1)
                    if !implicits.isEmpty then
                        println(pre + "using")
                        listPrint(implicits)
                    if !pattern.isEmpty then
                        println(pre + "on patterns")
                        listPrint(pattern)

                case CaseDef(pattern, guard, rhs) =>
                    println(pre + "caseDef:" )
                    treePrint(pattern, level + 1)
                    if guard.isDefined then
                        treePrint(guard.get, level + 1)
                    println(pre + "  =>")
                    treePrint(rhs, level + 1)

                case Bind(sym, pattern) =>
                        println(pre + s"Bind $sym to")
                        treePrint(pattern, level + 1)
                case Inlined(call, bindings, expansion) =>
                        if call.isDefined then 
                            println(pre + "Inlined with call: ")
                            treePrint(call.get, level + 1)
                        else 
                            println(pre + "Inlined with empty call")
                        println(pre+"with bindings")
                        listPrint(bindings)
                        println(pre+"with expansion")
                        treePrint(expansion, level + 1)
                        
                        

                case _ => 
                    tree match
                        case t: Term => println("Term")
                        case _ => ()
                    println(tree.getClass())
                    println(tree.show(using Printer.TreeStructure))
            }
        }
        val tree:Tree = expr.asTerm
        treePrint(tree,0)
        //println(tree.show(using Printer.TreeStructure))
        Expr(tree.show(using Printer.TreeStructure))
        

    
    

    def showExpr3[A,B](expr: Expr[A=>B])(using Quotes): Expr[String] =
        import quotes.reflect.*
        

        def printDefFun(tree: Tree): Unit ={
            val acc = new TreeAccumulator[Unit]{
                def foldTree(s: Unit, tree: Tree)(owner: Symbol): Unit = 
                    tree match
                        case deff : DefDef =>
                            val front:String = "(" + deff.paramss.map(_.toString()).fold("")(_+", "+_) + ") => "
                            if deff.rhs.isDefined then 
                                println(front)
                                treePrint(deff.rhs.get, 0)
                                println("++++++++++++++++")

                        case _ =>
                            foldOverTree(s, tree)(owner)
                }
            acc.foldTree(List(), tree)(tree.symbol)
        }

        
        def treePrint(tree: Tree, level: Int): Unit = {
            val pre = "  " * level

            def listPrint(trees : List[Tree]): Unit = {
                trees.zipWithIndex.map(
                            (tree, index) =>  
                                treePrint(tree, level +1)
                                if trees.size > 1 && index < trees.size -1  then 
                                    // Used to seperate list of parameters
                                    println(pre + ",")
                            )
            }

            tree match {

                // Terms
                //case body : Term => 
                //    body match{
                case Typed(expr, tpt) =>
                    //println(tree.getClass())
                    //TODO: tpt can be complex 
                    println(pre + s"Typed with ${tpt}:")
                    treePrint(expr , level + 1)

                case Block(statements, expr) =>
                    println(pre + "Block:{")
                    statements.map(stat => stat match{
                        case term: Term => treePrint(term, level + 1)
                        case deff: Definition =>
                            println(pre + "Definition statement")
                            treePrint(deff, level + 1)
                        case _ =>
                            println(pre + "Non-term statement")
                            println(stat.show(using Printer.TreeStructure))
                        })
                    treePrint(expr, level + 1)
                    println(pre + "}")

                case Match(scrutinee, cases) =>
                    println(pre + "Match:")
                    treePrint(scrutinee, level + 1)
                    println(pre + "with")
                    cases.map(treePrint(_, level +1))

                case Select(qualifier, name) => 
                        println(pre + s"Select $name as")
                        treePrint(qualifier, level + 1)

                case Ident(name) =>
                    println(pre + s"Identifier(${name})")

                case Apply(fun, args) =>
                    println(pre + "Apply:")
                    treePrint(fun, level + 1)
                    //HERE ===== printDefFun(fun.symbol.tree)
                    print(fun.symbol.tree)     
                    print("==============:"+ fun.symbol.signature)           
                    if !args.isEmpty then
                        println(pre + "with arguments")
                        listPrint(args)

                    //}

                //Other Trees
                case Typed(expr, tpt) =>
                    //println(tree.getClass())
                    //TODO: tpt can be complex 
                    println(pre + s"Typed2 with ${tpt}:")
                    treePrint(expr , level + 1)

                case Unapply(fun, implicits, pattern) => 
                    println(pre + "Unapply:") 
                    treePrint(fun , level + 1)
                    if !implicits.isEmpty then
                        println(pre + "using")
                        listPrint(implicits)
                    if !pattern.isEmpty then
                        println(pre + "on patterns")
                        listPrint(pattern)

                case CaseDef(pattern, guard, rhs) =>
                    println(pre + "caseDef:" )
                    treePrint(pattern, level + 1)
                    if guard.isDefined then
                        treePrint(guard.get, level + 1)
                    println(pre + "  =>")
                    treePrint(rhs, level + 1)

                case Bind(sym, pattern) =>
                        println(pre + s"Bind $sym to")
                        treePrint(pattern, level + 1)

                case _ => 
                    tree match
                        case t: Term => println("Term")
                        case _ => ()
                    println(tree.getClass())
                    println(tree.show(using Printer.TreeStructure))
            }
        }
        
        val tree: Term = expr.asTerm
        println(tree.show(using Printer.TreeStructure))
        printDefFun(tree)
        Expr("Finished")


}
