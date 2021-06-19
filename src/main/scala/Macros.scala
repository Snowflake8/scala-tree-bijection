import scala.quoted.*
import scala.reflect.*
import TreesTest as T
import scala.annotation.tailrec
import dotty.tools.dotc.reporting.Diagnostic.Warning
import org.scalajs.ir.Types.NothingType
import dotty.tools.dotc.ast.Trees.EmptyTree

def isEmpty[T](list: List[T]) : Boolean = list == Nil || list == List[T]()
object Macros {
    /*
    inline def f( x : T.ATree) : T.BTree = {
        x match {
            case T.ABranch(T.ALeaf(a), T.ALeaf(b)) => T.BBLeaf(a,b)
            case T.ABranch(l, r) => T.BBranch(f(l), f(r)) 
            case T.ALeaf(a) => T.BLeaf(a)
        }
    }
    
    inline def inverseBody[A, B](inline expr: A => B, fun: B=>A, arg: B): B =
        ${getMatchfirstLevel('{expr}, '{fun}, '{arg})}
    */
        
    inline def inverse[A, B](inline expr: A => B): B => A=
        ${firstLevel('expr)}

        /*
    def getMatchfirstLevel[A : Type , B : Type](expr: Expr[A => B], fun: Expr[B=>A], arg: Expr[B])(using Quotes) = { // Todo change this : Expr[B=>A] 
        import quotes.reflect.*
        import reflect._

        val apply = "apply"
        val unapply = "unapply"

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

        def extractFunction(tree: Tree): Match = {
            println(s"Tree : $tree")
            tree match {
                case Inlined(_,_, Block(list, term)) => 
                    //println(s"List size : ${list.size}")
                    if (list.size == 1)
                        //println(s"List: $list")
                        //println(s"Term: $term")
                        //println(s"List 0 :${list(0)}")
                        list.head match {
                            case DefDef(name, params, typeTree, body)=> 
                                //println(s"Inner term: $body")
                                println(s"First level type: ${list.head.asInstanceOf[DefDef]}")
                                body match {
                                    case Some(Apply(function, x)) => 
                                        handleFunctionIdentifier(function.asInstanceOf[Ident])
                                    case None => throw new Error("No function body detected in autogenerated lambda")
                                    case _ => throw new Error("Incorrect function body detected")
                                }

                        }
                    else
                        //TODO make this correct
                        throw new Error("Only the function must be passed")
                case Inlined(_,_, x :Ident) => handleFunctionIdentifier(x)
                case _ => 
                    throw new Error("Unknown format encountered in extractFunction")
            }

            
        }

        def handleFunctionIdentifier(function : Ident): Match ={
            val signature = function.symbol.signature
            println(s"Function signature: ${signature}")
            //maybe check signature later
            //if(signature.paramSigs.size != 1)
            //    throw new Error("Function should only have one parameter")
            /* 
            val newSignature = Signature(List(signature.resultSig), signature.paramSigs(0))
            println(s"New function signature: ${newSignature}")
            */
            //println(s"Function Tree :")
            //println(function.symbol.tree)
            secondLevel(function.symbol.tree)
            //treePrint(function.symbol.tree.asInstanceOf[DefDef].rhs.get, 0)     
        }

        def secondLevel(tree : Tree): Match = {
            def createMatch(body : Option[Term], inputName : String): Match = 
                
            //val newScrutinee = {)
            //        x(0)(0).asInstanceOf[Term]}
                
                body match {
                        case Some(Block( statements, Match(scrutinee, cases))) =>
                            if (!isEmpty(statements))
                                throw new Error("Function does not respect format. Only functions containing one match are allowed")
                            println(s"We got here with scrut as $scrutinee")

                            //Check of format
                            scrutinee match {
                                case Ident(s) => 
                                    if( s != inputName)
                                        throw new Error("Only input is accepted as scrutinee for match")
                                case _ => throw new Error("Only input is accepted as scrutinee for match")
                            }

                            handleMatch(Match(scrutinee, cases))
                        case Some(Match(scrutinee, cases)) =>
                            println(s"We got here with scrut as $scrutinee")

                            //Check of format
                            scrutinee match {
                                case Ident(s) => 
                                    if( s != inputName)
                                        throw new Error("Only input is accepted as scrutinee for match")
                                case _ => throw new Error("Only input is accepted as scrutinee for match")
                            }

                            handleMatch(Match(scrutinee, cases))
                        case None =>  throw new Error("No function body detected")
                        case _ => throw new Error("Function does not respect format. Only functions containing one match are allowed")
                    }


            println(s"Inner Tree is $tree")
            println{s"Inner signature is ${tree.symbol.signature}"}
            tree match {
                case DefDef(functionName, params, typeTree, body) =>
                    println(s"Total type is ${tree.asInstanceOf[Term].tpe}")
                    println(s"FunctionType is ${typeTree.tpe}")
                    println(s"Params are ${params.map(_.params)}")
                    if(params.size != 1)
                        throw new Error("Function should only have one parameter")
                    val inputDefinition = params.head.params.head
                    val (inputName, inputType) = inputDefinition match {
                        case ValDef(name, tp, None) => (name, tp)
                        case _ => throw new Error("Function parameter was not inexpected format")
                    }
                    println(s"What we want is $typeTree to $inputType")
                    //val newParam = "$y"
                    //DefDef()
                    //println(s"Body $body")

                    // We want as signature ~Signature(List(Main$package$._$S),Main$package$._$T)
                    // We have the body of the function

                    //Symbol.spliceOwner
                    //Symbol.noSymbol

                    val a = List(List(ValDef(inputDefinition.symbol, None)))
                    // LambdaTypeTree(tparams: List[TypeDef], body: Tree) then .tpe 
                    // TypeLambda(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr)
                    //Symbol.spliceOwner
                    val newSymbol = Symbol.newMethod(Symbol.noSymbol, s"$$$functionName-inverse", 
                        //LambdaTypeTree(
                        //    List(
                        //        TypeDef(typeTree.symbol)), 
                        //        inputType).tpe)//Applied(typeTree, List(inputType)).tpe)
                        inputType.tpe)
                    println(s"New signature ${newSymbol.signature}")
                    //Maybe enclose in lambdaTypeTree and just return it
                   //.changeOwner(newSymbol)
                        //List(List(ValDef(inputDefinition.symbol,EmptyTree))) 
                        //Nil
                        //=> Some(newMatch))
                    createMatch(body, inputName)
                case _ => throw new Error("A defined function was not found")
            }
        }

        def handleMatch(matche: Match) : Match = 
            println(s"Match scrutinee ${matche.scrutinee}")
            matche.scrutinee match {
                // Check that scutinee is an identifier
                case Ident(s) => 
                    val res = Match(arg.asTerm, matche.cases.map(handleCases(_, matche.scrutinee)))
                    res.asExpr.asInstanceOf[B]
                    res
                case _ => throw new Error("Only simple identifier is accepted as scrutinee for match")
                //Todo more
            }

        
        def handleCases(caseDef : CaseDef, scrutinee: Term) : CaseDef = {
            println(s"Case with $caseDef")
            caseDef match {
                case CaseDef(_, Some(_), _) => throw new Error("Case with guard are not yet supprorted") //todo
                case CaseDef(pattern, None, rhs) => 
                    println(s"Pattern: $pattern")
                    println(s"Rhs: $rhs")
                    val (requests, newRhs) = handlePattern(pattern)
                    val newPat = handleRhs(rhs, requests)
                    println(s"We get ${CaseDef(newPat, None, newRhs)}")
                    CaseDef(newPat, None, newRhs)
                    //Warning("Function contains wildcard as pattern")
            }
            
            
        }
        /*
        def collectPatternVariables(tree: Tree): List[Ident] = {
            val acc = new TreeAccumulator[List[Ident]]{
                def foldTree(idents: List[Ident], tree: Tree)(owner: Symbol): List[Ident] = tree match
                    case x : Ident =>
                        foldTree(x :: idents, body)(tree.symbol)
                    case _ =>
                        foldOverTree(idents, tree)(owner)
                }
            acc(Nil, tree)
        }*/

        def handlePattern(pattern: Tree): (List[String], Term) = 
            pattern match {
                case Ident(s) => 
                    if s == "_" then
                        System.err.println("Function contains wildcard as pattern, bijection may fail with MatchError. Removal of this case is suggested")
                        (Nil, '{throw new Error("Impossible to biject, wildcard cannot be inverted")}.asTerm)
                    else
                        (Nil, pattern.asInstanceOf[Ident])
                case Some(e) => handlePattern(e.asInstanceOf[Tree])
                case Block(statements, e) => 
                    if !isEmpty(statements) then 
                        throw new Error("Pattern is a non-empty block")
                    else
                        handlePattern(e)
                case Typed(term, typeTree) =>
                    val res = handlePattern(term)
                    (res._1, Typed(res._2, typeTree))
                    
                //case Unapply(_,_,_)=> (Nil, Literal(IntConstant(5)))
                
                case Unapply(fun, implicits, pattern) => 
                    if !isEmpty(implicits) then
                        throw new Error(s"handlePattern could not handle unapply which non-empty implicits: ${implicits}")

                    // gives list of parameters, and the corresponding terms to give to the apply
                    def unbindStrings(t : Tree): (List[String], List[Term]) = t match {
                        case Bind(string, tree) => 
                            tree match {
                                case Ident(name) if name == "_" =>
                                        (List(string), List(Ident(TermRef(tree.asInstanceOf[Ident].tpe, string))))
                                case _ : Term =>
                                    val (strings, terms) = unbindStrings(tree)
                                    //throw new Error("unbindString No simple Isdent in bind")
                                    (string :: strings, Ident(TermRef(tree.asInstanceOf[Term].tpe, string)) :: terms)
                                case _ => throw new Error(s"unbindString bind with non term tree ${tree}")
                            }
                        case _ => 
                            throw new Error(s"unbindString non-bind with ${t}")
                    }
                    //create function to handle select
                    //Use bindings in pattern to handle this,  Apply(Select apply from the fun , List the identifiers ...)
                    val newFun : Term = fun match {
                        case Select(term, string) => 
                            if string != unapply then 
                                throw new Error(s"handlePattern could not handle unapply with non-apply function selected")
                            val symbols = term.symbol.memberMethod(apply)
                            if isEmpty(symbols) then 
                                throw new Error(s"handlePattern could not find apply method on pattern")
                            Select(term, symbols.head)
                    }
                
                    val (strings, terms) = pattern.map(unbindStrings(_)).unzip.match { case (x, y) => (x.flatten, y.flatten) }
                    (strings ,Apply(newFun, terms))
                
                case _ =>
                    println("Pattern: " + pattern.show(using Printer.TreeStructure) )
                    throw new Error(s"handlePattern could not handle ${pattern}")
                //case Ident("_") => 
                    //UnApply(Select(Ident("Foo"),"unapply"),List(),List(Bind("aa",Ident("_"))))
                    //throw new Warning("Function contains wildcard as pattern, bijection may fail with MatchError. Removal of this case is suggested", tree.pos)
                    //TODO
            }

        def handleRhs(term: Term, requests: List[String]): Tree ={
            term match{
                case Ident(s) => 
                    //TODO use requests
                    return term
                case Block(statements, e) => 
                    if !isEmpty(statements) then 
                        throw new Error("handleRhs Rhs is a non-empty block")
                    else
                        handleRhs(e,requests)
                case Apply(function, arguments) =>
                    println(s"Apply with function $function and arguments $arguments")
                    def createBinds(term : Term, function : Term) : Bind = term match{
                        
                        case Ident(name) => 
                            println(s"bindTerm is $term")
                            println(s"bindTerm symbol is ${term.symbol}")
                            println(s"bindTerm tree is ${term.tpe}")
                            val newSymbol = Symbol.newBind(term.symbol, name, Flags.EmptyFlags, function.tpe)
                            Bind(newSymbol, Ident.copy(term)("_"))// Ident(TermRef(term.tpe,"_"))
                    }
                    val (clas: Term, newFun : Term) = function match {
                        case Select(term, string) => 
                            if string != apply then 
                                throw new Error(s"handleRhs could not handle apply with non-unapply function selected")
                            val symbols = term.symbol.memberMethod(unapply)
                            if isEmpty(symbols) then 
                                throw new Error(s"handleRhs could not find apply method on term")
                            (term,Select(term, symbols.head))
                    }
                    //todo change to aplly method when implemented
                    println(s"newFun is $newFun")
                    println(s"newFun symbol is ${newFun.symbol}")
                    println(s"newFun tree is ${newFun.tpe}")
                    println(s"clas is $clas")
                    println(s"clas symbol is ${clas.symbol}")
                    println(s"clas tree is ${clas.tpe}")
                    val res = Unapply.copy(term)(newFun, Nil, arguments.map(createBinds(_, clas)))//(function,Nil, generate binds)
                    println(s"Create Unapply: $res")
                    res
                case _ => throw new Error(s"handleRhs could not handle $term")
            }
        }
        //def handleRhs(term: Term)
        val tree : Term = expr.asTerm
        println(s"First level tree is $tree")
        println(s"First level typeRepr is ${tree.tpe.simplified}")
        val fun = extractFunction(tree)
        println(s"Final fun is $fun")
        //treePrint(fun, 0) 
        //'{case $fun($arg) => $term}
        //s"case $fun($arg) => $term"
        //Foo(Foo(aa))
        //Closure needs term, we put Ident (maybe select)
        //Ident needs TermRef (of TypeRepr)
        //TermRef needs (qual: TypeRepr, name: String)
        //println(s"Fun name is ${fun.name}")
        //println(s"Fun typeRepr is ${fun.returnTpt.tpe}")//TermRef(fun.returnTpt.tpe, fun.name) //TermRef(TypeRepr.memberType(fun.symbol), fun.name)
        //val funBlock = Block(List(fun), Closure(Ident(TermRef(tree.tpe.memberType(fun.symbol), fun.name)), Some(fun.returnTpt.tpe) ))//Some(fun.returnTpt.tpe)
        // Maybe in 
        //println(s"funBlock is $funBlock")
        fun.asExpr.asInstanceOf[Expr[B]]
            
        //Expr(tree.show(using Printer.TreeStructure))
        //printOnlyDef(tree)
    
    
    }
    */
    inline def show[A,B](inline expr: A => B): Unit =
        ${printExpr('expr)}
    
    def printExpr[A,B](expr: Expr[A=>B])(using Quotes) = 
        '{  println(${showExpr(expr)})
            println("Onto 2")
            println(${showExpr2(expr)})
            println("Onto 3")
            println(${showExpr3(expr)})}
        //println(${showExpr2(expr)})
        //println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
        //'{println(${showExpr3(expr)})}
        //firstLevel(expr)
        //Expr(1)
      
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
        //printOnlyDef(tree)

    
    

    def showExpr3[A,B](expr: Expr[A=>B])(using Quotes): Expr[String] =
        import quotes.reflect.*
        /*
        def collectPatternVariables(tree: Tree)(using ctx: Context): List[Symbol] = {
            val acc = new TreeAccumulator[List[Symbol]]{
                def foldTree(syms: List[Symbol], tree: Tree)(owner: Symbol): List[Symbol] = tree match
                    case ValDef(_, _, rhs) =>
                        val newSyms = tree.symbol :: syms
                        foldTree(newSyms, body)(tree.symbol)
                    case _ =>
                        foldOverTree(syms, tree)(owner)
            acc(Nil, tree)
            }
        }
        def printOnlyDef(tree: Tree): List[String] ={
            val acc = new TreeAccumulator[List[String]]{
                def foldTree(s: List[String], tree: Tree)(owner: Symbol): List[String] = 
                    tree match
                        case deff : DefDef =>
                            val front:String = "(" + deff.paramss.map(_.toString()).fold("")(_+", "+_) + ") => "
                            if deff.rhs.isDefined then 
                                println(front)
                                treePrint(deff.rhs.get, 0)
                                println("++++++++++++++++")
                                //List(front, deff.rhs.get.show(using Printer.TreeStructure))
                                List[String](front + " Function here")
                            else 
                                List[String](front)
                        case _ =>
                            foldOverTree(s, tree)(owner)
                }
            acc.foldTree(List(), tree)(tree.symbol)
        }
        */
        

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
        
        /*
        def expandTree(body: Term): Unit = {
            body match{
                case typed : Typed =>
                    print(s"Typed with ${typed.tpt}:")
                    expandTree(typed.expr)
                case block : Block =>
                    block.statements.map(expandTree)

            }
        }
        */
        val tree: Term = expr.asTerm
        println(tree.show(using Printer.TreeStructure))
        printDefFun(tree)
        //Expr(printOnlyDef(tree).fold("")(_++_))
        Expr("Finished")


    def firstLevel[A : Type , B : Type](expr: Expr[A => B])(using Quotes) = { // Todo change this : Expr[B=>A] 
        import quotes.reflect.*
        import reflect._

        val apply = "apply"
        val unapply = "unapply"

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

        def extractFunction(tree: Tree): DefDef = {
            println(s"Tree : $tree")
            tree match {
                case Inlined(_,_, Block(list, term)) => 
                    //println(s"List size : ${list.size}")
                    if (list.size == 1)
                        //println(s"List: $list")
                        //println(s"Term: $term")
                        //println(s"List 0 :${list(0)}")
                        list.head match {
                            case DefDef(name, params, typeTree, body)=> 
                                //println(s"Inner term: $body")
                                println(s"First level type: ${list.head.asInstanceOf[DefDef]}")
                                body match {
                                    case Some(Apply(function, x)) => 
                                        handleFunctionIdentifier(function.asInstanceOf[Ident])
                                    case None => throw new Error("No function body detected in autogenerated lambda")
                                    case _ => throw new Error("Incorrect function body detected")
                                }

                        }
                    else
                        //TODO make this correct
                        throw new Error("Only the function must be passed")
                case Inlined(_,_, x :Ident) => handleFunctionIdentifier(x)
                case _ => 
                    throw new Error("Unknown format encountered in extractFunction")
            }

            
        }

        def handleFunctionIdentifier(function : Ident): DefDef ={
            val signature = function.symbol.signature
            println(s"Function name is ${function.name}")
            println(s"Function tpe is ${function.tpe}")
            println(s"Function symbol is ${function.symbol}")
            println(s"Function signature: ${signature}")
            //maybe check signature later
            //if(signature.paramSigs.size != 1)
            //    throw new Error("Function should only have one parameter")
            /* 
            val newSignature = Signature(List(signature.resultSig), signature.paramSigs(0))
            println(s"New function signature: ${newSignature}")
            */
            //println(s"Function Tree :")
            //println(function.symbol.tree)
            secondLevel(function.symbol.tree)
            //treePrint(function.symbol.tree.asInstanceOf[DefDef].rhs.get, 0)     
        }

        def secondLevel(tree : Tree): DefDef = {
            println(s"Function symbol 2 is ${tree.symbol}")

            println(s"Inner Tree is $tree")
            println{s"Inner signature is ${tree.symbol.signature}"}
            tree match {
                case DefDef(functionName, params, typeTree, body) =>
                    println(s"Total type is ${tree.asInstanceOf[Term].tpe}")
                    println(s"FunctionType is ${typeTree.tpe}")
                    println(s"Params are ${params.map(_.params)}")
                    println(s"Original body is $body")
                    if(params.size != 1)
                        throw new Error("Function should only have one parameter")
                    val inputDefinition = params.head.params.head
                    println(s"Input definition symbol is ${inputDefinition.symbol}")
                    val (inputName, inputType) = inputDefinition match {
                        case ValDef(name, tp, None) => (name, tp)
                        case _ => throw new Error("Function parameter was not inexpected format")
                    }
                    println(s"What we want is $typeTree to $inputType")
                    //val newParam = "$y"
                    //DefDef()
                    //println(s"Body $body")

                    // We want as signature ~Signature(List(Main$package$._$S),Main$package$._$T)
                    // We have the body of the function

                    //Symbol.spliceOwner
                    //Symbol.noSymbol

                    val a = List(List(ValDef(inputDefinition.symbol, None)))
                    // LambdaTypeTree(tparams: List[TypeDef], body: Tree) then .tpe 
                    // TypeLambda(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr)
                    val newSymbol = Symbol.newMethod(Symbol.spliceOwner, s"$$$functionName-inverse", 
                        //LambdaTypeTree(
                        //    List(
                        //        TypeDef(typeTree.symbol)), 
                        //        inputType).tpe)//Applied(typeTree, List(inputType)).tpe)
                        inputType.tpe)
                    println(s"New signature ${newSymbol.signature}")
                    def createMatch(body : Option[Term]): Match = {
                        //println(s"Inner x :${x}"   )
                        //val newScrutinee = {)
                        //        x(0)(0).asInstanceOf[Term]}
                        body match {
                            case Some(Block( statements, Match(scrutinee, cases))) =>
                                if (!isEmpty(statements))
                                    throw new Error("Function does not respect format. Only functions containing one match are allowed")
                                println(s"We got here with scrut as $scrutinee")
                                println(s"We got here with scrut symbol as ${scrutinee.symbol}")

                                //Check of format
                                if(scrutinee.symbol != inputDefinition.symbol)
                                    throw new Error("Only function argument is accepted as scrutinee for match")
                                
                                handleMatch(Match(scrutinee, cases), tree.symbol,newSymbol)
                            case Some(Match(scrutinee, cases)) =>
                                println(s"We got here with scrut as $scrutinee")
                                println(s"We got here with scrut symbol as ${scrutinee.symbol}")

                                //Check of format
                                scrutinee match {
                                    case Ident(s) => 
                                        if( s != inputName)
                                            throw new Error("Only input is accepted as scrutinee for match")
                                    case _ => throw new Error("Only input is accepted as scrutinee for match")
                                }

                                handleMatch(Match(scrutinee, cases), tree.symbol, newSymbol)
                            case None =>  throw new Error("No function body detected")
                            case _ => throw new Error("Function does not respect format. Only functions containing one match are allowed")
                        }
                    }
                    /*
                    val newBody = (x:List[List[Tree]]) => Some(createMatch(body, x))
                //Maybe enclose in lambdaTypeTree and just return it
                    DefDef(newSymbol, //Do work on input type
                            newBody)//.changeOwner(newSymbol)
                        //List(List(ValDef(inputDefinition.symbol,EmptyTree))) 
                        //Nil
                        //=> Some(newMatch))
                        */
                    DefDef.copy(tree)
                        (s"$$$functionName-inverse", 
                            List(TermParamClause(List(ValDef.copy(tree)(inputName, typeTree, None)))), 
                            inputType, 
                            Some(createMatch(body)))
                case _ => throw new Error("A defined function was not found")
            }
        }

        def handleMatch(matche: Match, oldFunctionSymbol : Symbol, newFunctionSymbol : Symbol) : Match = {
            println(s"Match scrutinee ${matche.scrutinee}")
            def handleCases(caseDef : CaseDef, scrutinee: Term) : CaseDef = {
                println(s"Case with $caseDef")
                caseDef match {
                    case CaseDef(_, Some(_), _) => throw new Error("Case with guard are not yet supprorted") //todo
                    case CaseDef(pattern, None, rhs) => 
                        println(s"Pattern: $pattern")
                        println(s"Rhs: $rhs")
                        // Pattern becomes Rhs and vice versa
                        val (newPat, translations) = handleRhs(rhs, oldFunctionSymbol, newFunctionSymbol)
                        val newRhs = handlePattern(pattern, newFunctionSymbol, translations)
                        println(s"We get ${CaseDef(newPat, None, newRhs)}")
                        CaseDef(newPat, None, newRhs)
                        //Warning("Function contains wildcard as pattern")
                }
            }
            matche.scrutinee match {
                // Check that scutinee is an identifier
                case Ident(s) => 
                    val res = Match(matche.scrutinee, matche.cases.map(handleCases(_, matche.scrutinee)))
                    res.asExpr.asInstanceOf[B]
                    println(s"Final match is $res")
                    res
                case _ => throw new Error("Only simple identifier is accepted as scrutinee for match")
                //Todo more
            }
        }
        /*
        def collectPatternVariables(tree: Tree): List[Ident] = {
            val acc = new TreeAccumulator[List[Ident]]{
                def foldTree(idents: List[Ident], tree: Tree)(owner: Symbol): List[Ident] = tree match
                    case x : Ident =>
                        foldTree(x :: idents, body)(tree.symbol)
                    case _ =>
                        foldOverTree(idents, tree)(owner)
                }
            acc(Nil, tree)
        }*/

        def handlePattern(pattern: Tree, newFunctionSymbol : Symbol, translations: List[(Symbol, Term)]): Term = 
            println(s"handlePattern Translation is $translations")
            println(s"handlePattern pattern is $pattern") 
            def useTranslation(sym:Symbol): Option[Term] =
                val solutions = translations.distinct.filter(_._1 == sym)
                if solutions.isEmpty then None else Some(solutions.head._2)
            
            pattern match {
                case Ident(s) => 
                    if s == "_" then
                        System.err.println("Function contains wildcard as pattern, bijection may fail with MatchError. Removal of this case is suggested")
                        '{throw new Error("Impossible to biject, wildcard cannot be inverted")}.asTerm
                    else
                        println(s"handlePattern Translation is $translations")
                        println(s"handlePattern Ident is $pattern with symbol ${pattern.symbol}")
                        useTranslation(pattern.symbol).getOrElse(pattern.asInstanceOf[Ident])
                case Some(e) => handlePattern(e.asInstanceOf[Tree], newFunctionSymbol, translations)
                case Block(statements, e) => 
                    if !isEmpty(statements) then 
                        throw new Error("Pattern is a non-empty block")
                    else
                        handlePattern(e, newFunctionSymbol, translations)
                case Typed(term, typeTree) =>
                    val res = handlePattern(term, newFunctionSymbol, translations)
                    //Typed(res, typeTree)
                    res
                                    
                case Unapply(fun, implicits, pattern) => 
                    if !isEmpty(implicits) then
                        throw new Error(s"handlePattern could not handle unapply which non-empty implicits: ${implicits}")

                    // gives list of parameters, and the corresponding terms to give to the apply
                    def unbindStrings(t : Tree):  List[Term]= t match {
                        case Bind(string, tree) => 
                            tree match {
                                case Ident(name) if name == "_" =>
                                        //TODO check project, maybe bug comes from here. Maybe also look up translation7
                                        val translation = translations.filter(_._1.name == string)
                                        if !translation.isEmpty then
                                            println(s"Translation : $string to ${translation.head}")
                                            List(translation.head._2)
                                        else
                                            println(s"unbindString has a test with ${translations.map(t => t._1.name)}")
                                            //println(s"unbindString has a success with ${translations.map(t => t._1.fieldMembers())}")
                                            List(Ident(TermRef(tree.asInstanceOf[Ident].tpe, string)))
                                case _ : Term =>
                                    //if translations.filter(_._1.memberField())
                                    println(s"unbindStrings Translation is $translations")
                                    println(s"unbindStrings Ident is $pattern with symbol ${pattern.map(_.symbol)}")
                                    //throw new Error(s"unbindString bind with term tree ${tree}")
                                    List(useTranslation(tree.symbol).getOrElse({println("===No symbol found"); Ident(TermRef(tree.asInstanceOf[Term].tpe, string))}))
                                    //Ident(TermRef(tree.asInstanceOf[Term].tpe, string)) :: unbindStrings(tree)
                                case _ => throw new Error(s"unbindString bind with non term tree ${tree}")
                            }
                        case _ => 
                            throw new Error(s"unbindString non-bind with ${t}")
                    }
                    //create function to handle select
                    //Use bindings in pattern to handle this,  Apply(Select apply from the fun , List the identifiers ...)
                    val newFun : Term = fun match {
                        case Select(term, string) => 
                            if string != unapply then 
                                throw new Error(s"handlePattern could not handle unapply with non-apply function selected")
                            val symbols = term.symbol.memberMethod(apply)
                            if isEmpty(symbols) then 
                                throw new Error(s"handlePattern could not find apply method on pattern")
                            Select(term, symbols.head)
                    }
                    //Problem with the terms
                    val terms = pattern.map(unbindStrings(_)).flatten
                    val res = Apply(newFun, terms)
                    println(s"handlePattern Unapply became $res")
                    res
                
                case _ =>
                    println("Pattern: " + pattern.show(using Printer.TreeStructure) )
                    throw new Error(s"handlePattern could not handle ${pattern}")
                //case Ident("_") => 
                    //UnApply(Select(Ident("Foo"),"unapply"),List(),List(Bind("aa",Ident("_"))))
                    //throw new Warning("Function contains wildcard as pattern, bijection may fail with MatchError. Removal of this case is suggested", tree.pos)
                    //TODO
            }

        def handleRhs(term: Term, oldFunctionSymbol: Symbol, newFunctionSymbol : Symbol): (Tree, List[(Symbol, Term)]) ={
            term match{
                case Ident(s) => 
                    //TODO use requests
                    (term, List((term.symbol, term)))
                case Block(statements, e) => 
                    if !isEmpty(statements) then 
                        throw new Error("handleRhs Rhs is a non-empty block")
                    else
                        handleRhs(e, oldFunctionSymbol, newFunctionSymbol)
                case Apply(function, arguments) =>
                    println(s"Apply with function $function and arguments $arguments")
                    
                    def createBinds(term : Term, function : Term) : (Bind, (Symbol, Term)) = term match{
                        case Ident(name) => 
                            println(s"bindTerm is $term")
                            println(s"bindTerm symbol is ${term.symbol}")
                            println(s"bindTerm tree is ${term.tpe}")
                            val newSymbol = Symbol.newBind(term.symbol, name, Flags.EmptyFlags, function.tpe)
                            
                            (Bind(newSymbol, Ident.copy(term)("_")), (term.symbol, term))// Ident(TermRef(term.tpe,"_"))
                        case Apply(function, arguments) => 
                            if function.symbol == oldFunctionSymbol && arguments.size == 1 then 
                                val (bind, (symbol, _)) = createBinds(arguments.head, function)
                                //Ident(TermRef(newFunctionSymbol.tree.asInstanceOf[Term].tpe, newFunctionSymbol.name))
                                val translation = (symbol, Apply(Ident.copy(bind)(newFunctionSymbol.name), arguments))
                                (bind, translation)
                            else
                                throw new Error(s"createBinds in handleRhs could not handle Apply with ${function} and arg ${arguments}")

                        case _ => 
                            throw new Error(s"createBinds in handleRhs could not handle ${term} with symbol ${term.symbol}")
                    }
                    
                    val (classe: Term, newFun : Term) = function match {
                        case Select(term, string) => 
                            if string != apply then 
                                throw new Error(s"handleRhs could not handle apply with non-unapply function selected")
                            val symbols = term.symbol.memberMethod(unapply)
                            if isEmpty(symbols) then 
                                throw new Error(s"handleRhs could not find apply method on term")
                            (term,Select(term, symbols.head))
                    }
                    // Don't directly call create bind
                    val (binds, translations):(List[Bind], List[(Symbol, Term)]) = arguments.map(createBinds(_, classe)).unzip
                    val res = Unapply.copy(term)(newFun, Nil, binds)//(function,Nil, generate binds)
                    println(s"Create Unapply: $res")
                    (res, translations)
                case _ => throw new Error(s"handleRhs could not handle $term")
            }
        }
        //def handleRhs(term: Term)
        val tree : Term = expr.asTerm
        //println(s"First level tree is $tree")
        //println(s"First level typeRepr is ${tree.tpe.simplified}")
        val fun = extractFunction(tree)
        //println(s"Final fun is $fun")
        //treePrint(fun, 0) 
        //'{case $fun($arg) => $term}
        //s"case $fun($arg) => $term"
        //Foo(Foo(aa))
        //Closure needs term, we put Ident (maybe select)
        //Ident needs TermRef (of TypeRepr)
        //TermRef needs (qual: TypeRepr, name: String)
        //println(s"Fun name is ${fun.name}")
        //println(s"Fun typeRepr is ${fun.returnTpt.tpe}")//TermRef(fun.returnTpt.tpe, fun.name) //TermRef(TypeRepr.memberType(fun.symbol), fun.name)
        //Ident.copy(tree)(fun.name)
        //val funBlock = Block(List(fun), Closure(Ident(TermRef(tree.tpe.memberType(fun.symbol), fun.name)), Some(fun.returnTpt.tpe) ))//Some(fun.returnTpt.tpe)
        val funBlock = Block(List(fun), Closure(Ident.copy(tree)(fun.name), Some(fun.returnTpt.tpe) ))//Some(fun.returnTpt.tpe)
        // Maybe in 
        //println(s"funBlock is $funBlock")
        //println(s"Final fun is $fun")
        funBlock.asExpr.asInstanceOf[Expr[B=>A]]
        
            
        //Expr(tree.show(using Printer.TreeStructure))
        //printOnlyDef(tree)
    
    
    }


 
}

    /*
    def printOnlyDef(tree: Tree)(using ctx: Context): List[Symbol] ={
        val acc = new TreeAccumulator[List[String]]{
            def foldTree(s: List[String], tree: Tree)(owner: Symbol): List[String] = 
                tree match
                    case deff : DefDef =>
                        if deff.rhs.isDefined then 
                            List(deff.rhs.get.show(using Printer.TreeStructure))
                        else 
                            List[String]()
                    case _ =>
                        foldOverTree(s, tree)(owner)
            }
        acc.foldTree(Nil, tree)(tree.owner)
    }
        //tree.fol
        //Expr(tree.show(using Printer.TreeStructure))
    
    /*using Printer.TreeStructure
    inline def assert(inline expr: Boolean): Unit =
        ${ assertImpl('expr) }
    
    def assertImpl(expr: Expr[Boolean])(using Quotes) = '{
    if !$expr then
        throw AssertionError(s"failed assertion: ${${ showExpr(expr) }}")
    }
    def collectPatternVariables(tree: Tree)(using ctx: Context): List[String] =
        val shower = new TreeMap[List[String]] = {
            def foldTree
        }
        val acc = new TreeAccumulator[List[Symbol]]{
            def foldTree(s: List[Symbol], tree: Tree)(owner: Symbol): List[Symbol] = 
                tree match
                    case ValDef(_, _, rhs) =>
                        val newSyms = tree.symbol :: syms
                        foldTree(newSyms, body)(tree.symbol)
                    case _ =>
                        foldOverTree(syms, tree)(owner)
            }
        acc(Nil, tree)
        */
without unapply 
    caseDef:
        Typed with TypeTree[TypeRef(ThisType(TypeRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix,module class <root>)),module class <empty>)),module class TreesTest$)),class ALeaf)]:
class dotty.tools.dotc.ast.Trees$UnApply
Unapply(Select(Select(Ident("TreesTest"), "ALeaf"), "unapply"), Nil, List(Bind("a", Ident("_"))))

*/

