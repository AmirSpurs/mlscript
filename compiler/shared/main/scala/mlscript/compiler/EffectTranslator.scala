package mlscript
package compiler

import mlscript.utils.*
import mlscript.utils.shorthands.*
import scala.collection.mutable.StringBuilder as StringBuilder
import scala.collection.mutable.Map as MutMap
import scala.collection.mutable.Set as MutSet
import scala.collection.mutable.ArrayBuffer as ArrayBuffer
import mlscript.codegen.Helpers.inspect as showStructure
import mlscript.codegen.CodeGenError
import mlscript.compiler.mono.MonomorphError
import scala.collection.immutable.*
import scala.collection.mutable.{HashMap => MutHMap}
import scala.collection.mutable.{HashSet => MutHSet, Set => MutSet}
import scala.collection.mutable.MultiMap


class EffectTranslator(logDebugMsg: Boolean = true) { 
  val StackUpperLimit = GONode.Result(Ls(GOExpr.Literal(IntLit(BigInt(500)))))
  val StackLowerLimit = GONode.Result(Ls(GOExpr.Literal(IntLit(BigInt(250)))))
  private final val ref = x => GOExpr.Ref(x) 
  final val result = x => GONode.Result(x)
  private final val sresult = (x: TrivialExpr) => GONode.Result(List(x))
  
  private val counter = MutHMap[Str, Int]()
  def gensym(s: Str = "e") = {
    val count = counter.getOrElse(s, 0)
    val ts = s.split("%")(0)
    val tmp = s"$s%$count"
    counter.update(s, count + 1)
    Name(tmp)
  }
  type ClassName = String
  type FieldName = String
  val logOutput: StringBuilder = new StringBuilder
  private def log(str: String): Unit = {
    logOutput.append(str)
    if(logDebugMsg){
      println(str)
    }
  }
  def getLog: String = logOutput.toString()

  
//   adding Effect classes and Continuation classes
  def preProcess (srcClasses:Set[ClassInfo]) : Set[ClassInfo] = {
    // print number of memebers in srcClasses
     var len = srcClasses.size
     val newClass = ClassInfo(len,"StackDelay",Nil)
     val newClass2 = ClassInfo(len+1,"Cont",Nil)  
     return srcClasses + newClass + newClass2
    // val newClass = ClassInfo(srcClasses.size(),"StackDelay",Nil)

  }
  def translateCls(srcClass:Set[ClassInfo]) : Set[ClassInfo] = {
    val newClass = preProcess(srcClass)
    return newClass
  }

  def translateNode (srcNode: GONode): GONode = 
    srcNode match {
    case GONode.Result(xs) =>   GONode.Result(xs)
    case GONode.Jump(defnref, args) =>  GONode.Jump(defnref, args)
    case GONode.Case(scrut, cases) =>   GONode.Case(scrut, cases.map((cls, body) => (cls, body |> translateNode)))
    case GONode.LetExpr(name, expr, body) => GONode.LetExpr(name, expr, body |> translateNode)
    case GONode.LetJoin(defnref, params, rhs, body) => GONode.LetJoin(defnref, params, rhs |> translateNode, body |> translateNode)
    // here we add the code to check if the result of function call is continuation or it's the final result
    case GONode.LetCall(xs, defnref, args, body) => 
      val tName = gensym()
      val joinName = gensym()
      val paramName = gensym()
      // right now just fo showing it (should be let call or just handle the cont itself)
      val tru = GONode.Jump(GODefRef(Right(joinName.str)), Ls(GOExpr.Ref(Name("handle"))))
      //Q: should be head? question: why is it a list? why there isn't a wildcard?
      val caseTest = GONode.Case(xs.head, Ls((ClassInfo(0,"StackDelay",Nil), tru), (ClassInfo(0,"_",Nil), body |> translateNode)))
      val joinNode = GONode.LetJoin(joinName, List(paramName), paramName |> ref |> sresult , caseTest)
      return GONode.LetCall(xs, defnref, args, joinNode)
  }

  def translateDef (srcDef: GODef): GODef = {
    // adding the stackLimit test right now stack is just a Re
    //Q: should stackSize be a parameter of the function now? 
    val tName = gensym()
    val joinName = gensym()
    val paramName = gensym()    
    val tru = GONode.Jump(GODefRef(Right(joinName.str)), Ls(GOExpr.Ref(Name("StackDelay"))))
    val caseTest = GONode.Case(tName, Ls((ClassInfo(0,"True",Nil), tru), (ClassInfo(0,"False",Nil), srcDef.body |> translateNode)))
    val testStackLimit = GONode.LetExpr(tName, GOExpr.BasicOp("<",(Ls(GOExpr.Literal(IntLit(BigInt(500))),GOExpr.Ref(Name("stackSzie"))))),caseTest)
    val joinNode = GONode.LetJoin(joinName, List(paramName), paramName |> ref |> sresult , testStackLimit)
    return GODef(srcDef.id, srcDef.name, srcDef.isjp, srcDef.params, srcDef.resultNum, joinNode)
  }
  def translateDefs (srcDefs: Set[GODef]): Set[GODef] = {
    return srcDefs.map(translateDef) 
  }
  def translateMain (srcMain: GONode): GONode = {
    return srcMain |> translateNode
  }
  def translate(src: GOProgram): GOProgram = {
    val classes = translateCls(src.classes)
    val old_defs = translateDefs(src.defs)
    val old_main = translateMain(src.main)
    return GOProgram(classes, old_defs, old_main)
  }
}