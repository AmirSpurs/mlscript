package mlscript.compiler.wasm

import mlscript.compiler.wasm.Type as MachineType

import mlscript.compiler.*
import mlscript.utils.*
import mlscript.utils.shorthands.*
import mlscript.*

import collection.mutable.{Map => MutMap, Set => MutSet, ListBuffer}
import scala.annotation.meta.field
import mlscript.JSCodeHelpers.param
import scala.concurrent.ExecutionContext.parasitic


final case class WasmBackendError(message: String) extends Exception(message)


// TODO: optimize the assumption the all value are boxed
class WasmBackend(

):
  import MachineInstr.*
  import GOExpr.*
  import Node.*

  case class JoinPoint(params: Ls[Name], rhs: Node):
    def passArgs(args: Ls[TrivialExpr]): Ls[MachineInstr] =
      val evalArgs = params.zip(args) flatMap {
        case (Name(paramName), expr) =>
          translateTrivialExpr(expr)
          :+ SetLocal(paramName)
          :+ Comment(s"set ${paramName}")
      }
      evalArgs

    def apply(): Ls[MachineInstr] =
      // store args in a local variable
      rhs |> translateNode

    // TODO: possible inlining optimization
    private def inlineArgs(args: Ls[TrivialExpr]) =
      val numParams = params.size
      val paramNames = params.map(param => param.str).zip(List.range(0, numParams)).toSet
      rhs |> inlineNodeRec

      def inlineRefName(name: Name): Name =
        val str = name.str
        paramNames.collectFirst {
          case (paramName, index) if str.equals(paramName) =>
            args(index) match
              case Ref(Name(argName)) => Name(argName)
              case _ => throw WasmBackendError(s"try to inline a left value ref ${paramName}")
        } getOrElse(name)
      
      def inlineTrivialExpr(expr: TrivialExpr): TrivialExpr = expr match
        case Ref(Name(name)) =>
          paramNames.collectFirst {
            case (paramName, index) if name.equals(paramName) =>
              args(index)
          } getOrElse(expr)
        case _ => expr
      
      def inlineExpr(expr: GOExpr): GOExpr = expr match
        // very hacky and dangerous, I have to admit
        case s: TrivialExpr => inlineTrivialExpr(s).asInstanceOf[GOExpr]
        case CtorApp(cls, args) =>
          CtorApp(cls, args map inlineTrivialExpr)
        case Select(name, cls, field) =>
          Select(inlineRefName(name), cls, field)
        case BasicOp(op, args) =>
          BasicOp(op, args map inlineTrivialExpr)
        case _ => expr

      def inlineNodeRec(node: Node): Node = node match
        case Result(res) =>
          Result(res map inlineTrivialExpr)
        case Jump(joinName, args) =>
          Jump(joinName, args map inlineTrivialExpr)
        case Case(struct, cases) =>
          Case(inlineRefName(struct), cases map {
            case (cls, node) => (cls, node |> inlineNodeRec)
          })
        case LetExpr(name, expr, body) =>
          LetExpr(name, expr |> inlineExpr, body |> inlineNodeRec)
        case LetJoin(joinName, params, rhs, body) =>
          // join point should not ref outside the params
          LetJoin(joinName, params map inlineRefName, rhs, body |> inlineNodeRec)
        case LetCall(resultNames, defn, args, body) =>
          LetCall(resultNames, defn, args map inlineTrivialExpr, body |> inlineNodeRec)

  val functionMap: MutMap[Str, MachineFunction] = MutMap()
  val joinPoints: MutMap[Str, JoinPoint] = MutMap()
  val locals: MutSet[Str] = MutSet()

  protected def translateTrivialExprUnbox(expr: TrivialExpr) = expr match
    case Ref(Name(name)) => Ls(GetLocal(name), I32Load)
    case Literal(lit) => lit match
      case IntLit(value) => Ls(I32Const(value.intValue))
      case _ => throw WasmBackendError(s"unsupported literal ${lit.toString()}!")

  protected def translateTrivialExpr(expr: TrivialExpr): Ls[MachineInstr] = expr match
    case Ref(Name(name)) => Ls(GetLocal(name))
    case Literal(lit) => lit match
      case IntLit(value) => Ls(I32Const(value.intValue))
      case _ => throw WasmBackendError(s"unsupported literal ${lit.toString()}!")

  def translateExpr(expr: GOExpr): Ls[MachineInstr] = expr match
    case s: TrivialExpr => translateTrivialExpr(s)
    case CtorApp(cls, args) =>
      // for ctorApp, we just leave a trace of field values on stack (reverse order)
      val fieldsValues: ListBuffer[MachineInstr] = ListBuffer()
      args.zip(cls.fields) foreach {
        (arg, field) => 
          fieldsValues ++= translateTrivialExpr(arg)
          fieldsValues += Comment(s"${field}")
      }
      fieldsValues += I32Const(cls.id)
      fieldsValues.toList
    case Select(Name(refName), cls, field) =>
      val recordAux = RecordObj.from(cls)
      val offset = recordAux.getFieldOffset(field)
      Ls(GetLocal(refName), I32Const(offset), I32Add, I32Load)
    case BasicOp(operator, args) =>
      val argsInstr = args flatMap {arg => translateTrivialExprUnbox(arg)}
      val opInstr = operator match
        case op if op.equals("+") => I32Add
        case op if op.equals("-") => I32Sub
        case op if op.equals("*") => I32Mul
        case op if op.equals("/") => I32Div
        case op => throw WasmBackendError(s"unknown operator ${op}")
      argsInstr :+ opInstr
    // Lambda and App is deprecated
    case _ => throw WasmBackendError("deprecated GOExpr")
 
  def translateNode(node: Node): Ls[MachineInstr] =
    def translateNodeRec(node: Node, nestedBlocks: Int): Ls[MachineInstr] = node match
      case _ if nestedBlocks > 100 => throw WasmBackendError("too deep rec")
      case LetExpr(Name(name), expr, body) =>
        locals += name
        val exprInstr = allocate(name, expr)
        exprInstr ++ translateNodeRec(body, nestedBlocks)
      case LetJoin(Name(name), params, rhs, body) =>
        // lazy translate
        val jp = JoinPoint(params, rhs)
        joinPoints += name -> jp
        // wrap with a block
        val buffer = ListBuffer.from(translateNodeRec(body, nestedBlocks))
        Block(s"join_point@${name}", Ls()) +=: buffer

        buffer += Comment(s"join point ${name}(${params.map{ x => x.toString() }.mkString(",")})" )
        buffer ++= jp()
        buffer += End
        buffer.toList

      case LetCall(resultNames, defnRef, args, body) =>
        locals ++= resultNames.map(resultName => resultName.str)
        val argsInstr = args flatMap {arg => translateTrivialExpr(arg)}
        val calleeName = defnRef.getName()
        val numResultFields = resultNames.size
        val recordAux = RecordObj.opaque(numResultFields)
        
        // TODO: handle multivalue function result
        // We assume only one return value temporarily
        val bindInstrs = resultNames flatMap { case Name(name) =>
                            Ls(SetLocal(name))
                          }
        Call(calleeName) +: (argsInstr ++ bindInstrs ++ translateNodeRec(body, nestedBlocks))
      
      // terminal form, may have nested node
      case Case(Name(struct), cases) =>
        val numArms = cases.size 
        // We assume classinfo id of arms is continuous
        val caseIds = cases.map {case (cls, _) => cls.id}
        val minId = caseIds.min
        val maxId = caseIds.max
        if List.range(minId, maxId + 1).toSet equals caseIds.toSet then
          val brTable = caseIds map {_ - minId}
          val dispatchInstrs = Ls(Block(s"case_${struct}", Ls()),
                                  GetLocal(struct), 
                                  I32Const(minId), 
                                  I32Sub, 
                                  BrTable(brTable),
                                  Comment("case br table"),
                                  End)
          
          cases.zip(List.range(0, numArms)).foldLeft(dispatchInstrs) {
            case (instrs, ((cls, armNode), index) ) =>
              val depth = nestedBlocks - index
              val armInstrs = translateNodeRec(armNode, depth)
            
              // construct a new block for a arm:
              // (block
              //  (block ...)
              //  instr*  
              // )
              Block(s"match_${depth}", Ls()) +: (instrs ++ armInstrs) :+ End
          }
        else 
          throw WasmBackendError("not implemented cases dispatch")
      // terminal forms, no body
      case Jump(Name(joinName), args) =>
        val joinPoint = joinPoints.getOrElse(joinName, 
                                             throw WasmBackendError(s"join point ${joinName} not found"))
        joinPoint.passArgs(args) :+ Br(nestedBlocks)
        
      case Result(res) =>
        val resultInstrs = res flatMap {expr => translateTrivialExpr(expr)}
        nestedBlocks match
          case 0 => resultInstrs
          case labels => resultInstrs :+ Br(nestedBlocks)
    translateNodeRec(node, 0)

  // translate GODef to a function.
  // after optimization, some join point will be lifted to a GODef,
  // but we should just treat it as a basic block
  def translateGODefs(godef: GODef): Either[MachineFunction, JoinPoint] =
    val name = godef.name
    
    if godef.isjp then
      val jp = JoinPoint(godef.params, godef.body)
      joinPoints += name -> jp
      Right(jp)
    else 
      locals.clear()
      val body = translateNode(godef.body)
      val paramsType = godef.params map { case Name(param) => param -> MachineType.defaultNumType}
      // local info collect when codegen
      val localsType = locals.toList map {local => local -> MachineType.defaultNumType}
      val retType = MachineType.defaultNumType
      Left(
        MachineFunction(
          name,
          paramsType,
          localsType,
          retType,
          body)
        )

  // entry of lowering graph ir to wasm module
  def translate(goprog: GOProgram): Module =
    val main = GODef(
      -1, // invalid id
      "main",
      false,
      Ls(),
      1, // return only one value (may be pointer)
      goprog.main
    )
    val defn = goprog.defs.toList :+ main
    val functions = defn collect {godef =>
      translateGODefs(godef) match
        case Left(function) => function
    }

    // name `Module` shadows Module in java.lang
    wasm.Module(
      "",
      List(), // empty imports
      functions 
    )

  // We treat Wasm memory as a linear memory pool,
  // each time we allocate a class, we move forward the memory pointer (a global variable)
  def allocate(bindName: Str, rhs: GOExpr): Ls[MachineInstr] =
    // Debug only
    val ctorInstrs: ListBuffer[MachineInstr] = ListBuffer()
    val objSize = rhs match
      case CtorApp(cls, _) => RecordObj.from(cls).size
      case _ => MachineType.defaultNumType.size 

    ctorInstrs ++= Ls(GetGlobal(0), SetLocal(bindName))
    ctorInstrs ++= Ls(GetGlobal(0), I32Const(objSize), I32Add, SetGlobal(0))
    ctorInstrs += Comment(s"allocate ${objSize} bytes for ${bindName}")
    // store expr values into linear memory
    rhs match
      case CtorApp(cls, _) =>
        ctorInstrs ++= translateExpr(rhs)
        ctorInstrs ++= Ls(GetLocal(bindName), I32Store)
        // store id and fields
        val recordAux = RecordObj.from(cls)
        cls.fields foreach { fieldName =>
          ctorInstrs ++= Ls(
            GetLocal(fieldName),
            I32Const(recordAux.getFieldOffset(fieldName)),
            I32Add,
            I32Store
          )
        }
        ctorInstrs += Comment(s"CtorApp ${cls.ident}")
      case _ =>
        ctorInstrs ++= translateExpr(rhs)
        ctorInstrs ++= Ls(GetLocal(bindName), I32Store)

    ctorInstrs.toList
 
end WasmBackend