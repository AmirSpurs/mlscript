package mlscript.compiler.wasm

import mlscript.compiler.wasm.Type as MachineType

import mlscript.compiler.*
import mlscript.utils.*
import mlscript.utils.shorthands.*
import mlscript.*

import collection.mutable.{Map => MutMap, Set => MutSet, ListBuffer}


final case class WasmBackendError(message: String) extends Exception(message)


// TODO: optimize the assumption the all value are boxed
class WasmBackend(

):
  import MachineInstr.*
  import GOExpr.*
  import GONode.*

  case class JoinPoint(
    params: Ls[Name], 
    rhs: GONode, 
    needPassArg: Bool = true
  ):
    def passArgs(args: Ls[TrivialExpr]): Ls[MachineInstr] =
      if needPassArg then
        val evalArgs = params.zip(args) flatMap {
          case (Name(paramName), expr) =>
            translateTrivialExpr(expr)
            :+ SetLocal(paramName)
            :+ Comment(s"set ${paramName}")
        }
        evalArgs
      else
        Ls()

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

      def inlineNodeRec(node: GONode): GONode = node match
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
  
  val joinPoints: MutMap[Str, JoinPoint] = MutMap()
  var locals: Set[Str] = Set()
  // classinfo context
  var clsctx: Map[Str, ClassInfo] = Map()
  // top level def context
  var fDefCtx: Map[Int, GODef] = Map()
  // TODO: some functions are hard to encode in graph-ir,
  // e.g. class constructors, we manully generate wasm code.
  var blackMagicFnCtx: Map[Str, MachineFunction] = Map()

  def nameMangling(clsName: Str, methodName: Str) =
    s"_Z${clsName}_${methodName}"
  
  protected def promoteMethod(cls: ClassInfo, mdef: GODef) =
    val id = mdef.id
    val name = mdef.name
    val isjp = mdef.isjp
    val params = mdef.params
    val resultNum = mdef.resultNum
    val body = mdef.body
    GODef(
      id,
      nameMangling(cls.ident, name),
      false,
      Name("this") +: params,
      resultNum,
      body
    )

  protected def preprocess(goprog: GOProgram): Unit =
    val classes = goprog.classes
    val defs = goprog.defs
    this.clsctx = this.clsctx ++ classes.map(cls => (cls.ident -> cls)).toMap
    this.fDefCtx = this.fDefCtx ++ defs.map(fdef => (fdef.id -> fdef)).toMap
    // promote methods

    val methods = classes.toList.flatMap(
      cls => cls.methods.values.map(mdef => promoteMethod(cls, mdef)))
    this.fDefCtx = this.fDefCtx ++ methods.map(mdef => (mdef.id -> mdef))

    val main = GODef(
      -1, // invalid id
      "main",
      false,
      Ls(),
      1, // return only one value (may be pointer)
      goprog.main
    )
    this.fDefCtx = this.fDefCtx.updated(-1, main)
    RecordObj.clearCache()

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

  protected def translateExpr(expr: GOExpr): Ls[MachineInstr] = expr match
    case s: TrivialExpr => translateTrivialExpr(s)
    case CtorApp(cls, args) =>
      throw WasmBackendError(s"unexpected CtorApp handling for ${expr}")
    case Select(Name(refName), scls, field) =>
      // ad hoc, as select method is built before getClassInfoUniverse
      val cls = clsctx(scls.ident)
      val recordAux = RecordObj.from(clsctx, cls)
      val offset = recordAux.getFieldOffset(field)
      Ls(GetLocal(refName), I32Const(offset), I32Add, I32Load)
    case SelectMember(Name(refName), scls, member, args) =>
      val cls = clsctx(scls.ident)
      val layoutAux = RecordObj.from(clsctx, cls)
      layoutAux.getMemberUniverse(member) match
        case L(method) =>
          val mdef = fDefCtx(method.fid)
          val symbolName = mdef.name
          val argsInstr = GetLocal(refName) +: // `this` pointer
                          args.flatMap(arg => translateTrivialExpr(arg))
          argsInstr ++ Ls(Call(symbolName), 
                          Comment(s"call method ${member} of object ${refName}"))
        case R(offset) => 
          Ls(GetLocal(refName), I32Const(offset), I32Add, I32Load, 
             Comment(s"select field ${member} of object ${refName}"))
      
    case BasicOp(operator, args) =>
      val argsInstr = args flatMap {arg => translateTrivialExpr(arg)}
      val opInstr = operator match
        case op if op.equals("+") => I32Add
        case op if op.equals("-") => I32Sub
        case op if op.equals("*") => I32Mul
        case op if op.equals("/") => I32Div
        case op => throw WasmBackendError(s"unknown operator ${op}")
      argsInstr :+ opInstr
    // Lambda and App is deprecated
    case expr @ _ => throw WasmBackendError(s"unsupported GOExpr $expr")
 
  protected def translateNode(node: GONode): Ls[MachineInstr] =
    def translateNodeRec(node: GONode, nestedBlocks: Int): Ls[MachineInstr] = node match
      case LetExpr(Name(name), expr, body) =>
        this.locals = this.locals + name
        val exprInstr = allocate(name, expr)
        exprInstr ++ translateNodeRec(body, nestedBlocks)
      case LetJoin(Name(name), params, rhs, body) =>
        // lazy translate
        val jp = JoinPoint(params, rhs)
        joinPoints += name -> jp
        // wrap with a block
        val buffer = ListBuffer.from(translateNodeRec(body, nestedBlocks))

        // Wasm requires offer block type, so we give up wrap join point with block temporarily
        buffer += Comment(s"join point ${name}(${params.map{ x => x.toString() }.mkString(",")})" )
        buffer ++= jp()
        buffer.toList

      case LetCall(resultNames, defnRef, args, body) =>
        this.locals = this.locals ++ resultNames.map(resultName => resultName.str)
        // leftmost param in the bottom
        val argsInstr = args flatMap {arg => translateTrivialExpr(arg)}
        val calleeName = defnRef.getName
        val numResultFields = resultNames.size
        val recordAux = RecordObj.opaque(numResultFields)
        
        // left most result in the bottom
        val bindInstrs = resultNames.reverse flatMap { case Name(name) =>
                            Ls(SetLocal(name))
                          }
        (argsInstr :+ Call(calleeName)) ++ (bindInstrs ++ translateNodeRec(body, nestedBlocks))
      
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
                                  I32Load,
                                  I32Const(minId), 
                                  I32Sub, 
                                  BrTable(brTable),
                                  Comment("case br table"),
                                  End)
          
          cases.zip(List.range(0, numArms)).foldLeft(dispatchInstrs) {
            case (instrs, ((cls, armNode), index) ) =>
              // depth show number of blocks outside current arm (excluding self)
              val depth = nestedBlocks + (numArms - index) - 1
              val armInstrs = translateNodeRec(armNode, depth)
            
              // construct a new block for a arm:
              // (block
              //  (block ...)
              //  instr*  
              // )
              Block(s"match_${index}", Ls()) +: (instrs ++ armInstrs) :+ End
          }
        else 
          throw WasmBackendError("not implemented cases dispatch")
      // terminal forms, no body
      case Jump(defnRef, args) =>
        val joinName = defnRef.getName
        val joinPoint = joinPoints.getOrElse(joinName, 
                                             throw WasmBackendError(s"join point ${joinName} not found"))
        this.locals = this.locals ++ joinPoint.params.map { paramName => paramName.str }
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
  protected def translateGODefs(godef: GODef): Either[MachineFunction, JoinPoint] =
    val name = godef.name
    
    if godef.isjp then
      val jp = JoinPoint(godef.params, godef.body)
      joinPoints += name -> jp
      Right(jp)
    else 
      this.locals = Set()
      val body = translateNode(godef.body)
      val paramsType = godef.params map { case Name(param) => param -> MachineType.defaultNumType}
      // local info collect when codegen
      val localsType = locals.toList map {local => local -> MachineType.defaultNumType}
      val retType = List.fill(godef.resultNum)(MachineType.defaultNumType)
      Left(
        MachineFunction(
          name,
          paramsType,
          localsType,
          retType,
          body)
        )

  // entry of lowering graph ir to wasm module
  def translate(goprog: GOProgram, moduleName: Str): Module =
    preprocess(goprog)
    val functions = fDefCtx.values.toList collect {godef =>
      translateGODefs(godef) match
        case Left(function) => function
    }
    val synthFunctions = blackMagicFnCtx.values.toList

    // name `Module` shadows Module in java.lang
    wasm.Module(
      moduleName,
      List(), // empty imports
      functions ++ synthFunctions
    )
  
  private def getOrGenerateCtor(cls: ClassInfo): MachineFunction =
    val ctorName = nameMangling(cls.ident, "constructor")
    if blackMagicFnCtx.contains(ctorName) then
      blackMagicFnCtx(ctorName)
    else 
      val bkupLocals = this.locals
      this.locals = Set()

      val params = cls.fields
      val fields = cls.members
      val parents = cls.parents

      val layoutAux = RecordObj.from(clsctx, cls)
      val instrs = ListBuffer[MachineInstr]()
      // construct parent
      val pc = parents flatMap {
        case (name, ctor) => 
          val pcls = clsctx(name)
          val poffset = layoutAux.getParentOffet(pcls.ident)
          val pthis = Ls(GetLocal("this"), I32Const(poffset), I32Add)
          val pargs = ctor match
            case S(argNode) => translateNode(argNode)
            case None => Ls()
          val pctorName = nameMangling(name, "constructor")
          val pctor = getOrGenerateCtor(pcls)
          pthis ++ pargs ++ Ls(Call(pctorName), Comment(f"ctor parent ${pcls.ident}"))
      }
      // store params
      val paramc = params flatMap { paramName =>
        Ls(
          GetLocal("this"),
          I32Const(layoutAux.getFieldOffset(paramName)),
          I32Add,
          GetLocal(paramName),
          I32Store,
          Comment(s"set param ${paramName}")
        )
      }

      val fc = fields flatMap { case (fieldName, initNode) =>
        Ls(
          GetLocal("this"),
          I32Const(layoutAux.getFieldOffset(fieldName)),
          I32Add, 
        ) ++
        translateNode(initNode) ++
        Ls(
          I32Store,
          Comment(s"set field ${fieldName}")
        )
      }

      val paramsType = ("this" +: params) map { param => param -> MachineType.defaultNumType}
      // local info collect when codegen
      val localsType = locals.toList map {local => local -> MachineType.defaultNumType}
      val retType = Ls()
      val machineF = MachineFunction(
        ctorName,
        paramsType,
        localsType,
        retType,
        (pc ++ paramc ++ fc).toList
      )
      this.blackMagicFnCtx = this.blackMagicFnCtx.updated(ctorName, machineF)
      // TODO: use better way to recover locals
      this.locals = bkupLocals
      machineF

  // We treat Wasm memory as a linear memory pool,
  // each time we allocate a class, we move forward the memory pointer (a global variable)
  private def allocate(bindName: Str, rhs: GOExpr): Ls[MachineInstr] =
    // Debug only
    val ctorInstrs: ListBuffer[MachineInstr] = ListBuffer()
    // store expr values into linear memory
    rhs match
      case CtorApp(scls, args) =>
        val cls = clsctx(scls.ident)
        val recordAux = RecordObj.from(clsctx, cls)
        val objSize = recordAux.size
        ctorInstrs ++= Ls(GetGlobal(0), SetLocal(bindName))
        ctorInstrs ++= Ls(GetGlobal(0), I32Const(objSize), I32Add, SetGlobal(0))
        ctorInstrs += Comment(s"allocate ${objSize} bytes for ${bindName}")
        // class id
        ctorInstrs ++= Ls(GetLocal(bindName), I32Const(cls.id), I32Store)
        
        val ctorF = getOrGenerateCtor(cls)
        ctorInstrs ++= Ls(GetLocal(bindName)) // `this` pointer
        ctorInstrs ++= args flatMap {
            arg => translateTrivialExpr(arg)
        }
        ctorInstrs += Call(ctorF.name)

        ctorInstrs += Comment(s"CtorApp ${cls.ident}")
      case _ =>
        ctorInstrs ++= translateExpr(rhs)
        ctorInstrs += SetLocal(bindName)
        ctorInstrs += Comment(s"bind variable ${bindName}")

    ctorInstrs.toList
 
end WasmBackend