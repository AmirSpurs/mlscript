package mlscript.compiler.backend
import mlscript.compiler.backend.algorithms._
import mlscript.compiler.backend.Instruction._
import mlscript.compiler.backend.PureValue._
import mlscript.compiler.backend.Operand._
import mlscript.compiler.backend.wasm.WasmInstructions.{
  Return => WasmReturn,
  Unreachable => WasmUnreachable,
  Call => WasmCall,
  _
}
import scala.collection.mutable.ListBuffer
import mlscript.utils.shorthands.*
import mlscript.codegen.CodeGenError
import mlscript.compiler.backend.wasm.CodePrinter
import mlscript.compiler.backend.wasm.LocalsHandler
import mlscript.compiler.backend.wasm.ModulePrinter
import mlscript.compiler.backend.wasm.Function
import mlscript.SimpleTerm
import scala.language.implicitConversions
import scala.collection.mutable.{ListMap, HashMap, LinkedHashMap}

enum ContainingSyntax:
  case IfThenElse
  case LoopHeadedBy(val label: String)
  case BlockFollowedBy(val label: String)

class Ir2wasm {
  import ContainingSyntax._
  val memoryBoundary = 0

  def translate(
      blocks: Ls[(BasicBlock, Map[Operand.Var, Type], Type)],
      moduleName: String,
      imports: List[String],
      classDefMap: Map[Type.TypeName, (LinkedHashMap[String, Type], Int)],
      recordDefMap: Map[Type.Record, Int]
  ) =
    wasm.Module(
      moduleName.replaceAll("/", "_"),
      imports,
      blocks.map((bb, symbolTypeMap, retType) =>
        val isMain = bb.name == "entry"
        Function(
          if isMain then "main" else bb.name,
          bb.params,
          isMain,
          retType
        ) { lh =>
          val paramsTpe =
            if (bb.params.size == 0)
              Nil
            else
              symbolTypeMap.get(Var(bb.name)) match
                case S(Type.Function(paramTpe, _)) => paramTpe
                case _ => throw CodeGenError(s"unresolved symbol ${bb.name}")
          bb.params
            .zip(paramsTpe)
            .foreach((param, tpe) =>
              lh.register(
                param.name,
                tpe,
                true
              )
            )
          translateFunction(
            bb,
            moduleName,
            imports,
            classDefMap,
            recordDefMap,
            symbolTypeMap,
            lh
          )
        }
      )
    )

  def translateFunction(
      entryBB: BasicBlock,
      moduleName: String,
      imports: List[String],
      classDefMap: Map[Type.TypeName, (LinkedHashMap[String, Type], Int)],
      recordDefMap: Map[Type.Record, Int],
      symbolTypeMap: Map[Operand.Var, Type],
      lh: LocalsHandler
  ): Code =

    var worklist: ListBuffer[BasicBlock] = ListBuffer(entryBB)
    var nodes: ListBuffer[BasicBlock] = ListBuffer()
    var edges: ListBuffer[(BasicBlock, BasicBlock)] = ListBuffer()

    while (worklist.nonEmpty)
      val curr = worklist.head
      worklist = worklist.tail
      if (!nodes.contains(curr))
        edges ++= curr.children.map(child => (curr, child))
        nodes += curr
        worklist ++= curr.children.filterNot(nodes.contains)

    val isLoopHeader = getLoopHeader(edges.toList, nodes.toList)
    val isMergedNode = getMergedNode(edges.toList, nodes.toList)
    val getIndex = reversePostOrder(edges.toList, nodes.toList)
    val getChildern = getImDomChild(edges.toList, nodes.toList)

    def doTree(source: BasicBlock)(implicit
        context: List[ContainingSyntax],
        lh: LocalsHandler
    ): Code =
      val children = getChildern(source)
      val mergedChildren: List[BasicBlock] = children.filter(isMergedNode)
      if (isLoopHeader(source))
        Loop(source.name) <:> nodeWithin(source, mergedChildren)(
          ContainingSyntax.LoopHeadedBy(source.name) +: context,
          lh
        ) <:> End
      else
        nodeWithin(source, mergedChildren)

    def nodeWithin(
        source: BasicBlock,
        children: List[BasicBlock]
    )(implicit
        context: List[ContainingSyntax],
        lh: LocalsHandler
    ): Code = children match
      case node :: rest =>
        Block(
          source.name,
          source.params.map(_ match
            case Var(name) => name
          )
        ) <:> nodeWithin(source, rest)(
          LoopHeadedBy(node.name) +: context,
          lh
        ) <:> End <:> doTree(node)
      case Nil =>
        instrToWasm(source.instructions.toList) <:>
          (source.leaving match
              case S(Branch(target, args)) =>
                doBranch(source, target, args)
              case S(Match(value, cases, default)) =>
                if (cases.size == 1)
                  val (key, (thenBlock, thenArgs)) = cases.head
                  val thenlh = LocalsHandler(lh)
                  (value, key) match
                    case (Operand.Var(op), mlscript.Var(name)) =>
                      // TODO better class shadowing
                      // TODO do it in multi branches case
                      if (classDefMap.keys.exists(Type.TypeName(name) == _))
                        thenlh.register(
                          op,
                          Type.TypeName(name),
                          lh.getIsParam(op)
                        )
                    case _ => ()
                  val eqInstr = key.getType(symbolTypeMap) match
                    case Type.Int64 => I64Eq
                    case _ => I32Eq
                  operandToWasm(value) <:> getLoad(value) <:> operandToWasm(
                    key
                  ) <:> eqInstr <:>
                    If_void <:> doBranch(source, thenBlock, thenArgs)(
                      context,
                      thenlh
                    ) <:>
                    Else <:> doBranch(source, default._1, default._2) <:> End
                else
                  val casesList = cases.toList
                  Block(default._1.name, Nil) <:>
                    translateCases(
                      value,
                      casesList.map(_._1),
                      casesList.map(_._2),
                      default._1.name
                    ) <:>
                    doTree(default._1)
                    <:> End
              case S(Return(S(Var(op))))           => Code(List(GetLocal(op)))
              case S(Return(S(op: Operand.Const))) => operandToWasm(op)
              case S(Unreachable) => Code(List(WasmUnreachable))
              case _              => Code(Nil)
            // TODO
          )

    def translateCases(
        value: Operand,
        conditions: List[SimpleTerm],
        cases: List[(BasicBlock, List[Operand])],
        default: String
    )(implicit
        context: List[ContainingSyntax],
        lh: LocalsHandler
    ): Code =
      cases match
        case head :: tail =>
          Block(head._1.name, Nil) <:> translateCases(
            value,
            conditions,
            tail,
            default
          ) <:>
            doTree(head._1) <:>
            Br(default) <:>
            End
        case Nil =>
          val comparison: List[Code] =
            conditions.zipWithIndex.map((cond, idx) =>
              val eqInstr = cond.getType(symbolTypeMap) match
                case Type.Int64 => I64Eq
                case _ => I32Eq
              operandToWasm(value) <:> getLoad(value) <:>
                operandToWasm(cond) <:>
                eqInstr <:>
                I32Const(idx + 1) <:>
                I32Mul
            )
          Block(s"Match_$value", Nil) <:>
            comparison.head <:>
            comparison.tail.map(_ <:> I32Add) <:>
            BrTable(conditions.length) <:>
            End

    def doBranch(
        source: BasicBlock,
        target: BasicBlock,
        args: List[Operand]
    )(implicit
        context: List[ContainingSyntax],
        lh: LocalsHandler
    ): Code =
      if (getIndex(source) >= getIndex(target) || isMergedNode(target))
        (target.params zip args).map((param, arg) =>
          lh.register(param.name, arg.getType(symbolTypeMap))
          operandToWasm(arg) <:> SetLocal(param.name)
        )
      else
        doTree(target)

    def instrToWasm(instrs: List[Instruction])(implicit
        lh: LocalsHandler
    ): Code = {
      instrs.headOption match
        case S(Assignment(lhs, rhs)) =>
          val head: Code = rhs match
            case Alloc(tpe) =>
              lh.register(lhs.name, tpe)
              GetGlobal(memoryBoundary) <:> SetLocal(
                lhs.name
              ) <:> pureValueToWasm(rhs)
            case Op(op) =>
              lh.register(lhs.name, op.getType(symbolTypeMap))
              pureValueToWasm(rhs) <:> SetLocal(lhs.name)
            case BinOp(kind, operand, _) =>
              val tpe = kind match
                case BinOpKind.Eq => Type.Boolean
                case BinOpKind.Ne => Type.Boolean
                case BinOpKind.Lt => Type.Boolean
                case BinOpKind.Le => Type.Boolean
                case _            => operand.getType(symbolTypeMap)
              lh.register(lhs.name, tpe)
              pureValueToWasm(rhs) <:> SetLocal(lhs.name)
            case _ =>
              lh.register(lhs.name, lhs.getType(symbolTypeMap))
              pureValueToWasm(rhs) <:> SetLocal(lhs.name)
          head <:> instrToWasm(instrs.tail)
        case S(_: Branch)   => Code(Nil)
        case S(Unreachable) => Code(Nil)
        case S(_: Match)    => Code(Nil)
        case S(_: Return)   => Code(Nil)
        case S(Call(result, Var("log"), args)) =>
          lh.register(result.get.name, Type.Unit)
          val resultCode: Code = I32Const(0)
          val typeCode: Code = args.head.getType(symbolTypeMap) match
            case Type.Unit           => I32Const(0)
            case Type.Boolean        => I32Const(1)
            case Type.Int64          => Code(Nil)
            case Type.Float64        => Code(Nil)
            case Type.OpaquePointer  => I32Const(4)
            case Type.Record(_)      => I32Const(5)
            case Type.Variant(_)     => I32Const(6)
            case Type.Function(_, _) => I32Const(7)
            case Type.TypeName(_)    => I32Const(8)
          val funName = args.head.getType(symbolTypeMap) match
            case Type.Float64 => "logF64"
            case Type.Int64   => "logI64"
            case _            => "logI32"
          args.map(operandToWasm) <:>
            typeCode <:>
            WasmCall(funName) <:>
            resultCode <:>
            SetLocal(result.get.name) <:>
            instrToWasm(instrs.tail)
        case S(Call(result, Var(name), args)) =>
          symbolTypeMap(Var(name)) match
            case Type.Function(_, ret) =>
              lh.register(result.get.name, ret)
              val resultCode: Code = symbolTypeMap.get(Var(name)) match
                case S(Type.Function(_, _)) =>
                  Code(Nil)
                case S(_) => ???
                case N    => I32Const(0)
              args.map(operandToWasm) <:> WasmCall(name) <:>
                resultCode <:> SetLocal(result.get.name) <:>
                instrToWasm(instrs.tail)
            case _ => throw CodeGenError(s"Undefined function $name")
        case S(SetField(obj, field, value)) =>
          val offset = lh.getType(obj.name) match //TODO i64 should be 8
            case tpe: Type.TypeName =>
              val fields = classDefMap(tpe)._1
              val idx = fields.toList.indexWhere(_._1 == field)
              4 + fields.take(idx).map{
                case (_,Type.Int64) => 8
                case _ => 4
              }.sum
            case Type.Record(recObj) =>
              val fields = recObj.fields
              val idx = fields.toList.indexWhere(_._1 == field)
              4 + fields.take(idx).map{
                case (_,Type.Int64) => 8
                case _ => 4
              }.sum
            case _ => ???
          val store = value.getType(symbolTypeMap) match
            case Type.Int64 => I64Store
            case _ => I32Store
          GetLocal(obj.name) <:>
            I32Const(offset) <:>
            I32Add <:>
            operandToWasm(value) <:>
            store <:>
            instrToWasm(instrs.tail)
        case S(instr) =>
          ??? // TODO
        case N => Code(Nil)
    }

    def pureValueToWasm(pureValue: PureValue)(implicit
        lh: LocalsHandler
    ): Code = pureValue match
      case Op(op) => operandToWasm(op)
      case Alloc(tpe: Type.TypeName) =>
        val (args, classId) = classDefMap(tpe)
        val offset = 4 + args.map{
          case (_,Type.Int64) => 8
          case _ => 4
        }.sum
        GetGlobal(memoryBoundary)
          <:> I32Const(classId)
          <:> I32Store
          <:> GetGlobal(memoryBoundary)
          <:> I32Const(offset)
          <:> I32Add
          <:> SetGlobal(memoryBoundary)
      case Alloc(obj: Type.Record) =>
        val offset = 4 + obj.impl.fields.map{
          case (_,Type.Int64) => 8
          case _ => 4
        }.sum
        GetGlobal(memoryBoundary)
          <:> I32Const(recordDefMap(obj))
          <:> I32Store
          <:> GetGlobal(memoryBoundary)
          <:> I32Const(offset)
          <:> I32Add
          <:> SetGlobal(memoryBoundary)
      case Alloc(_) => ???
      case BinOp(kind, lhs, rhs) =>
        val op: Code = (kind, lhs.getType(symbolTypeMap)) match
          case (BinOpKind.Add, Type.Float64) => F64Add
          case (BinOpKind.Add, Type.Int64)   => I64Add
          case (BinOpKind.Add, _)            => I32Add
          case (BinOpKind.Sub, Type.Float64) => F64Sub
          case (BinOpKind.Sub, Type.Int64) => I64Sub
          case (BinOpKind.Sub, _)            => I32Sub
          case (BinOpKind.Mul, Type.Float64) => F64Mul
          case (BinOpKind.Mul, Type.Int64) => I64Mul
          case (BinOpKind.Mul, _)            => I32Mul
          case (BinOpKind.Div, Type.Float64) => F64Div
          case (BinOpKind.Div, _)            => I64Div
          case (BinOpKind.Rem, _)            => I64Rem
          case (BinOpKind.And, Type.Float64) => F64And
          case (BinOpKind.And, _)            => I32And
          case (BinOpKind.Or, Type.Float64)  => F64Or
          case (BinOpKind.Or, _)             => I32Or
          case (BinOpKind.Xor, Type.Float64) => ???
          case (BinOpKind.Xor, _)            => ???
          case (BinOpKind.Eq, Type.Float64)  => F64Eq
          case (BinOpKind.Eq, Type.Int64)    => I64Eq
          case (BinOpKind.Eq, _)             => I32Eq
          case (BinOpKind.Ne, Type.Float64)  => ???
          case (BinOpKind.Ne, _)             => ???
          case (BinOpKind.Lt, Type.Float64)  => F64Lt_s
          case (BinOpKind.Lt, _)             => I64Lt_s
          case (BinOpKind.Le, Type.Float64)  => F64Le_s
          case (BinOpKind.Le, _)             => I64Le_s
        operandToWasm(lhs) <:> operandToWasm(rhs) <:> op
      case Neg(value) => ???
      case GetField(obj, field) =>
        val (name, tpe) = obj match
          case Var(str) => (str, lh.getType(str))
          case _        => ??? // TODO record type
        val (offset,load) = tpe match
          case tpe: Type.TypeName =>
            val fields = classDefMap(tpe)._1
            val idx = fields.keys.toList.indexOf(field)
            val offset = 4 + fields.take(idx).map{
              case (_,Type.Int64) => 8
              case _ => 4
            }.sum
            val load = fields.get(field) match
              case S(Type.Int64) => I64Load
              case _ => I32Load
            (offset,load)
          case Type.Record(recObj) =>
            val fields = recObj.fields
            val idx = fields.keys.toList.indexOf(field)
            val offset = 4 + fields.take(idx).map{
              case (_,Type.Int64) => 8
              case _ => 4
            }.sum
            val load = fields.get(field) match
              case S(Type.Int64) => I64Load
              case _ => I32Load
            (offset,load)
          case _ => ???
        GetLocal(name) <:> I32Const(offset) <:> I32Add <:> load
      case IsType(obj, ty)         => ???
      case Cast(obj, ty)           => operandToWasm(obj)
      case IsVariant(obj, variant) => ???
      case AsVariant(obj, variant) => ???

    implicit def simpleTerm2Operand(term: SimpleTerm): Operand = term match
      case mlscript.IntLit(v)    => Operand.Const(v.toInt)
      case mlscript.DecLit(v)    => Operand.Const(v.toFloat)
      case mlscript.StrLit(v)    => Operand.Const(v)
      case mlscript.UnitLit(v)   => Operand.Unit
      case mlscript.Var("true")  => Operand.Const(true)
      case mlscript.Var("false") => Operand.Const(false)
      case mlscript.Var(name)    => Operand.Var(name)

    def operandToWasm(op: Operand): Code = op match
      case Const(value: Boolean) =>
        I32Const(if (value) 1 else 0)
      case Const(value: Int) =>
        I64Const(value)
      case Const(value: Float) =>
        F64Const(value)
      case Const(value: String) =>
        mkString(value)
      case Unit =>
        I32Const(0)
      case Var(name) =>
        if (classDefMap.keys.exists(_.name == name))
          I32Const(classDefMap(Type.TypeName(name))._2)
        else
          GetLocal(name)

    def getLoad(op: Operand)(implicit lh: LocalsHandler): Code = op match
      case Var(name) =>
        lh.getType(name) match
          case Type.TypeName(_) => I32Load
          case _                => Code(Nil)
      case _ => Code(Nil)

    def mkString(s: String): Code =
      val size = s.length
      val padding = 4 - size % 4

      val completeS = s + 0.toChar.toString * padding

      val setChars: Code =
        for ((c, ind) <- completeS.zipWithIndex.toList) yield {
          GetGlobal(memoryBoundary) <:> I32Const(ind) <:> I32Add <:>
            I32Const(c.toInt) <:> Store8
        }

      val setMemory: Code =
        GetGlobal(memoryBoundary) <:> GetGlobal(memoryBoundary) <:> I32Const(
          size + padding
        ) <:> I32Add <:>
          SetGlobal(memoryBoundary)

      setChars <:> setMemory

    doTree(entryBB)(Nil, lh)
}