package mlscript
package compiler.backend

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer, HashMap}
import scala.collection.mutable.{Set => MutSet, Map => MutMap}
import mlscript.codegen._
import mlscript.utils.shorthands.*
import mlscript.utils.*
import mlscript.codegen.Helpers.*
import mlscript.Term
import scala.collection.mutable.{ListMap, LinkedHashMap}

class Mls2ir {
  val visitedSymbols = MutSet[RuntimeSymbol]()
  val classDefMap = HashMap[Type.TypeName, LinkedHashMap[String, Type]]()
  val recordDef = ListBuffer[Type.Record]()
  val scope = Scope("root")
  val varMap = HashMap[Str, Operand.Var]()
  val entryBB = BasicBlock("entry", Nil, ListBuffer())
  val entrySymbolTypeMap = HashMap[Operand.Var, Type]()
  var bb = entryBB
  var symbolTypeMap = HashMap[Operand.Var, Type]()
  var functionList = ListBuffer[(BasicBlock, Map[Operand.Var, Type], Type)]()
  var imports = ListBuffer[String]() // TODO handle MLscript buildin function

  def makeCall(name: Str, fn: Operand, args: List[Operand])(implicit
      scope: Scope
  ): Operand =
    val result: Operand.Var = Operand.Var(scope.allocateRuntimeName(name))
    bb.instructions += Instruction.Call(Some(result), fn, args)
    val returnTpe = fn match
      case Operand.Var("log") => Type.Unit
      case _ =>
        fn.getType(symbolTypeMap.toMap) match
          case Type.Function(_, ret) => ret
          case _                     => ???
    symbolTypeMap += result -> returnTpe
    result

  def addTmpVar(name: Str, value: PureValue)(implicit
      scope: Scope
  ): Operand.Var =
    scope.declareValue(name, Some(false), false) // FIXME byvaluerec, islambda
    val result: Operand.Var = Operand.Var(scope.allocateRuntimeName)
    varMap += name -> result
    bb.instructions += Instruction.Assignment(result, value)
    // TODO getType in ir.scala
    val tpe: Type = value match
      case PureValue.Op(op)    => op.getType(symbolTypeMap.toMap)
      case PureValue.Alloc(ty) => ty
      case PureValue.BinOp(kind, lhs, _) =>
        kind match
          case BinOpKind.Eq => Type.Boolean
          case BinOpKind.Ne => Type.Boolean
          case BinOpKind.Lt => Type.Boolean
          case BinOpKind.Le => Type.Boolean
          case _            => lhs.getType(symbolTypeMap.toMap)
      case PureValue.Neg(value) => value.getType(symbolTypeMap.toMap)
      case PureValue.GetField(obj, field) =>
        obj.getType(symbolTypeMap.toMap) match
          case tpNme: Type.TypeName =>
            classDefMap(tpNme).get(field) match
              case S(tpe) => tpe
              case N      => ??? // TODO handle casting
          case _ => ???
      case PureValue.IsType(_, ty)         => Type.Boolean
      case PureValue.Cast(_, ty)           => ty
      case PureValue.IsVariant(_, variant) => Type.Boolean
      case PureValue.AsVariant(_, variant) => ???
    symbolTypeMap += result -> tpe
    result

  def addTmpVar(value: PureValue)(implicit scope: Scope): Operand.Var =
    val result: Operand.Var = Operand.Var(scope.allocateRuntimeName)
    // TODO Handle scoping
    val tpe = value match
      case PureValue.Op(op)    => op.getType(symbolTypeMap.toMap)
      case PureValue.Alloc(ty) => ty
      case PureValue.BinOp(kind, lhs, _) =>
        kind match
          case BinOpKind.Eq => Type.Boolean
          case BinOpKind.Ne => Type.Boolean
          case BinOpKind.Lt => Type.Boolean
          case BinOpKind.Le => Type.Boolean
          case _            => lhs.getType(symbolTypeMap.toMap)
      case PureValue.Neg(value) => value.getType(symbolTypeMap.toMap)
      case PureValue.GetField(obj, field) =>
        obj.getType(symbolTypeMap.toMap) match
          case tpNme: Type.TypeName =>
            classDefMap(tpNme).get(field) match
              case S(tpe) => tpe
              case N      => Type.Unit // TODO handle casting
          case record: Type.Record =>
            record.impl(field) match
              case S(tpe) => tpe
              case N      => Type.Unit // TODO handle casting
          case _ => ???
      case PureValue.IsType(_, ty)         => Type.Boolean
      case PureValue.Cast(_, ty)           => ty
      case PureValue.IsVariant(_, variant) => Type.Boolean
      case PureValue.AsVariant(_, variant) => ???
    symbolTypeMap += result -> tpe
    bb.instructions += Instruction.Assignment(result, value)
    result

  def translateVar(name: Str, isCallee: Bool)(implicit scope: Scope): Operand =
    scope.resolveValue(name) match
      case S(sym: BuiltinSymbol) =>
        name match
          case "true"  => Operand.Const(true)
          case "false" => Operand.Const(false)
          case "log" =>
            if (!imports.contains("log"))
              imports += name
            Operand.Var("log")
          case _ => ??? // TODO: How should we deal with the remaining builtins?
      case S(sym: StubValueSymbol) =>
        if sym.accessible then varMap(sym.runtimeName)
        else throw new UnimplementedError(sym)
      case S(sym: ValueSymbol) =>
        if sym.isByvalueRec.getOrElse(false) && !sym.isLam then
          throw CodeGenError(
            s"unguarded recursive use of by-value binding name"
          )
        visitedSymbols += sym
        val op = varMap(sym.runtimeName)
        if sym.isByvalueRec.isEmpty && !sym.isLam then
          makeCall(s"${sym.runtimeName}_result", op, Nil)
        else op
      case S(sym: MixinSymbol)    => varMap(sym.runtimeName)
      case S(sym: ModuleSymbol)   => varMap(sym.runtimeName)
      case S(sym: NewClassSymbol) => varMap(sym.runtimeName)
      case S(sym: NewClassMemberSymbol) =>
        if sym.isByvalueRec.getOrElse(false) && !sym.isLam then
          throw CodeGenError(
            s"unguarded recursive use of by-value binding $name"
          )
        val selfSym = scope.resolveValue("this").get
        visitedSymbols += selfSym
        val self = varMap(selfSym.runtimeName)
        if !symbolTypeMap.contains(Operand.Var(selfSym.runtimeName)) then
          // FIXME: add cast if needed
          ???
        val result = addTmpVar(
          s"this_${sym.runtimeName}",
          PureValue.GetField(self, sym.runtimeName)
        )
        if sym.isByvalueRec.isEmpty && !sym.isLam then
          makeCall(s"this_${sym.runtimeName}_result", result, Nil)
        else result
      case S(sym: ClassSymbol) =>
        if isCallee then
          addTmpVar(
            s"new_${sym.runtimeName}",
            PureValue.Alloc(symbolTypeMap(Operand.Var(sym.runtimeName)))
          )
        else ??? // FIXME: lifter should handle lambda?
      case S(sym: TraitSymbol)    => ??? // unimplemented for now
      case S(sym: CapturedSymbol) => ??? // unimplemented for now
      case N => throw CodeGenError(s"unresolved symbol ${name}")

  def translateApp(term: App)(implicit scope: Scope): Operand = term match
    case App(
          App(Var(op), Tup((N -> Fld(_, _, lhs)) :: Nil)),
          Tup((N -> Fld(_, _, rhs)) :: Nil)
        ) if BinOpKind.getBinOp(op).isDefined =>
      val lhsOp = translateTerm(lhs)
      val rhsOp = translateTerm(rhs)
      addTmpVar(PureValue.BinOp(BinOpKind.getBinOp(op).get, lhsOp, rhsOp))
    case App(
          App(
            App(Var("if"), Tup((_, Fld(_, _, tst)) :: Nil)),
            Tup((_, Fld(_, _, con)) :: Nil)
          ),
          Tup((_, Fld(_, _, alt)) :: Nil)
        ) =>
      val tstOp = translateTerm(tst)
      val result: Operand.Var = Operand.Var(scope.allocateRuntimeName)
      val trueBB =
        BasicBlock(scope.allocateRuntimeName("true"), Nil, ListBuffer())
      val falseBB =
        BasicBlock(scope.allocateRuntimeName("false"), Nil, ListBuffer())
      val joinBB = BasicBlock(
        scope.allocateRuntimeName("join"),
        List(result),
        ListBuffer()
      )
      bb.instructions += Instruction.Match(
        tstOp,
        ListMap(IntLit(1) -> (trueBB, Nil)),
        (falseBB, Nil)
      )
      bb = trueBB
      val conOp = translateTerm(con)
      bb.instructions += Instruction.Branch(joinBB, List(conOp))
      bb = falseBB
      val altOp = translateTerm(alt)
      bb.instructions += Instruction.Branch(joinBB, List(altOp))
      joinBB.params.foreach(param =>
        symbolTypeMap += param -> altOp.getType(symbolTypeMap.toMap)
      )
      bb = joinBB
      result
    case App(App(App(Var("if"), tst), con), alt) => die
    case App(Var(nme), Tup(args)) =>
      symbolTypeMap.get(Operand.Var(nme)) match
        case S(tpe: Type.TypeName) =>
          val params = classDefMap(tpe)
          val arguments = (args.zip(params.toList)).map { (arg, param) =>
            (param._1, translateTerm(arg._2.value))
          }
          val result: Operand.Var =
            Operand.Var(scope.allocateRuntimeName())
          bb.instructions += Instruction.Assignment(
            result,
            PureValue.Alloc(symbolTypeMap(Operand.Var(nme)))
          )
          symbolTypeMap += result -> symbolTypeMap(Operand.Var(nme))
          arguments.foreach((argName, term) =>
            bb.instructions += Instruction.SetField(result, argName, term)
          )
          result
        case S(Type.Function(params, ret)) =>
          val callee = translateVar(nme, true)
          val arguments = args map { case (_, Fld(_, _, arg)) =>
            translateTerm(arg)
          }
          makeCall(scope.allocateRuntimeName(), callee, arguments)
        case _ =>
          val callee = translateVar(nme, true)
          val arguments = args map { case (_, Fld(_, _, arg)) =>
            translateTerm(arg)
          }
          makeCall(scope.allocateRuntimeName(), callee, arguments)
    case _ => throw CodeGenError(s"ill-formed application ${inspect(term)}")

  def translateCase(
      caseOf: CaseOf,
      defaultJoin: Option[(BasicBlock, List[Operand])]
  )(implicit scope: Scope): Operand =
    val parent = bb
    val result: Operand.Var =
      Operand.Var(scope.allocateRuntimeName())
    val joinBB = defaultJoin
      .map(_._1)
      .getOrElse(
        BasicBlock(scope.allocateRuntimeName(), List(result), ListBuffer())
      )
    var elseBranch = (joinBB, Nil)
    var childern = MutMap.empty[SimpleTerm, (BasicBlock, List[Operand])]

    @tailrec
    def flatten(
        branch: CaseBranches,
        acc: List[(Option[SimpleTerm], Term)] = Nil
    ): List[(Option[SimpleTerm], Term)] = branch match
      case Case(pat, body, rest) => flatten(rest, acc :+ (Some(pat), body))
      case Wildcard(body)        => (None, body) :: acc
      case NoCases               => acc

    @tailrec
    def translateChild(term: Term, target: BasicBlock, next: BasicBlock)(
        implicit scope: Scope
    ): Unit =
      val currentBB = bb
      bb = target
      term match
        case Let(false, Var(name), value, body) =>
          val letScope = scope.derive("Let")
          val runtimeName = letScope.declareParameter(name)
          val lhs: Operand.Var = Operand.Var(runtimeName)
          varMap += runtimeName -> lhs
          val rhs = translateTerm(value)(letScope)
          symbolTypeMap += lhs -> rhs.getType(symbolTypeMap.toMap)
          bb.instructions += Instruction.Assignment(lhs, PureValue.Op(rhs))
          translateChild(body, target, next)(letScope)
        case caseOf: CaseOf =>
          translateCase(
            caseOf,
            S((joinBB, Nil))
          )
        case _ =>
          val lhs: Operand.Var = Operand.Var(scope.allocateRuntimeName)
          val rhs = translateTerm(term)
          symbolTypeMap += lhs -> rhs.getType(symbolTypeMap.toMap)
          target.instructions += Instruction.Assignment(
            lhs,
            PureValue.Op(rhs)
          )
          target.instructions += Instruction.Branch(
            joinBB,
            List(lhs)
          )
          joinBB.params.foreach(param =>
            symbolTypeMap += param -> lhs.getType(symbolTypeMap.toMap)
          )
          bb = currentBB

    flatten(caseOf.cases).foreach(_ match
      case (S(pat), body) =>
        val thenBB = BasicBlock(scope.allocateRuntimeName(), Nil, ListBuffer())
        val varMapPair = pat match
          //TODO skip Var(keyword)
          case Var("true") => N
          case Var("false") => N
          case Var(name) => 
            val original = translateTerm(caseOf.trm)
            val casted:Operand.Var = Operand.Var(scope.allocateRuntimeName())
            val castTpe = Type.TypeName(name)
            val pair = varMap.find(_._2 == original).get
            symbolTypeMap += casted -> castTpe
            varMap += pair._1 -> casted
            thenBB.instructions += Instruction.Assignment(casted,PureValue.Cast(original,castTpe))
            S(pair)
          case _ => N
        translateChild(body, thenBB, joinBB)
        varMapPair.foreach(varMap += _)
        childern += pat -> (thenBB, Nil)
      case (N, body) =>
        val elseBB =
          BasicBlock(scope.allocateRuntimeName(), Nil, ListBuffer())
        elseBranch = (elseBB, Nil)
        translateChild(body, elseBB, joinBB)
    )
    parent.instructions += Instruction.Match(
      translateTerm(caseOf.trm),
      childern.to(ListMap),
      elseBranch
    )
    bb = joinBB
    result

  def translateTerm(term: Term)(implicit scope: Scope): Operand = term match
    case _ if term.desugaredTerm.isDefined =>
      translateTerm(term.desugaredTerm.getOrElse(die))
    case Var(name) => translateVar(name, false)
    case Super()   => ??? // FIXME: need to check the semantics
    case Lam(_, _) => ??? // should be handled by lifter
    case t: App    => translateApp(t)
    case Rcd(fields) =>
      val result: Operand.Var = Operand.Var(scope.allocateRuntimeName())
      val values = fields.map((k, v) => (k.name, translateTerm(v.value)))
      val recTpe: Type.Record = Type.Record(
        RecordObj(
          values.map((k, v) => k -> v.getType(symbolTypeMap.toMap)).to(MutMap)
        )
      )
      if (!recordDef.contains(recTpe))
        recordDef += recTpe
      bb.instructions += Instruction.Assignment(result, PureValue.Alloc(recTpe))
      values.foreach((k, v) =>
        bb.instructions += Instruction.SetField(result, k, v)
      )
      symbolTypeMap += result -> recTpe
      result
    case Sel(receiver, fieldName) =>
      val receiverOp = translateTerm(receiver)
      // FIXME: add cast
      addTmpVar(PureValue.GetField(receiverOp, fieldName.name))
    case Let(true, Var(name), Lam(_, _), _) =>
      ??? // should be handled by lifter
    case Let(true, Var(name), _, _) =>
      throw new CodeGenError(
        s"recursive non-function definition $name is not supported"
      )
    case Let(false, Var(name), value, body) =>
      // FIXME repeated Let in "if $num is (then ...)* else ..."
      val letScope = scope.derive("Let")
      val runtimeName = letScope.declareParameter(name)
      val lhs: Operand.Var = Operand.Var(runtimeName)
      varMap += runtimeName -> lhs
      val rhs = translateTerm(value)
      symbolTypeMap += lhs -> rhs.getType(symbolTypeMap.toMap)
      bb.instructions += Instruction.Assignment(
        lhs,
        PureValue.Op(rhs)
      )
      translateTerm(body)(letScope)
    case Blk(stmts) =>
      val blkScope = scope.derive("Blk")
      val flattened = stmts.iterator.flatMap(_.desugared._2).toList
      var result = Operand.Unit;
      flattened.iterator.zipWithIndex.foreach {
        case (t: Term, index) =>
          result = translateTerm(t)(blkScope)
        case (NuFunDef(isLetRec, Var(nme), _, L(rhs)), _) =>
          result = translateTerm(rhs)(blkScope)
          val pat = blkScope.declareValue(nme, isLetRec, isLetRec.isEmpty)
          val lhs: Operand.Var = Operand.Var(pat.runtimeName)
          varMap += pat.runtimeName -> lhs
          symbolTypeMap += lhs -> result.getType(symbolTypeMap.toMap)
          bb.instructions += Instruction.Assignment(
            lhs,
            PureValue.Op(result)
          )
        case (_: Def | _: TypeDef | _: NuFunDef /* | _: NuTypeDef */, _) =>
          throw CodeGenError("unsupported definitions in blocks")
      }
      result
    case c: CaseOf => translateCase(c, None)
    case IntLit(value) =>
      if value.isValidInt then Operand.Const(value.toInt)
      else throw CodeGenError(s"integer literal $value is too large")
    case DecLit(value)          => Operand.Const(value.toFloat)
    case StrLit(value)          => Operand.Const(value)
    case UnitLit(_)             => Operand.Unit
    case Asc(trm, _)            => translateTerm(trm)
    case With(trm, Rcd(fields)) => ???
    case Bra(_, trm)            => translateTerm(trm)
    // FIXME: array related stuff requires intrinsic functions
    case Tup(terms)       => ???
    case Subs(arr, idx)   => ???
    case Assign(lhs, rhs) => ??? // FIXME: we have assign???
    case Inst(bod)        => translateTerm(bod)
    case iff: If   => throw CodeGenError(s"if expression was not desugared")
    case New(_, _) => ??? // FIXME: need to check semantics too
    case Forall(_, bod) => translateTerm(bod)
    case TyApp(base, _) => translateTerm(base)
    case Eqn(_, _)      => ??? // unimplemented for now
    case _: Bind | _: Test | If(_, _) | _: Splc | _: Where =>
      throw CodeGenError(s"cannot generate code for term ${inspect(term)}")

  def translateTypingUnit(unit: TypingUnit)(implicit scope: Scope): Unit =
    unit.entities.foreach { entity =>
      symbolTypeMap = entrySymbolTypeMap
      entity match
        case LetS(isRec, pat, rhs)    => ???
        case DataDefn(body)           => ???
        case DatatypeDefn(head, body) => ???
        case NuTypeDef(
              kind,
              nme,
              tparams,
              params,
              ctor, // TODO handle
              sig,
              parents,
              superAnnot,
              thisAnnot,
              body
            ) =>
          kind match
            case Cls =>
              scope.declareClass(
                nme.name,
                params.getOrElse(Tup(Nil)).fields.collect {
                  case S(variable) -> field =>
                    variable.name
                },
                TypeName(nme.name),
                Nil
              )
              val symbolParams = params.get.fields
                .map { case (name, Fld(_, _, tpe)) =>
                  val paramTpe = toType(tpe)
                  (name.map(_.name).get, paramTpe)
                }
              entrySymbolTypeMap += Operand.Var(nme.name) -> Type.TypeName(
                nme.name
              )
              classDefMap += Type.TypeName(nme.name) -> symbolParams.to(
                LinkedHashMap
              )
            case Trt => ???
            case Mxn => ???
            case Als => ???
            case Mod => ???
          translateTypingUnit(body)
        case term: Term =>
          translateTerm(term) // FIXME handle return value
        case NuFunDef(isLetRec, nme, tparams, rhs) =>
          isLetRec match
            case None =>
              rhs match
                case Left(Lam(Tup(fields), rhs)) =>
                  rhs match
                    case Asc(term, tpe) =>
                      val funSymbolTypeMap = entrySymbolTypeMap.clone()
                      val ret = tpe
                      val functionMap = Operand.Var(nme.name) -> (Type.Function(
                        fields.map((k, v) =>
                          val paramTpe = toType(v.value)
                          funSymbolTypeMap += Operand.Var(
                            k.get.name
                          ) -> paramTpe
                          paramTpe
                        ),
                        toType(ret)
                      ))
                      funSymbolTypeMap += functionMap
                      entrySymbolTypeMap += functionMap
                      symbolTypeMap = funSymbolTypeMap
                      val result: Operand.Var =
                        Operand.Var(scope.allocateRuntimeName(nme.name))
                      varMap += nme.name -> result
                      scope.declareValue(nme.name, N, true)
                      val funScope = scope.deriveUnit("Fun")
                      val params = fields.map((k, v) =>
                        val param: Operand.Var =
                          Operand.Var(funScope.allocateRuntimeName(k.get.name))
                        funScope.declareParameter(param.name)
                        varMap += param.name -> param
                        param
                      )
                      val funEntry = BasicBlock(
                        nme.name,
                        params,
                        ListBuffer.empty[Instruction]
                      )
                      bb = funEntry
                      val funRet = translateTerm(term)(funScope)
                      bb.instructions += Instruction.Return(S(funRet))
                      functionList += ((
                        funEntry,
                        funSymbolTypeMap.toMap,
                        toType(ret)
                      ))
                      bb = entryBB
                    case _ => ??? // TODO type inference of function result
                case Left(IntLit(x)) =>
                  throw CodeGenError(s"$x; ${x.getClass}")
                case _ =>
                  ???
            case Some(true) => ???
            case Some(false) =>
              rhs match
                case Left(term) =>
                  addTmpVar(nme.name, PureValue.Op(translateTerm(term)))
                case Right(tpe) => ???
        case TypeDef(
              kind,
              nme,
              tparams,
              body,
              mthDecls,
              mthDefs,
              positionals
            ) =>
          ???
        case Def(rec, nme, rhs, isByname) => ???
        case Constructor(params, body)    => ???
    }

  def apply(
      unit: TypingUnit
  ): (
      List[(BasicBlock, Map[Operand.Var, Type], Type)],
      List[String],
      Map[Type.TypeName, (LinkedHashMap[String, Type], Int)],
      Map[Type.Record, Int]
  ) =
    translateTypingUnit(unit)(Scope("root"))
    (
      functionList.toList :+ ((entryBB, entrySymbolTypeMap.toMap, Type.Unit)),
      imports.toList,
      classDefMap.toIterable.zipWithIndex
        .map((pair, i) => pair._1 -> (pair._2, i))
        .toMap,
      recordDef.zipWithIndex.map((k, v) => k -> v).toMap
    )
}