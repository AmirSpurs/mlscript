package mlscript.compiler.wasm

import mlscript.compiler.wasm.MachineInstr
import mlscript.compiler.wasm.Type
import mlscript.compiler.wasm.WasmPrinter
import mlscript.utils.shorthands.*

import scala.collection.immutable.ListMap
import scala.collection.mutable.{Map => MutMap}

class MachineFunction (
    val name: String,
    val args: ListMap[String, Type],
    val locals: Map[String, Type],
    val retType: Type,
    val instrs: Ls[MachineInstr],
    val visibility: Bool = false
):
  override def toString: String = show
  
  def show: String =
    toDocument.print

  def toDocument: Document
    = WasmPrinter.mkFunction(this)

end MachineFunction

// class LocalsHandler:
//   var locals: MutMap[String, Type] = new MutMap()
//   var args: 


//   def getType(name: String) = locals_(name)._1

//   def getIsParam(name: String) = locals_(name)._2

//   def register(name: String, tpe: Type, isParam: Boolean = false) =
//     locals_ += (name -> (tpe, isParam))
//   private[wasm] def getLocals = locals_


object MachineFunction:
  // def apply(
  //     name: Str,
  //     args: Map[String, Type],
  //     retType: Type
  // )=
  //   val lh = new LocalsHandler()
  //   // Make code first, as it may increment the locals in lh
  //   val code = codeGen(lh)
  //   new Function(name, args, isMain, lh.getLocals.toMap, retType, code, codeGen)
end MachineFunction