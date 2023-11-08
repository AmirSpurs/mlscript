package mlscript.compiler.wasm

import mlscript.compiler.wasm.MachineInstr.*
import mlscript.utils.*
import mlscript.utils.shorthands.*

// Printer for Wasm modules, function and instruction in fancy style
// TODO: make fancier print for S-expr style
object WasmPrinter:

  def mkModule(mod: Module): Document = stack(
    "(module " |> raw,
    indent(stack(
      // some test case `out of bounds memory access` if only 1 page
      "(memory (export \"mem\") 100)" |> raw,
      stack(mod.imports map mkImport),
      "(global (mut i32) i32.const 0) " |> raw,
      stack(mod.functions map mkFunction)
    )),
    ")" |> raw
  )

  def mkImport(s: String): Document =
    raw("(import") <:> raw(s) <#>  raw(")")

  def mkFunction(function: MachineFunction): Document =
    val name = function.name
    val exportDoc = if (function.visibility) then
       s"""(export "$name" (func $$$name))""" |> raw
       else "" |> raw
    val paramsDoc = line(
      function.args.toList map { case (name, typ) =>
        s"(param $$${name} $typ)" |> raw}
      )
    val resultDoc = line(
      function.retType map { typ =>
        s"(result ${typ})" |> raw}
    )
    val localsDoc = stack(
        (function.locals map { case (name, typ) =>
            s"(local $$$name $typ)" |> raw
        }).toList
      )

    stack(
      raw(s"(func $$${name}") <:> paramsDoc <:> resultDoc, 
      indent(localsDoc),
      indent(stack(mkInstrLists(function.instrs))),
      ")" |> raw,
      exportDoc
    )

  def mkInstrLists(code: Ls[MachineInstr]): Ls[Document] = code match 
    case Nil => Nil 
    case instr :: ts => instr match
        case Else =>
            List(
              mkInstruction(instr),
              indent(stack(mkInstrLists(ts)))
            )
        case End =>
            List(unindent(stack(
              mkInstruction(instr)
              +: mkInstrLists(ts)))
            )
        case If_void | If_i32 | Block(_, _) | Loop(_) =>
          List(
            mkInstruction(instr),
            indent(stack(mkInstrLists(ts)))
          )
        case _ =>
            mkInstruction(instr)
            :: mkInstrLists(ts)

  private def mkInstruction(instr: MachineInstr): Document = 
    instr.toDocument
  
  def apply(module: Module) = mkModule(module).print
  def apply(function: MachineFunction) = mkFunction(function).print
  def apply(instr: MachineInstr) = mkInstruction(instr).print

end WasmPrinter
