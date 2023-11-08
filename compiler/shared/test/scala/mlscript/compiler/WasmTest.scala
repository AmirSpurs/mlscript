package mlscript.compiler

import mlscript.utils.shorthands.*
import scala.util.control.NonFatal
import scala.collection.mutable.StringBuilder
import mlscript.codegen.Helpers.inspect as showStructure
import mlscript.{DiffTests, ModeType, TypingUnit}
import mlscript.compiler.debug.TreeDebug
import mlscript.compiler.mono.Monomorph
import mlscript.compiler.printer.ExprPrinter
import mlscript.compiler.mono.MonomorphError
import mlscript.compiler.wasm.WasmInterp

class WasmDiffTestCompiler extends DiffTests {
  import WasmDiffTestCompiler.*
  override def postProcess(mode: ModeType, basePath: List[Str], testName: Str, unit: TypingUnit): List[Str] = 
    val outputBuilder = StringBuilder()
    outputBuilder ++= "Parsed:\n"
    outputBuilder ++= showStructure(unit)

    outputBuilder ++= "\nLifted:\n"
    var rstUnit = unit;
    try
      // rstUnit = ClassLifter().liftTypingUnit(unit)
      // outputBuilder ++= PrettyPrinter.showTypingUnit(rstUnit)
      outputBuilder ++= "\n\nGraphOpt:\n"
      val go = GraphOptimizer()
      val graph = go.buildGraph(unit)
      outputBuilder ++= graph.toString()
      outputBuilder ++= "\n\nWasmBackend:\n"
      val wasmBackend = wasm.WasmBackend()
      val wasmModule = wasmBackend.translate(graph)
      outputBuilder ++= wasm.WasmPrinter(wasmModule)

      val wasmInterpStdout = WasmInterp.check(wasmModule)
      outputBuilder ++= "\nWasm Interpreter Output:\n"
      outputBuilder ++= wasmInterpStdout

    catch
      case err @ GraphOptimizingError(msg) =>
        outputBuilder ++= s"GraphOpt failed: ${msg}"
        outputBuilder ++= "\n" ++ err.getStackTrace().map(_.toString()).mkString("\n")
      case err @ wasm.WasmBackendError(msg) =>
        outputBuilder ++= s"WasmBackend failed: ${msg}"
        outputBuilder ++= "\n" ++ err.getStackTrace().map(_.toString()).mkString("\n")
      case err =>
        outputBuilder ++= "Unknown error:"
        outputBuilder ++= "\n" ++ err.getStackTrace().map(_.toString()).mkString("\n")
      case NonFatal(err) =>
        outputBuilder ++= "Lifting failed: " ++ err.toString()
        /* if mode.fullExceptionStack then */ outputBuilder ++=
          "\n" ++ err.getStackTrace().map(_.toString()).mkString("\n")
    outputBuilder.toString().linesIterator.toList
  
  override protected lazy val files = allFiles.filter { file =>
      val fileName = file.baseName
      validExt(file.ext) && filter(file.relativeTo(pwd))
  }
}

object WasmDiffTestCompiler {

  private val pwd = os.pwd
  private val dir = pwd/"compiler"/"shared"/"test"/"wasm"
  
  private val allFiles = os.walk(dir).filter(_.toIO.isFile)
  
  private val validExt = Set("fun", "mls")

  private def filter(file: os.RelPath) = DiffTests.filter(file)
  
}
