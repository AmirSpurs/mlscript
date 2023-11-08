package mlscript.compiler.wasm
import mlscript.codegen.CodeGenError
import scala.collection.mutable.{Map => MutMap, StringBuilder}
import scala.sys.process._
import java.io._
import os.*


object Env {
  sealed trait OS
  object Linux extends OS
  object Windows extends OS
  object Mac extends OS

  lazy val osName = {
    // If all fails returns Linux
    val optOsName = Option(System.getProperty("os.name"))
    optOsName.map(_.toLowerCase()).map { osName =>
      if (osName contains "linux") Linux
      else if (osName contains "win") Windows
      else if (osName contains "mac") Mac
      else Linux
    } getOrElse Linux
  }

  val tmpDir =
    Path(System.getProperty("java.io.tmpdir"))
}

object WasmInterp:
  private var counterMap = MutMap.empty[String, Int]


  // check the wasm module by `wasmtime` and return the output from stdin
  def check(module: Module): String =
    def pathWithExt(ext: String): Path = Env.tmpDir / s"${nameWithExt(ext)}"
    def nameWithExt(ext: String): String = s"${module.name}.$ext"

    val (local, sys) =
      Env.osName match {
        case Env.Linux   => ("./bin/linux/wat2wasm", "wat2wasm")
        case Env.Windows => ("./bin/windows/wat2wasm.exe", "wat2wasm.exe")
        case Env.Mac     => ("./bin/macos/wat2wasm", "wat2wasm")
      }
    
    val wasmPath = pathWithExt("wasm")
    val watPath = pathWithExt("wat")

    val w2wOptions = s"${watPath} -o ${wasmPath}"

    module.writeWasmText(watPath)

    // wat2wasm
    try {
      try {
        // compile WAT to WASM with local wat2wasm
        s"$local $w2wOptions".!!
      } catch {
        // compiler WAT to WASM with system wat2wasm
        case _: IOException =>
          s"$sys $w2wOptions".!!
      }
    } catch {
      case _: IOException =>
        throw WasmBackendError(
          "wat2wasm utility was not found under ./bin or in system path, " +
            "or did not have permission to execute"
        )
      case _ =>
        throw WasmBackendError(
          s"wat2wasm failed to translate WebAssembly text file ${watPath} to binary"
        )
    }

    // wasmtime check
    // ad hoc code, manually invoke main function
    val wasmtimeCommand = s"wasmtime run --invoke main ${wasmPath}"
    val stdoutOutput = StringBuilder()
    try {
      stdoutOutput ++= wasmtimeCommand.!!
    } catch {
      case _: Exception =>
        throw WasmBackendError(
          s"wasmtime failed to run the wasm module ${wasmPath}!"
        )
    } finally {
      // remove temporary wat and wasm file
      if os.exists(watPath) then
        os.remove(watPath)
      if os.exists(wasmPath) then
        os.remove(wasmPath)
    }


    stdoutOutput.toString()
end WasmInterp

