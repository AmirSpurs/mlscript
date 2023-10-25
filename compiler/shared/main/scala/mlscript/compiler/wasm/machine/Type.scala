package mlscript.compiler.wasm

import mlscript.compiler.ClassInfo
import mlscript.utils.*
import mlscript.utils.shorthands.*

import scala.collection.BitSet
import scala.collection.immutable.{List, Map, VectorMap}

// mlscript class is encoded as a record with some field
class RecordObj(val fields: Ls[(String, Type)]):
  def apply(name: String): Option[Type] = ???

  def size: Int =
    RecordObj.tagSize + fields.size * RecordObj.defaultFieldSize

  def getFieldOffset(name: String): Int =
    val index = fields.indexWhere { _._1 == name }
    RecordObj.tagSize + index * RecordObj.defaultFieldSize
  
  // ad hoc overload only for opaque record e.g. LetCall
  def getFieldOffset(index: Int) : Int =
    RecordObj.tagSize + index * RecordObj.defaultFieldSize


object RecordObj:
  final val tagSize = 4
  final val defaultFieldSize = 4
  def from(classinfo: ClassInfo) =
    val fields = classinfo.fields.map { name => (name -> Type.defaultNumType)}
    RecordObj(fields)
  def opaque(numFields: Int) =
    val fields = List.range(0, numFields) map { index => (index.toString() -> Type.defaultNumType)}
    RecordObj(fields)

class VariantObj(val variants: Map[String, Option[Type.Record]]):
  def apply(name: String): Option[Option[Type.Record]] = variants.get(name)
  // Pattern to check if the variant is of a certain type
  // The first element is the offset in bits, the second element is
  // the bit pattern. offset + bit pattern length = record offset,
  // the third element is a list of forbidden patterns.
  def checkBits(name: String): (Int, BitSet, List[BitSet]) = ???

  override def toString(): String = "<" + variants
    .map((name, ty) => name + ty.map(rec => s": $rec").getOrElse(""))
    .mkString(" | ") + ">"


sealed trait ValueType:
  override def toString(): String
  def show: String

// TODO: support all wasm types
enum Type:
  case Int32 extends Type, ValueType
  case Int64 extends Type, ValueType
  case Float32 extends Type, ValueType
  case Float64 extends Type, ValueType
  case Record(impl: ClassInfo)

  def size: Int = this match
    case (Int32 | Float32) => 4
    case (Int64 | Float64) => 8
    case Record(cls) => RecordObj.from(cls).size

  override def toString(): String = show

  def show: String = 
    toDocument.print
  
  def toDocument: Document = this match
    case Int32 => "i32" |> raw
    case Int64 => "i64" |> raw
    case Float32 => "f32" |> raw
    case Float64 => "f64" |> raw
    case Record(classinfo) => 
      raw(classinfo.ident)
      <#> raw("(") <#> raw(classinfo.fields map {x => x.toString()} mkString(","))
      <#> raw(")")

object Type:
  // TODO: GraphOptimizer does not support type 
  final val defaultNumType: Type = Int32