package mlscript.compiler.wasm

import mlscript.compiler.ClassInfo
import mlscript.utils.*
import mlscript.utils.shorthands.*

import scala.collection.BitSet
import scala.collection.immutable.{List, Map, VectorMap}
import scala.collection.mutable.ListBuffer

case class MethodInfo(
  clsName: Str,
  methodName: Str,
  fid: Int,
  isCtor: Bool,
  isVirtual: Bool
)

// mlscript class is encoded as a record with some field
// handle memory layout
class RecordObj(
  fields: Ls[(String, Type)], 
  methods: Map[String, MethodInfo]
):
  def apply(name: String): Option[Type] = ???

  def buildVtable() = ???

  lazy val vtable: Ls[(String, MethodInfo)] = buildVtable()

  def size: Int =
    fields.size * RecordObj.defaultFieldSize

  def getFieldOffset(name: String): Int =
    val index = fields.indexWhere { _._1 == name }
    index * RecordObj.defaultFieldSize
  
  // ad hoc overload only for opaque record e.g. LetCall
  def getFieldOffset(index: Int) : Int =
    index * RecordObj.defaultFieldSize
  
  def getParentOffet(name: Str): Int =
    // in our case class have at most 1 parent.
    // its members are decomposed into scalar and
    // placed at the beginning of the class
    0


object RecordObj:
  final val tagSize = 4
  final val defaultFieldSize = 4
  var recordCache = Map[Str, RecordObj]()

  private def collectField(clsctx: Map[Str, ClassInfo], cls: ClassInfo): Ls[(String, Type)] =
    def collectFieldRec(wcls: ClassInfo): Ls[Str] =
      val params = wcls.fields
      val fields = wcls.members.keys.toList
      val parents = wcls.parents.keys.toList flatMap { 
        pname =>
          val pcls = clsctx(pname)
          collectFieldRec(pcls)
      }
      parents ++ params ++ fields
    val fieldNames = collectFieldRec(cls)
    val numFields = fieldNames.size
    fieldNames.zip(List.fill(numFields)(Type.defaultNumType))

  private def collectMethod(clsctx: Map[Str, ClassInfo], cls: ClassInfo) =
    def collectMethodRec(wcls: ClassInfo): Ls[(Str, MethodInfo)] =
      // scala's default duplicate handling policy shows
      // duplicate keys will be overwritten by later keys
      val methods = wcls.methods.toList map {
        case (name, mdef) =>
          name ->
          MethodInfo(
            wcls.ident,
            name,
            mdef.id,
            false,
            false
          )
      }
      val parents = wcls.parents.keys.toList flatMap {
        pname =>
          val pcls = clsctx(pname)
          collectMethodRec(pcls)
      }
      parents ++ methods
    collectMethodRec(cls).toMap


  def from(clsctx: Map[Str, ClassInfo], cls: ClassInfo) =
    if recordCache.contains(cls.ident) then
      recordCache(cls.ident)
    else
      val fieldsLayout = collectField(clsctx, cls)
      val methodLayout = collectMethod(clsctx, cls)
      val layoutAux = RecordObj(fieldsLayout, methodLayout)
      recordCache = recordCache.updated(cls.ident, layoutAux)
      layoutAux

  def opaque(numFields: Int) =
    val fields = List.range(0, numFields) map { index => (index.toString() -> Type.defaultNumType)}
    RecordObj(fields, Map[String, MethodInfo]())

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