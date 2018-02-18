//       ___       ___       ___       ___       ___
//      /\  \     /\__\     /\__\     /\  \     /\__\
//     /::\  \   /:/ _/_   /:| _|_   /::\  \   /:/  /
//    /::\:\__\ /::-"\__\ /::|/\__\ /::\:\__\ /:/__/
//    \;:::/  / \;:;-",-" \/|::/  / \;:::/  / \:\  \
//     |:\/__/   |:|  |     |:/  /   |:\/__/   \:\__\
//      \|__|     \|__|     \/__/     \|__|     \/__/

package ru.rknrl.rpc

import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Label._
import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Type._
import com.google.protobuf.DescriptorProtos.{DescriptorProto, FieldDescriptorProto}
import com.google.protobuf.compiler.PluginProtos.CodeGeneratorRequest

import scala.collection.JavaConversions.asScalaBuffer

object Utils {
  def msgId(d: DescriptorProto) = d.getOptions.getExtension(Options.msgid)

  def checkMsgIdDuplicates(ds: Seq[DescriptorProto]): Unit = {
    val duplicates = ds.groupBy(d ⇒ msgId(d)).filter(_._2.size > 1)
    if (duplicates.nonEmpty) throw new Error("Duplicate msgId: " + duplicates.keys.mkString(","))
  }

  /** MyWord => myWord */
  def uncapitalize(s: String): String =
    if (s == null) null
    else if (s.length == 0) ""
    else if (s.charAt(0).isLower) s
    else {
      val chars = s.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    }


  def fullPathClass(f: FieldDescriptorProto) =
    if (f.getTypeName.charAt(0) == '.')
      f.getTypeName.substring(1)
    else
      f.getTypeName

  def messageNameToPackage(request: CodeGeneratorRequest) =
    for (f ← request.getProtoFileList;
         m ← f.getMessageTypeList)
      yield m.getName → f.getPackage

  def className(d: DescriptorProto) = d.getName

  def fieldName(f: FieldDescriptorProto) = uncapitalize(f.getName)

  def upperFieldName(f: FieldDescriptorProto) = f.getName.capitalize

  def privateFieldName(f: FieldDescriptorProto) = "_" + fieldName(f)

  def isNullableType(as3Type: String) =
    as3nullableType(as3Type) == as3Type

  def as3nullableType(as3Type: String) =
    as3Type match {
      case "Number" ⇒ "Num"
      case "int" ⇒ "Int"
      case "uint" ⇒ "UInt"
      case "Boolean" ⇒ "Bool"
      case _ ⇒ as3Type
    }

  def as3Type(f: FieldDescriptorProto) =
    f.getType match {
      case TYPE_DOUBLE |
           TYPE_FLOAT ⇒ "Number"

      case TYPE_INT32 |
           TYPE_FIXED32 |
           TYPE_SFIXED32 |
           TYPE_SINT32 ⇒ "int"

      case TYPE_UINT32 ⇒ "uint"

      case TYPE_BOOL ⇒ "Boolean"

      case TYPE_INT64 |
           TYPE_FIXED64 |
           TYPE_SFIXED64 |
           TYPE_SINT64 |
           TYPE_UINT64 ⇒ "Number"

      case TYPE_STRING ⇒ "String"

      case TYPE_MESSAGE |
           TYPE_ENUM ⇒ fullPathClass(f)

      case TYPE_BYTES ⇒ "ByteArray"
    }

  def fieldType(f: FieldDescriptorProto) =
    f.getLabel match {
      case LABEL_OPTIONAL ⇒ as3nullableType(as3Type(f))
      case LABEL_REQUIRED ⇒ as3Type(f)
      case LABEL_REPEATED ⇒ "Vector.<" + as3Type(f) + ">";
    }

  def getterType(f: FieldDescriptorProto) =
    f.getLabel match {
      case LABEL_OPTIONAL ⇒ as3Type(f)
      case LABEL_REQUIRED ⇒ as3Type(f)
      case LABEL_REPEATED ⇒ "Vector.<" + as3Type(f) + ">";
    }


}
