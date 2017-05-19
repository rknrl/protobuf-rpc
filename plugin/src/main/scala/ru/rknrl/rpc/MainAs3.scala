//       ___       ___       ___       ___       ___
//      /\  \     /\__\     /\__\     /\  \     /\__\
//     /::\  \   /:/ _/_   /:| _|_   /::\  \   /:/  /
//    /::\:\__\ /::-"\__\ /::|/\__\ /::\:\__\ /:/__/
//    \;:::/  / \;:;-",-" \/|::/  / \;:::/  / \:\  \
//     |:\/__/   |:|  |     |:/  /   |:\/__/   \:\__\
//      \|__|     \|__|     \/__/     \|__|     \/__/

package ru.rknrl.rpc

import java.io.{PrintWriter, StringWriter}

import com.google.protobuf.DescriptorProtos.FieldDescriptorProto.Label.LABEL_OPTIONAL
import com.google.protobuf.DescriptorProtos.{DescriptorProto, FieldDescriptorProto}
import com.google.protobuf.ExtensionRegistry
import com.google.protobuf.compiler.PluginProtos.{CodeGeneratorRequest, CodeGeneratorResponse}
import ru.rknrl.rpc.Utils._

import scala.collection.JavaConversions.{asScalaBuffer, asJavaIterable}

object MainAs3 {
  def main(args: Array[String]) {
    val extensionRegistry = ExtensionRegistry.newInstance()

    Options.registerAllExtensions(extensionRegistry)

    val request = CodeGeneratorRequest.parseFrom(System.in, extensionRegistry)
    createResponse(request).writeTo(System.out)
    System.out.flush()
  }

  def createResponse(request: CodeGeneratorRequest) =
    try {
      CodeGeneratorResponse.newBuilder.addAllFile(createFiles(request)).build
    } catch {
      case t: Throwable ⇒
        val sw = new StringWriter
        t.printStackTrace(new PrintWriter(sw))
        CodeGeneratorResponse.newBuilder.setError(sw.toString).build
    }

  def createFiles(request: CodeGeneratorRequest) = {
    val messageNameToPackage = Utils.messageNameToPackage(request).toMap

    def serverFile(messages: Seq[DescriptorProto], `package`: String) =
      CodeGeneratorResponse.File.newBuilder
        .setName(packageToPath(`package`) + "Server.as")
        .setContent(serverContent(messages, `package`))
        .build

    def serverContent(messages: Seq[DescriptorProto], `package`: String) =
      s"package ${`package`} {\n" +
        imports(messages) +
        "public class Server extends TcpConnection {\n" +
        "public function Server(socket:Socket):void {\n" +
        "    super(socket);\n" +
        "}\n" +
        "override protected function parsePacket(): void {\n" +
        "switch(msgId) {\n" +
        serverReceiveContent(messages) +
        "default:\n" +
        "var skipLength:uint = ReadUtils.readUInt32(socket);\n" +
        "if (skipLength > 0) {\n" +
        "byteArray.clear();\n" +
        "socket.readBytes(byteArray, 0, skipLength);\n" +
        "}\n" +
        "}\n" +
        "}\n" +
        serverSendContent(messages) +
        "}\n" +
        "}\n"

    def imports(messages: Seq[DescriptorProto]) =
      "import ru.rknrl.rpc.TcpConnection;\n" +
        "import flash.net.Socket;\n" +
        "import ru.rknrl.protobuf.*;\n" +
        messagesImports(messages)

    def messagesImports(messages: Seq[DescriptorProto]) =
      messages.map(m ⇒ "import " + messageNameToPackage(m.getName) + "." + m.getName + "\n").mkString

    def serverReceiveContent(messages: Seq[DescriptorProto]) =
      messages.map(receiveContent).mkString

    def receiveContent(d: DescriptorProto) =
      "case " + msgId(d) + ":\n" +
        "dispatchEvent(new " + messageNameToPackage(d.getName) + "." + eventClassName(d) + "(" + messageNameToPackage(d.getName) + "." + eventVarType(d) + ".parseDelimitedFrom(socket)));\n" +
        "break;\n"

    def serverSendContent(messages: Seq[DescriptorProto]) =
      messages.map(sendContent).mkString

    def sendContent(d: DescriptorProto) =
      "public function " + sendMethodName(d) + "(message:" + messageNameToPackage(d.getName) + "." + className(d) + "):void {\n" +
        "byteArray.clear();\n" +
        "message.writeDelimitedTo(byteArray);\n" +
        "send(" + msgId(d) + ", byteArray);\n" +
        "}\n" +
        "public function " + methodName(d) + "(" + sendMethodParameters(d) + "):void {\n" +
        sendMethodName(d) + "(new " + messageNameToPackage(d.getName) + "." + eventVarType(d) + "(" + constructorParameters(d) + "));\n" +
        "}\n"

    def sendMethodParameters(d: DescriptorProto) =
      d.getFieldList.map(f ⇒ fieldName(f) + ":" + fieldType(f)).mkString(",")

    def constructorParameters(d: DescriptorProto) =
      d.getFieldList.map(fieldName).mkString(",")

    val messages = request.getProtoFileList
      .filter(f ⇒ request.getFileToGenerateList.contains(f.getName))
      .flatMap(f ⇒ f.getMessageTypeList
        .filter(m ⇒ m.hasOptions && m.getOptions.hasExtension(Options.msgid)))
      .toSeq

    val events = messages.map(m ⇒ createEventFile(m, messageNameToPackage(m.getName)))

    events ++ Seq(serverFile(messages, ""))
  }

  def packageToPath(`package`: String) =
    if (`package`.isEmpty)
      ""
    else
      `package`.replace('.', '/') + "/"

  def createEventFile(d: DescriptorProto, `package`: String) =
    CodeGeneratorResponse.File.newBuilder
      .setName(packageToPath(`package`) + d.getName + "Event.as")
      .setContent(eventContent(d, `package`))
      .build

  def methodName(d: DescriptorProto) = uncapitalize(d.getName)

  def sendMethodName(d: DescriptorProto) = "send" + d.getName

  def eventClassName(d: DescriptorProto) = d.getName + "Event"

  /**
    * "buyItem" => "BUY_ITEM"
    * "startGC" => "START_GC"
    */
  def eventConstName(d: DescriptorProto) = {
    val builder = new StringBuilder()

    val chars = d.getName.toCharArray
    for (i ← chars.indices) {
      val c = chars(i)
      if (c.isUpper && i > 0 && chars(i - 1).isLower) builder.append("_")
      builder.append(c.toUpper)
    }

    builder.toString
  }

  def eventConstValue(d: DescriptorProto) = eventVarName(d)

  def eventVarName(d: DescriptorProto) = uncapitalize(d.getName)

  def eventVarType(d: DescriptorProto) = d.getName

  def eventContent(d: DescriptorProto, `package`: String) =
    "package " + `package` + " {\n" +
      "import flash.events.Event;\n" +
      "import ru.rknrl.protobuf.*;\n" +
      "public class " + eventClassName(d) + " extends Event {\n" +
      "public static const " + eventConstName(d) + ": String = '" + eventConstValue(d) + "';\n" +
      "private var _" + eventVarName(d) + ":" + eventVarType(d) + ";\n" +
      "public function get" + d.getName + "():" + eventVarType(d) + " {\n" +
      "return _" + eventVarName(d) + ";\n" +
      "}\n" +
      eventGetters(d) +
      eventConstructor(d) +
      eventToString(d) +
      "}\n" +
      "}\n"

  def eventGetters(d: DescriptorProto) =
    d.getFieldList.map(f ⇒ eventGetter(d, f)).mkString

  def eventGetter(d: DescriptorProto, f: FieldDescriptorProto) =
    "public function get " + fieldName(f) + "():" + getterType(f) + " {\n" +
      "return _" + eventVarName(d) + "." + fieldName(f) + ";\n" +
      "}\n" +
      eventHasGetter(d, f)

  def eventHasGetter(d: DescriptorProto, f: FieldDescriptorProto) =
    if (f.getLabel == LABEL_OPTIONAL)
      "public function get has" + upperFieldName(f) + "():Boolean {\n" +
        "return _" + eventVarName(d) + ".has" + upperFieldName(f) + ";\n" +
        "}\n"
    else
      ""

  def eventConstructor(d: DescriptorProto) =
    "public function " + eventClassName(d) + "(" + eventVarName(d) + ":" + eventVarType(d) + "){\n" +
      "_" + eventVarName(d) + "=" + eventVarName(d) + ";\n" +
      "super(" + eventConstName(d) + ")\n" +
      "}\n"

  def eventToString(d: DescriptorProto) =
    "override public function toString():String {\n" +
      "return type + ' ' + _" + eventVarName(d) + ";\n" +
      "}\n"
}
