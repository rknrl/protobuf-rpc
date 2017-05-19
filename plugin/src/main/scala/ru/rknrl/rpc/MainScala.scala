//       ___       ___       ___       ___       ___
//      /\  \     /\__\     /\__\     /\  \     /\__\
//     /::\  \   /:/ _/_   /:| _|_   /::\  \   /:/  /
//    /::\:\__\ /::-"\__\ /::|/\__\ /::\:\__\ /:/__/
//    \;:::/  / \;:;-",-" \/|::/  / \;:::/  / \:\  \
//     |:\/__/   |:|  |     |:/  /   |:\/__/   \:\__\
//      \|__|     \|__|     \/__/     \|__|     \/__/

package ru.rknrl.rpc

import java.io.{PrintWriter, StringWriter}

import com.google.protobuf.DescriptorProtos.DescriptorProto
import com.google.protobuf.ExtensionRegistry
import com.google.protobuf.compiler.PluginProtos.{CodeGeneratorRequest, CodeGeneratorResponse}
import ru.rknrl.rpc.Utils.msgId

import scala.collection.JavaConversions.{asJavaIterable, asScalaBuffer}

object MainScala {
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

    val messages = request.getProtoFileList
      .filter(f ⇒ request.getFileToGenerateList.contains(f.getName))
      .flatMap(f ⇒ f.getMessageTypeList
        .filter(m ⇒ m.hasOptions && m.getOptions.hasExtension(Options.msgid)))

    val content =
      "package generated\n" +
        "import akka.util.ByteString\n" +
        "object Serializer extends ru.rknrl.rpc.Serializer {\n" +
        "def bytesToMessage(msgId: Int, byteString: ByteString) = {\n" +
        "val is = byteString.iterator.asInputStream\n" +
        "msgId match {\n" +
        messages.map(m ⇒ receiveContent(m, messageNameToPackage(m.getName))).mkString +
        "}\n" +
        "}\n" +
        "def messageToId(message: Any) = {\n" +
        "message match {\n" +
        messages.map(m ⇒ sendContent(m, messageNameToPackage(m.getName))).mkString +
        "}\n" +
        "}\n" +
        "}\n"

    Seq(
      CodeGeneratorResponse.File.newBuilder
        .setName("generated/Serializer.scala")
        .setContent(content)
        .build
    )
  }

  def messageClass(d: DescriptorProto, `package`: String) = `package` + "." + d.getName

  def sendContent(d: DescriptorProto, `package`: String) =
    "case _:" + messageClass(d, `package`) + "⇒" + msgId(d) + "\n"

  def receiveContent(d: DescriptorProto, `package`: String) =
    "case " + msgId(d) + "⇒" + messageClass(d, `package`) + ".parseDelimitedFrom(is).get\n"

}
