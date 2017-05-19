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
import ru.rknrl.rpc.Utils._

import scala.collection.JavaConversions.{asJavaIterable, asScalaBuffer}

object MainJava {
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
        .setName("protos/Server.java")
        .setContent(serverContent(messages, `package`))
        .build

    def serverContent(messages: Seq[DescriptorProto], `package`: String) =
      "package protos;\n" +
        imports(messages) +
        "public class Server extends TcpConnection {\n" +
        "public Server(Listener listener) {\n        super(listener);\n    }\n" +
        "@Override\n" +
        "protected void processMessage(Message message) {\n" +
        "final InputStream is = new ByteArrayInputStream(message.byteArray);\n" +
        "try {\n" +
        "switch(message.msgId) {\n" +
        serverReceiveContent(messages) +
        "default:\n" +
        "d(\"Unhandled message \" + message.msgId);\n" +
        "}\n" +
        "} catch (Exception e) {" +
        "e(\"Process message error\", e);" +
        "}\n" +
        "}\n" +
        listenersContent(messages) +
        serverSendContent(messages) +
        removeAllListenersContent(messages) +
        "}\n"

    def imports(messages: Seq[DescriptorProto]) =
      "import ru.rknrl.rpc.TcpConnection;\n" +
        "import java.io.ByteArrayInputStream;\n" +
        "import java.io.InputStream;\n" +
        "import java.util.HashSet;\n" +
        "import java.util.Set;\n" +
        messagesImports(messages)

    def messagesImports(messages: Seq[DescriptorProto]) =
      messages.map(m ⇒ "import " + messageNameToPackage(m.getName) + "." + m.getName + ";\n").mkString

    def serverReceiveContent(messages: Seq[DescriptorProto]) =
      messages.map(receiveContent).mkString

    def receiveContent(d: DescriptorProto) = {
      val clazz = className(d)
      val value = Utils.uncapitalize(clazz)
      val clazzWithPackage = messageNameToPackage(d.getName) + "." + clazz
      "case " + msgId(d) + ":\n" +
        clazzWithPackage + " " + value + " = " + clazzWithPackage + ".parseDelimitedFrom(is);\n" +
        "if (isLoggable) {\n" +
        "d(" + value + ".toString());\n" +
        "}\n" +
        "for (" + clazz + "Listener listener : " + value + "Listeners) {\n" +
        "listener.on" + clazz + "(" + value + ");\n" +
        "}\n" +
        "break;\n"
    }

    def listenersContent(messages: Seq[DescriptorProto]) =
      messages.map(listenerContent).mkString

    def listenerContent(d: DescriptorProto) = {
      val clazz = className(d)
      val value = Utils.uncapitalize(clazz)
      val clazzWithPackage = messageNameToPackage(d.getName) + "." + clazz
      val listener = clazz + "Listener"
      s"public interface $listener {\n" +
        s"void on$clazz($clazzWithPackage $value);\n" +
        "}\n" +
        s"private final Set<$listener> ${value}Listeners = new HashSet<$listener>();\n" +
        s"public void addOn${clazz}Listener($listener listener) {\n" +
        s"${value}Listeners.add(listener);\n" +
        "}\n" +
        s"public void removeOn${clazz}Listener($listener listener) {\n" +
        s"${value}Listeners.remove(listener);\n" +
        "}\n"
    }

    def removeAllListenersContent(messages: Seq[DescriptorProto]) =
      "public void removeAllListeners(){\n" +
        messages.map { d ⇒
          val clazz = className(d)
          val value = Utils.uncapitalize(clazz)
          s"${value}Listeners.clear();\n"
        }.mkString +
        "}\n"

    def serverSendContent(messages: Seq[DescriptorProto]) =
      messages.map(sendContent).mkString

    def sendContent(d: DescriptorProto) =
      "public void " + sendMethodName(d) + "(" + messageNameToPackage(d.getName) + "." + className(d) + " message) {\n" +
        "sendMessage(" + msgId(d) + ", message);\n" +
        "}\n"

    val messages = request.getProtoFileList
      .filter(f ⇒ request.getFileToGenerateList.contains(f.getName))
      .flatMap(f ⇒ f.getMessageTypeList
        .filter(m ⇒ m.hasOptions && m.getOptions.hasExtension(Options.msgid)))
      .toSeq

    Seq(serverFile(messages, ""))
  }

  def packageToPath(`package`: String) =
    if (`package`.isEmpty)
      ""
    else
      `package`.replace('.', '/') + "/"


  def methodName(d: DescriptorProto) = uncapitalize(d.getName)

  def sendMethodName(d: DescriptorProto) = "send" + d.getName


}
