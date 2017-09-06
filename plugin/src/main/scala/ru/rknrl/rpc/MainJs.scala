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

import scala.collection.JavaConversions.asScalaBuffer

object MainJs {
  def main(args: Array[String]) {
    val extensionRegistry = ExtensionRegistry.newInstance()

    Options.registerAllExtensions(extensionRegistry)

    val request = CodeGeneratorRequest.parseFrom(System.in, extensionRegistry)
    createResponse(request).writeTo(System.out)
    System.out.flush()
  }

  def createResponse(request: CodeGeneratorRequest) =
    try {
      CodeGeneratorResponse.newBuilder.addFile(createFiles(request)).build
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
        .setName("server.js")
        .setContent(serverContent(messages, `package`))
        .build

    def serverContent(messages: Seq[DescriptorProto], `package`: String) =
      s"""/* GENERATED. DO NOT MODIFY */
         |"use strict";
         |
         |var Server;
         |
         |(function () {
         |    Server = function Server(connection, protos) {
         |        this.connection = connection;
         |        this.protos = protos;
         |
         |        const self = this;
         |
      |        connection.onmessage = function (messageId, messageBytes) {
         |            switch (messageId) {
         |            ${serverReceiveContent(messages)}
         |            }
         |        };
         |    };
         |
         |    ${serverHandlersContent(messages)}
         |
         |    ${serverSendContent(messages)}
         |})();
         |
      |module.exports = Server;
    """.stripMargin


    def serverReceiveContent(messages: Seq[DescriptorProto]) =
      messages.map(receiveContent).mkString

    def receiveContent(d: DescriptorProto) =
      s"""
         |                case ${msgId(d)}:
         |                    if (self.on${d.getName})
         |                        self.on${d.getName}(protos.${d.getName}.decodeDelimited(messageBytes));
         |                    break;
      """.stripMargin

    def serverHandlersContent(messages: Seq[DescriptorProto]) =
      messages.map(handlerContent).mkString

    def handlerContent(d: DescriptorProto) =
      s"Server.prototype.on${d.getName} = null;\n"

    def serverSendContent(messages: Seq[DescriptorProto]) =
      messages.map(sendContent).mkString

    def sendContent(d: DescriptorProto) =
      s"""
         |    Server.prototype.${methodName(d)} = function (${sendMethodParameters(d).mkString(",")}) {
         |        this.connection.send(
         |            ${msgId(d)},
         |            this.protos.${className(d)}.encodeDelimited(
         |                this.protos.${className(d)}.create({
         |                    ${sendMethodParameters(d).map(x ⇒ x + ": " + x).mkString(",")}
         |                })
         |            )
         |        );
         |    };
    """.stripMargin


    def sendMethodParameters(d: DescriptorProto) =
      d.getFieldList.map(f ⇒ fieldName(f))


    val messages = request.getProtoFileList
      .filter(f ⇒ request.getFileToGenerateList.contains(f.getName))
      .flatMap(f ⇒ f.getMessageTypeList
        .filter(m ⇒ m.hasOptions && m.getOptions.hasExtension(Options.msgid))
      )

    serverFile(messages, "")
  }

  def methodName(d: DescriptorProto) = uncapitalize(d.getName)

  def sendMethodName(d: DescriptorProto) = "send" + d.getName

}
