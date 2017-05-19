//       ___       ___       ___       ___       ___
//      /\  \     /\__\     /\__\     /\  \     /\__\
//     /::\  \   /:/ _/_   /:| _|_   /::\  \   /:/  /
//    /::\:\__\ /::-"\__\ /::|/\__\ /::\:\__\ /:/__/
//    \;:::/  / \;:;-",-" \/|::/  / \;:::/  / \:\  \
//     |:\/__/   |:|  |     |:/  /   |:\/__/   \:\__\
//      \|__|     \|__|     \/__/     \|__|     \/__/

package ru.rknrl.rpc {
import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.ProgressEvent;
import flash.net.Socket;
import flash.utils.ByteArray;
import flash.utils.Endian;

public class TcpConnection extends EventDispatcher {
    private static const HEADER_SIZE:int = 4 + 4; // msgid (int) + size (int)
    protected const byteArray:ByteArray = new ByteArray();
    protected var socket:Socket;
    private var destoyed:Boolean = false;
    public var verbose:Boolean = false;

    public function TcpConnection(socket:Socket):void {
        this.socket = socket;
        socket.addEventListener(ProgressEvent.SOCKET_DATA, onSocketData);
        socket.endian = byteArray.endian = Endian.LITTLE_ENDIAN;
    }

    public function destroy():void {
        destoyed = true;
        socket.removeEventListener(ProgressEvent.SOCKET_DATA, onSocketData);
    }

    protected var msgId:int = -1;
    protected var size:int = HEADER_SIZE;

    private function onSocketData(event:ProgressEvent):void {
        while (!destoyed && socket.bytesAvailable >= size) {
            if (msgId == -1) {
                msgId = socket.readInt();
                size = socket.readInt();
            } else {
                parsePacket();
                msgId = -1;
                size = HEADER_SIZE;
            }
        }
    }

    protected function parsePacket():void {
        throw new Error("override me")
    }

    protected function send(msgId:int, byteArray:ByteArray):void {
        if (destoyed) return;

        socket.writeInt(msgId);
        socket.writeInt(byteArray.length);
        socket.writeBytes(byteArray);
        socket.flush();
    }

    override public function dispatchEvent(event:Event):Boolean {
        if (verbose) {
            trace(event);
        }
        return super.dispatchEvent(event);
    }
}
}
