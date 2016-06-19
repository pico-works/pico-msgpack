package org.pico.msgpack

trait MsgDecode[+A] {
  def decode(window: Window): MsgDecodeResult[A]
}
