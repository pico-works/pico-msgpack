package org.pico.msgpack

package object syntax {
  @inline
  final def msgDecode[A: MsgDecode](window: Window): MsgDecodeResult[A] = {
    implicitly[MsgDecode[A]].decode(window)
  }
}
