package org.pico.msgpack

import org.pico.msgpack.model.Binary

case class Window(buffer: Array[Byte], start: Int, length: Int) {
  assert(start + length <= buffer.length)

  @inline
  final def drop(n: Int): Window = {
    val diff = length min n
    Window(buffer, start + diff, length - diff)
  }

  @inline
  final def take(n: Int): Window = Window(buffer, start, length min n)

  @inline
  final def headByte: Byte = buffer(start)

  @inline
  final def headShort: Short = {
    val v0 = (buffer(start + 0) << 8) & 0xff00
    val v1 = (buffer(start + 1) << 0) & 0x00ff
    (v0 | v1).toShort
  }

  @inline
  final def headInt: Int = {
    val v0 = (buffer(start + 0) << 24) & 0xff000000
    val v1 = (buffer(start + 1) << 16) & 0x00ff0000
    val v2 = (buffer(start + 2) <<  8) & 0x0000ff00
    val v3 = (buffer(start + 3) <<  0) & 0x000000ff
    v0 | v1 | v2 | v3
  }

  @inline
  final def headLong: Long =  {
    val v0 = (buffer(start + 0).toLong << 56) & 0xff00000000000000L
    val v1 = (buffer(start + 1).toLong << 48) & 0x00ff000000000000L
    val v2 = (buffer(start + 2).toLong << 40) & 0x0000ff0000000000L
    val v3 = (buffer(start + 3).toLong << 32) & 0x000000ff00000000L
    val v4 = (buffer(start + 4).toLong << 24) & 0x00000000ff000000L
    val v5 = (buffer(start + 5).toLong << 16) & 0x0000000000ff0000L
    val v6 = (buffer(start + 6).toLong <<  8) & 0x000000000000ff00L
    val v7 = (buffer(start + 7).toLong <<  0) & 0x00000000000000ffL
    (v0 | v1 | v2 | v3 | v4 | v5 | v6 | v7).toShort
  }

  @inline
  final def headFloat: Float = java.lang.Float.intBitsToFloat(headInt)

  @inline
  final def headDouble: Double = java.lang.Double.longBitsToDouble(headLong)

  @inline
  final def dropByte: Window = drop(1)

  @inline
  final def dropShort: Window = drop(2)

  @inline
  final def dropInt: Window = drop(4)

  @inline
  final def dropLong: Window = drop(8)

  @inline
  final def dropFloat: Window = drop(4)

  @inline
  final def dropDouble: Window = drop(8)

  @inline
  final def utf8String: String = new String(buffer, start, length, "UTF-8")

  @inline
  final def binary: Binary = Binary(this.take(length))

  @inline
  final def eof: Boolean = start == length
}
