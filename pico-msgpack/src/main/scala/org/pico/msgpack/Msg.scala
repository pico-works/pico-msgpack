package org.pico.msgpack

sealed trait Msg

case object NilMsg extends Msg

case class LongMsg(value: Long) extends Msg

case class DoubleMsg(value: Double) extends Msg

case class StringMsg(value: String) extends Msg

case class BinaryMsg(buffer: Array[Byte], offset: Int, length: Int) extends Msg

case class ArrayMsg(buffer: Array[Byte], count: Int) extends Msg
