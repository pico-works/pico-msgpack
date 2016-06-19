package org.pico.msgpack.model

sealed trait Msg

case class BinaryMsg(value: Array[Byte]) extends Msg

case class ByteMsg(value: Byte) extends Msg

case class ShortMsg(value: Short) extends Msg

case class IntMsg(value: Int) extends Msg

case class LongMsg(value: Long) extends Msg

case class FloatMsg(value: Float) extends Msg

case class DoubleMsg(value: Double) extends Msg

case class StringMsg(value: String) extends Msg

case class MapMsg(map: Map[Msg, Msg]) extends Msg
