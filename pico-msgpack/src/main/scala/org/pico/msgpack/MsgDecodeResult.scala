package org.pico.msgpack

sealed trait MsgDecodeResult[@specialized(Int, Long, Boolean, Double) +A] {
  def map[B](f: A => B): MsgDecodeResult[B]
}

case class MsgDecodeError(error: String) extends MsgDecodeResult[Nothing] {
  override def map[B](f: Nothing => B): MsgDecodeResult[B] = this
}

case class MsgDecodeOk[@specialized(Int, Long, Boolean, Double) A](
    value: A,
    remainder: Window) extends MsgDecodeResult[A] {
  override def map[B](f: A => B): MsgDecodeResult[B] = MsgDecodeOk(f(value), remainder)
}
