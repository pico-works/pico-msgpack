package org.pico.msgpack.std

import org.pico.msgpack._
import org.pico.msgpack.model.Binary
import org.pico.msgpack.syntax._

package object anyVal {
  implicit val msgDecode_Boolean = new MsgDecode[Boolean] {
    override def decode(window: Window): MsgDecodeResult[Boolean] = {
      window.headByte match {
        case 0xc2 => MsgDecodeOk(false, window.dropByte)
        case 0xc3 => MsgDecodeOk(true , window.dropByte)
        case _    => MsgDecodeError("Not a boolean")
      }
    }
  }

  implicit val msgDecode_Long = new MsgDecode[Long] {
    override def decode(window: Window): MsgDecodeResult[Long] = {
      val intro = window.headByte

      if ((intro & 0x80) == 0x00) {
        MsgDecodeOk(intro, window.dropByte)
      } else if ((intro & 0xe0) == 0xe0) {
        // TODO Is this twos complement?
        MsgDecodeOk(intro, window.dropByte)
      } else {
        val dataWindow = window.dropByte

        if (intro == 0xcc) {
          MsgDecodeOk(dataWindow.headByte, dataWindow.dropByte)
        } else if (intro == 0xcd) {
          MsgDecodeOk(dataWindow.headShort, dataWindow.dropShort)
        } else if (intro == 0xce) {
          MsgDecodeOk(dataWindow.headInt, dataWindow.dropInt)
        } else if (intro == 0xcf) {
          val dataWindow  = window.dropByte

          // TODO Is sign handled appropriately?
          MsgDecodeOk(dataWindow.headLong, dataWindow.dropLong)
        } else if (intro == 0xd0) {
          MsgDecodeOk(dataWindow.headByte, dataWindow.dropByte)
        } else if (intro == 0xd1) {
          MsgDecodeOk(dataWindow.headShort, dataWindow.dropShort)
        } else if (intro == 0xd2) {
          MsgDecodeOk(dataWindow.headInt, dataWindow.dropInt)
        } else if (intro == 0xd3) {
          val dataWindow  = window.dropByte

          // TODO Is sign handled appropriately?
          MsgDecodeOk(dataWindow.headLong, dataWindow.dropLong)
        } else {
          MsgDecodeError("Not a Long")
        }
      }
    }
  }

  implicit val msgDecode_Double = new MsgDecode[Double] {
    override def decode(window: Window): MsgDecodeResult[Double] = {
      val intro = window.headByte
      val dataWindow = window.dropByte

      if (intro == 0xca) {
        val dataWindow  = window.dropByte
        MsgDecodeOk(dataWindow.headFloat, dataWindow.dropFloat)
      } else if (intro == 0xcb) {
        val dataWindow  = window.dropByte
        MsgDecodeOk(dataWindow.headDouble, dataWindow.dropDouble)
      } else {
        MsgDecodeError("Not a Double")
      }
    }
  }

  implicit val msgDecode_String = new MsgDecode[String] {
    override def decode(window: Window): MsgDecodeResult[String] = {
      val intro = window.headByte

      if ((intro & 0xe0) == 0xa0) {
        val length = intro & 0x1f
        MsgDecodeOk(window.take(length).utf8String, window.drop(length))
      } else {
        val lengthWindow = window.dropByte

        if (intro == 0xd9) {
          val length = lengthWindow.headByte
          MsgDecodeOk(lengthWindow.take(length).utf8String, lengthWindow.drop(length))
        } else if (intro == 0xda) {
          val length = lengthWindow.headShort
          MsgDecodeOk(lengthWindow.take(length).utf8String, lengthWindow.drop(length))
        } else if (intro == 0xdb) {
          val length = lengthWindow.headInt
          MsgDecodeOk(lengthWindow.take(length).utf8String, lengthWindow.drop(length))
        } else {
          MsgDecodeError("Not a String")
        }
      }
    }
  }

  implicit val msgDecode_Binary = new MsgDecode[Binary] {
    override def decode(window: Window): MsgDecodeResult[Binary] = {
      val intro = window.headByte

      if ((intro & 0xe0) == 0xa0) {
        val length = intro & 0x1f
        MsgDecodeOk(window.take(length).binary, window.drop(length))
      } else {
        val lengthWindow = window.dropByte

        if (intro == 0xd9) {
          val length = lengthWindow.headByte
          MsgDecodeOk(lengthWindow.take(length).binary, lengthWindow.drop(length))
        } else if (intro == 0xda) {
          val length = lengthWindow.headShort
          MsgDecodeOk(lengthWindow.take(length).binary, lengthWindow.drop(length))
        } else if (intro == 0xdb) {
          val length = lengthWindow.headInt
          MsgDecodeOk(lengthWindow.take(length).binary, lengthWindow.drop(length))
        } else {
          MsgDecodeError("Not a String")
        }
      }
    }
  }

  implicit def msgDecode_List[A: MsgDecode] = new MsgDecode[List[A]] {
    override def decode(window: Window): MsgDecodeResult[List[A]] = {
      val intro = window.headByte

      if ((intro & 0xf0) == 0x90) {
        val length = intro & 0x0f
        val dataWindow = window.dropByte

        // TODO Return better error
        val result = (Option(dataWindow -> List.empty[A]) /: (0 until length)) { case (acc, b) =>
          acc match {
            case Some((elemWindow, es)) =>
              msgDecode[A](elemWindow) match {
                case MsgDecodeOk(e, remainder)    => Some(remainder -> (e :: es))
                case MsgDecodeError(error)  => None
              }
            case None => None
          }
        }

        result match {
          case Some((pos, es)) => MsgDecodeOk(es.reverse, pos)
          case None => MsgDecodeError("Unable to decode List")
        }
      } else {
        if (intro == 0xd9) {
          val lengthWindow = window.dropByte
          val length = lengthWindow.headByte
          val dataWindow = lengthWindow.dropByte

          val result = (Option(dataWindow -> List.empty[A]) /: (0 until length)) { case (acc, b) =>
            acc match {
              case Some((elemWindow, es)) =>
                msgDecode[A](elemWindow) match {
                  case MsgDecodeOk(e, remainder)    => Some(remainder -> (e :: es))
                  case MsgDecodeError(error)  => None
                }
              case None => None
            }
          }

          result match {
            case Some((pos, es)) => MsgDecodeOk(es.reverse, pos)
            case None => MsgDecodeError("Unable to decode List")
          }
        } else if (intro == 0xda) {
          val lengthWindow = window.dropByte
          val length = lengthWindow.headShort
          val dataWindow = lengthWindow.dropShort

          val result = (Option(dataWindow -> List.empty[A]) /: (0 until length)) { case (acc, b) =>
            acc match {
              case Some((elemWindow, es)) =>
                msgDecode[A](elemWindow) match {
                  case MsgDecodeOk(e, remainder)    => Some(remainder -> (e :: es))
                  case MsgDecodeError(error)  => None
                }
              case None => None
            }
          }

          result match {
            case Some((pos, es)) => MsgDecodeOk(es.reverse, pos)
            case None => MsgDecodeError("Unable to decode List")
          }
        } else if (intro == 0xdb) {
          val lengthWindow = window.dropByte
          val length = lengthWindow.headInt
          val dataWindow = lengthWindow.dropInt

          val result = (Option(dataWindow -> List.empty[A]) /: (0 until length)) { case (acc, b) =>
            acc match {
              case Some((elemWindow, es)) =>
                msgDecode[A](elemWindow) match {
                  case MsgDecodeOk(e, remainder)    => Some(remainder -> (e :: es))
                  case MsgDecodeError(error)  => None
                }
              case None => None
            }
          }

          result match {
            case Some((pos, es)) => MsgDecodeOk(es.reverse, pos)
            case None => MsgDecodeError("Unable to decode List")
          }
        } else {
          MsgDecodeError("Not a Double")
        }
      }
    }
  }

  implicit def msgDecode_Option[A: MsgDecode] = new MsgDecode[Option[A]] {
    override def decode(window: Window): MsgDecodeResult[Option[A]] = {
      window.headByte match {
        case 0xc0 => MsgDecodeOk(None, window.dropByte)
        case _    => msgDecode[A](window).map(Some(_))
      }
    }
  }
}
