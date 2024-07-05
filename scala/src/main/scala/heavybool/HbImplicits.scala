package heavybool
import heavybool.HeavyBool.{HTrue, HFalse}

object HbImplicits {

  import scala.language.implicitConversions

  implicit def boolToHeavyBool(raw: Boolean):HeavyBool = {
    raw match {
      case true => HTrue
      case false => HFalse
    }
  }

  implicit def HeavyBoolToBool(raw:HeavyBool):Boolean = {
    raw.toBoolean
  }
}
