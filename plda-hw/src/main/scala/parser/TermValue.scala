package parser

  sealed trait TermValue

  final case class TermBoolean(boolean: Boolean) extends TermValue

  final case class TermString(value: String) extends TermValue

  final case class TermNumber(value: Int) extends TermValue

  final case class TermArray(jss: List[TermValue]) extends TermValue

  final case class TermObject(properties: Map[String, TermValue]) extends TermValue
  /**
    * Not implemented on purpose. We will see why in our next lab :)
    */
  //final case object JsonNull extends JsonValue

  /**
    * Convenience methods to help construct these JsonValues, we could have
    * grouped just as easily in their respective companion objects as well
    */
  object TermValue {
    def apply(s: Boolean): TermBoolean = TermBoolean(s)

    def apply(s: String): TermString = TermString(s)

    def apply(b: Int): TermNumber = TermNumber(b)

    def apply(jss: TermValue*): TermArray = TermArray(jss.toList)

    def apply(props: (String, TermValue)*): TermObject = TermObject(props.toMap)
  }

