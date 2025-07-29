package spinal.core.internals

/**
 * Configurable design aspects for templates
 */
abstract class Configurable {
  var parameters : List[Any] = List()
}

object AllBooleans extends Configurable {
  parameters = List(true, false)
}

object TrueBoolean extends Configurable {
  parameters = List(true)
}

object FalseBoolean extends Configurable {
  parameters = List(false)
}

case class Integer(i : Int) extends Configurable {
  parameters = List(i)
}

case class IntegerList(p : List[Int]) extends Configurable {
  parameters = p
}

case class StringList(p : List[String]) extends Configurable {
  parameters = p
}