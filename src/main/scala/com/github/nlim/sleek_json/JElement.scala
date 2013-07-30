package com.github.nlim.sleek_json



sealed trait JElement {
 def toJson: String = Json.toJson(this)
}

case object JNull extends JElement

case class JArray(value: Vector[JElement]) extends JElement {
  def get(i: Int) = value(i)

  def ++(other: JArray): JArray = JArray(value ++ other.value)

  def :+(elem: JElement): JArray = JArray(value :+ elem)
  def +:(elem: JElement): JArray = JArray(elem +: value)
}

case class JObject(value: Map[String, JElement]) extends JElement {
  def keys = value.keySet

  def keyValues = value.toSeq

  def apply(k: String): JElement = value(k)

  def get(k: String): Option[JElement] = value.get(k)

  def ++(other: JObject): JObject = JObject(value ++ other.value)

  def +(t: (String, JElement)): JObject = JObject(value + t)

  def -(t: String): JObject = JObject(value - t)
}

case class JInteger(value: Integer) extends JElement

case class JString(value: String) extends JElement

case class JDouble(value: java.lang.Double) extends JElement

case class JFloat(value: java.lang.Float) extends JElement

case class JBoolean(value: Boolean) extends JElement



