package com.github.nlim.sleek_json


trait JElementizer[D] {
  def toJson(x: D): JElement
}

object JElementizer {
  implicit object StringJson extends JElementizer[String] {
    def toJson(sa: String) = JString(sa)
  }

  implicit object StringSetJson extends JElementizer[Set[String]] {
    def toJson(ssa: Set[String]) = JArray(ssa.map(JString(_)).toVector)
  }
}
