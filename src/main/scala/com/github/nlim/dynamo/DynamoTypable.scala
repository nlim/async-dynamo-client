package com.github.nlim.dynamo


trait DynamoTypable[A] {
  def typeId: String
}

object DynamoTypable {
  implicit object DynamoStringType extends DynamoTypable[String] {
    def typeId = "S"
  }

  implicit object DynamoStringSetType extends DynamoTypable[Set[String]] {
    def typeId = "SS"
  }
}
