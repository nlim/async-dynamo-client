package com.github.nlim.dynamo

import com.github.nlim.sleek_json.{JString, JElement, JObject}

// The general UpdateDynamoAction trait for specifying
// PUT or ADD actions to perform on some DynamoAttribute
sealed trait UpdateDynamoAction[A] {
  type D = DynamoAttribute[A]
  def action: String
  def attribute: D
}

case class PutAction[A](attribute: DynamoAttribute[A]) extends UpdateDynamoAction[A] {
  val action = "PUT"
}

case class AddAction[A](attribute: DynamoAttribute[A]) extends UpdateDynamoAction[A] {
  val action = "ADD"
}

/**
 * This is a companion object (like a holder of Java static methods)
 * that holds functions for manipulating this UpdateDynamoAction type
 */
object UpdateDynamoAction {

  /**
   * A Helper function for generating the Json serializable Map
   * that's used when constructing the Dynamo query that runs update actions
   */

  def updateMap(udas: Seq[UpdateDynamoAction[_]]): JObject = {
    val m = udas.foldLeft(JObject(Map[String, JElement]())) { (result, a) =>
      result + (
        a.attribute.key ->
        JObject(Map(
          "Action" -> JString(a.action),
          "Value" -> JObject(Map(a.attribute.typeId -> a.attribute.jsonize))
        )))
    }
    JObject(Map("AttributeUpdates" -> m))
  }
}
