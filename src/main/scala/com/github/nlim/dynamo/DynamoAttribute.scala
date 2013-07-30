package com.github.nlim.dynamo

import com.github.nlim.sleek_json._
import com.github.nlim.sleek_json.Json._
import scala.Some

// The general DynamoAttribute trait, holding some abstract type A
sealed trait DynamoAttribute[A] {
  def value: A
  def jElementizer: JElementizer[A]
  def jsonize = jElementizer.toJson(value)
  def key: String
  def typeId: String
}


/**
 * In Scala it is customary that whenever a new type is defined
 * (like the above DynamoAttribute) that you also define what is called
 * a companion object, which really acts like a module, that holds
 * some functions to help manipulate this new type
 *
 * In this case, the DynamoAttribute companion object holds
 * functions for parsing results and creating Key Maps.
 */
object DynamoAttribute {

  def apply[A](k: String, v: A)(implicit typable: DynamoTypable[A],
                                         je: JElementizer[A]): DynamoAttribute[A] = {
    new DynamoAttribute[A] {
      def key: String = k

      def value: A = v

      def typeId: String = typable.typeId

      def jElementizer: JElementizer[A] = je
    }
  }

  def unapply[A](da: DynamoAttribute[A]): Option[(String, A)] =  Some((da.key, da.value))

  // Some commonly used type aliases
  type DynamoAttributesMap = Map[String, DynamoAttribute[_]]

  type DynamoAttributes = Seq[DynamoAttribute[_]]

  def emptyMap: DynamoAttributesMap = Map.empty[String, DynamoAttribute[_]]

  def emptySeq: DynamoAttributes = Seq.empty[DynamoAttribute[_]]

  // Implicit Conversion for using the Java JsonNode here in Scala
  import scala.collection.JavaConversions.{ asScalaIterator }

  /**
   * Takes in a JsonNode and the Key to find the attributes info
   * and constructs the DynamoAttributesMap
   *
   * Using immutable maps here is efficient because of structure reuse
   * http://docs.scala-lang.org/overviews/collections/performance-characteristics.html
   */

  val SS = KeyAndValue("SS", ARR)
  val S = KeyAndValue("S", STRING)

  def parseAttributes(obj: JObject, attrKey: String): DynamoAttributesMap = {
    (obj >>: attrKey ~> OBJ) map { item =>
      // Fold over the Key-Value pairs of the JsonNode to build
      // up the DynamoAttributesMap
      item.keyValues.foldLeft(emptyMap) { case (attributesSoFar: DynamoAttributesMap, (key: String, value: JElement)) =>
        (value >>: OBJ).map {
          case S((k, s))    => attributesSoFar + (key -> DynamoAttribute(k, s))
          case SS((k, arr)) => attributesSoFar + (key -> DynamoAttribute(k, arr))
          case _ => attributesSoFar
        } getOrElse {
          attributesSoFar
        }
      }
    } getOrElse {
      emptyMap
    }
  }

  /**
   * A Helper function for generating the Json serializable Map
   * that's used to create the key to query Dynamo with.
   */
  def keyMap(attributes: DynamoAttributes): JObject = {
    val key = attributes.foldLeft(JObject(Map[String, JElement]())) { (result, a) =>
      result + (a.key -> JObject(Map(a.typeId -> a.jsonize)))
    }
    JObject(Map("Key" -> key))
  }
}
