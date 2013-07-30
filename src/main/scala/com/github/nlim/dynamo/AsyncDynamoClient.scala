package com.github.nlim.dynamo

import com.github.nlim.sleek_json._
import Json._
import com.amazonaws.auth.{AWSCredentials, AWS4Signer}
import com.amazonaws.DefaultRequest
import java.net.URI
import concurrent.Future
import play.api.libs.ws.WS
import scala.collection.JavaConversions.mapAsScalaMap
import DynamoAttribute._
import UpdateDynamoAction._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import java.nio.charset.Charset
import java.io.InputStream
import org.apache.commons.io.IOUtils
import com.github.nlim.sleek_json.JObject
import scala.Some
import com.github.nlim.sleek_json.JString

/**
 * An AsyncDynamoClient backed by the Play WS API (which is backed by Java NIO)
 */
class AsyncDynamoClient(creds: AWSCredentials, tableName: String, timeOutMillis: Long) {

  /**
   * Runs the Dynamo GetItem command which uses a sequence of DynamoAttributes
   * for keys and returns a Future[Try[DynamoAttributesMap]] which will contain
   * all of the typed attributes from Dynamo
   */
  def getItem(keys: DynamoAttribute[_]*): Future[DynamoAttributesMap] = {
    val params = getItemParams(keyMap(keys))

    transformedResponse(params)(parseAttributes(_, "Item"))
  }

  /**
   * Runs the Dynamo DeleteItem command which uses a variable sequence
   * of DynamoAttributes as keys, and produces a Future[Try[DynamoAttributesMap]]
   * which holds nothing I believe unless we provide the option have Dynamo
   * hand back the attributes of the old key that we just deleted.
   */
  def deleteItem(keys: DynamoAttribute[_]*): Future[DynamoAttributesMap] = {
    val params = deleteItemParams(keyMap(keys))

    transformedResponse(params)(parseAttributes(_, "Attributes"))
  }

  /**
   * Runs the Dynamo UpdateItem command which uses a sequence of DynamoAttributes
   * for keys and a sequence of UpdateDynamoActions for PUT and ADD actions.
   */
  def updateItem(keys: Seq[DynamoAttribute[_]], values: Seq[UpdateDynamoAction[_]]): Future[DynamoAttributesMap] = {
    val params = updateItemParams(keyMap(keys) ++ updateMap(values))

    transformedResponse(params)(parseAttributes(_, "Attributes"))
  }

  /**
   * Runs the Dynamo DescribeTable command and
   * and provides a Future[Try[JsonNode]] of the table description
   *
   */
  def describeTable: Future[JObject] = transformedResponse(DescribeTableVersion, tableNameMap)(identity)


  // =================== Private Methods =============================

  private def getItemParams(keyBody: JObject) =
    (GetItemVersion, tableNameMap ++ keyBody ++ JObject(Map("ConsistentRead" -> JBoolean(false))))

  private def deleteItemParams(deleteBody: JObject) =
    (DeleteItemVersion, tableNameMap ++ deleteBody)

  private def updateItemParams(updateBody: JObject) =
    (UpdateItemVersion, tableNameMap ++ updateBody ++ JObject(Map("ReturnValues" -> JString("ALL_NEW"))))

  /**
   * This helper function takes in the target ~ Dynamo command, and a
   * Map structure with which to query Dynamo, runs the request
   * and parses the response to Json
   */
  private def transformedResponse[A](tup: (String, JObject))(f: JObject => A): Future[A] =
    request(tup._1, tup._2) map f

  // In all the Dynamo Queries, and TableName is specified
  private def tableNameMap = JObject(Map("TableName" -> JString(tableName)))

  /**
   * This is the Bread-n-Butter helper function that constructs an
   * authenticated POST request for a given Dynamo target and
   * uses the Play WS API to make an asynchronous Http Request
   * to the Amazon Dynamo DB Service
   */
  private def request[A](target: String, jo: JObject): Future[JObject] = {

    val body = toJson(jo)

    val headers = {
      // Uses the Amazon Java SDK to simply to obtain the signed headers
      // No request made here
      val r = new DefaultRequest[String](serviceName)
      r.setEndpoint(new URI(dynamoEndpoint))
      r.setContent(toInputStream(body))
      signer.sign(r, creds)
      mapAsScalaMap(r.getHeaders).toSeq
    } ++ Seq(("X-Amz-Target", target), ("Content-Type", awsContentType))

    val requestHolder = WS.url(dynamoEndpoint).withHeaders(headers: _*).withTimeout(timeOutMillis.toInt)

    requestHolder.post(body) flatMap { r =>
      parseJson(r.body) >>: OBJ match {
        case Some(obj) => Future.successful(obj)
        case _ => Future.failed(new RuntimeException("Dynamo didn't return a JObject"))
      }
    }
  }

  // A private helper to obtain an InputStream from a String
  private val toInputStream: String => InputStream = s => IOUtils.toInputStream(s, Charset.forName("UTF-8"))

  // The AWS service we are using!
  private val serviceName = "dynamodb"

  // The Region
  private val regionId = "us-east-1"

  // The required content type to put in the header of the request
  private val awsContentType = "application/x-amz-json-1.0"

  private val dynamoEndpoint = "http://dynamodb.us-east-1.amazonaws.com/"

  // The target for the Dynamo GetItem command
  private val GetItemVersion = "DynamoDB_20120810.GetItem"

  // The target for the Dynamo DescribeTable command
  private val DescribeTableVersion = "DynamoDB_20120810.DescribeTable"

  // The target for the Dynamo UpdateItem command
  private val UpdateItemVersion = "DynamoDB_20120810.UpdateItem"

  // The target for the Dynamo DeleteItem command
  private val DeleteItemVersion = "DynamoDB_20120810.DeleteItem"

  // The entity used to to sign (authenticate) the web request
  private val signer = {
    val s = new AWS4Signer;
    s.setServiceName(serviceName)
    s.setRegionName(regionId)
    s
  }
  */
}
