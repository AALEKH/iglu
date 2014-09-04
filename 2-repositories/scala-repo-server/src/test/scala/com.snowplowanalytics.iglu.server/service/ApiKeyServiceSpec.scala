/*
* Copyright (c) 2014 Snowplow Analytics Ltd. All rights reserved.
*
* This program is licensed to you under the Apache License Version 2.0, and
* you may not use this file except in compliance with the Apache License
* Version 2.0.  You may obtain a copy of the Apache License Version 2.0 at
* http://www.apache.org/licenses/LICENSE-2.0.
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the Apache License Version 2.0 is distributed on an "AS
* IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
* implied.  See the Apache License Version 2.0 for the specific language
* governing permissions and limitations there under.
*/
package com.snowplowanalytics.iglu.server
package test.service

// Java
import java.util.UUID

// Json4s
import org.json4s._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._

// Scala
import scala.concurrent.duration._

// Specs2
import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions

// Spray
import spray.http._
import StatusCodes._
import MediaTypes._
import spray.testkit.Specs2RouteTest

class ApiKeyServiceSpec extends Specification
  with Api with Specs2RouteTest with NoTimeConversions {

  def actorRefFactory = system

  implicit val routeTestTimeout = RouteTestTimeout(30 seconds)

  implicit val formats = DefaultFormats

  //case classes for json formatting
  case class ResApiKey(vendorPrefix: String, key: String, metadata: Metadata)
  case class Metadata(permission: String, createdAt: String)

  val uidRegex =
    "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}".r

  val superKey = "d0ca1d61-f6a8-4b40-a421-dbec5b9cdbad"
  val notSuperKey = "6eadba20-9b9f-4648-9c23-770272f8d627"
  val notUuidKey = "6ead20-9b9f-4648-9c23-770272f8d627"

  var readKey = ""
  var readKey2 = ""
  var writeKey = ""
  var writeKey2 = ""

  val start = "/api/apikeys/"

  val vendorPrefixUrl = s"${start}vendorprefixes/"
  val keyUrl = s"${start}keys/"

  val vendorPrefix = "com.test.dont.take.this"
  val otherVendorPrefix = "com.no.idea"
  val faultyVendorPrefix = "com.test.dont"

  sequential

  "ApiKeyGenService" should {

    "for POST requests" should {

      "return a 401 if the key provided is not super" in {
        Post(vendorPrefixUrl + vendorPrefix) ~>
        addHeader("api_key", notSuperKey) ~> sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("You do not have sufficient privileges")
        }
      }

      "return a 401 if the key provided is not an uuid" in {
        Post(vendorPrefixUrl + vendorPrefix) ~>
        addHeader("api_key", notUuidKey) ~> sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("The supplied authentication is invalid")
        }
      }

      """return a 200 with the keys if the vendor prefix is not colliding with
      anyone""" in {
        Post(vendorPrefixUrl + vendorPrefix) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === Created
          val response = responseAs[String]
          response must contain("read") and contain("write")

          val list = parse(response).extract[List[ResApiKey]]
          readKey = list.find(k => k.metadata.permission == "read") match {
            case Some(k) => k.key
            case None => ""
          }
          writeKey = list.find(k => k.metadata.permission == "write") match {
            case Some(k) => k.key
            case None => ""
          }

          readKey must beMatching(uidRegex)
          writeKey must beMatching(uidRegex)
        }
      }

      "return a 401 if the vendor prefix already exists" in {
        Post(vendorPrefixUrl + vendorPrefix) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("This vendor prefix is conflicting with an existing one")
        }
      }

      """return a 401 if the new vendor prefix is conflicting with an existing
      one""" in {
        Post(vendorPrefixUrl + faultyVendorPrefix) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("This vendor prefix is conflicting with an existing one")
        }
      }
    }

    "for PUT requests" should {

      "return a 401 if the key provided is not super" in {
        Put(vendorPrefixUrl + otherVendorPrefix) ~>
        addHeader("api_key", notSuperKey) ~> sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("You do not have sufficient privileges")
        }
      }

      "return a 401 if the key provided is not an uuid" in {
        Put(vendorPrefixUrl + otherVendorPrefix) ~>
        addHeader("api_key", notUuidKey) ~> sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("The supplied authentication is invalid")
        }
      }

      "return a 401 if the vendor prefix is conflicting" in {
        Put(vendorPrefixUrl + faultyVendorPrefix) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("This vendor prefix is conflicting with an existing one")
        }
      }

      "return a 201 if the prefix doesnt already exist" in {
        Put(vendorPrefixUrl + otherVendorPrefix) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === Created
          val response = responseAs[String]
          response must contain("read") and contain("write")

          val list = parse(response).extract[List[ResApiKey]]
          readKey2 = list.find(k => k.metadata.permission == "read") match {
            case Some(k) => k.key
            case None => ""
          }
          writeKey2 = list.find(k => k.metadata.permission == "write") match {
            case Some(k) => k.key
            case None => ""
          }

          readKey2 must beMatching(uidRegex)
          writeKey2 must beMatching(uidRegex)
        }
      }

      "return a 200 if both keys exist" in {
        Put(vendorPrefixUrl + otherVendorPrefix) ~>
        addHeader("api_Key", superKey) ~> sealRoute(routes) ~> check {
          val response = responseAs[String]
          val list = parse(response).extract[List[ResApiKey]]

          val newReadKey =
            list.find(k => k.metadata.permission == "read") match {
              case Some(k) => k.key
              case None => ""
            }
          val newWriteKey =
            list.find(k => k.metadata.permission == "write") match {
              case Some(k) => k.key
              case None => ""
            }

          newReadKey must beMatching(uidRegex)
          newReadKey must not be equalTo(readKey2)
          readKey2 = newReadKey
          newWriteKey must beMatching(uidRegex)
          newWriteKey must not be equalTo(writeKey2)
          writeKey2 = newWriteKey

          status === OK
          response must contain("read") and contain("write")
        }
      }

      "return a 200 if one key exists" in {
        Delete(keyUrl + readKey2) ~> addHeader("api_key", superKey)
        Put(vendorPrefixUrl + otherVendorPrefix) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          val response = responseAs[String]
          val list = parse(response).extract[List[ResApiKey]]

          val newReadKey =
            list.find(k => k.metadata.permission == "read") match {
              case Some(k) => k.key
              case None => ""
            }
          val newWriteKey =
            list.find(k => k.metadata.permission == "write") match {
              case Some(k) => k.key
              case None => ""
            }

          newReadKey must beMatching(uidRegex)
          newReadKey must not be equalTo(readKey2)
          newWriteKey must beMatching(uidRegex)
          newWriteKey must not be equalTo(writeKey2)

          status === OK
          response must contain("read") and contain("write")
        }
      }
    }

    "for GET requests with UUID" should {

      "return a 401 if the key provided is not super" in {
        Get(keyUrl + readKey) ~> addHeader("api_key", notSuperKey) ~>
        sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("You do not have sufficient privileges")
        }
      }

      "return a 404 if the key is not found" in {
        Get(keyUrl + UUID.randomUUID.toString) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === NotFound
          responseAs[String] must contain("API key not found")
        }
      }

      "return a 200 if the key is found and sufficient privileges" in {
        Get(keyUrl + readKey) ~> addHeader("api_key", superKey) ~>
        sealRoute(routes) ~> check {
          status === OK
          responseAs[String] must contain(readKey) and contain("read") and
            contain(vendorPrefix)
        }
      }
    }

    "for GET requests with vendor prefix" should {

      "return a 401 if the key provided is not super" in {
        Get(vendorPrefixUrl + vendorPrefix) ~>
        addHeader("api_key", notSuperKey) ~> sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("You do not have sufficient privileges")
        }
      }

      "return a 200 if there are keys associated with this vendor prefix" in {
        Get(vendorPrefixUrl + vendorPrefix) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === OK
          responseAs[String] must contain(readKey) and contain(writeKey) and
            contain(vendorPrefix)
        }
      }

      "return a 404 if there are no keys associated with this vendor prefix" in
      {
        Get(vendorPrefixUrl + faultyVendorPrefix) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === NotFound
          responseAs[String] must contain("Vendor prefix not found")
        }
      }
    }

    "for DELETE requests with UUID" should {

      "return a 401 if the key provided is not super" in {
        Delete(keyUrl + readKey) ~> addHeader("api_key", notSuperKey) ~>
        sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("You do not have sufficient privileges")
        }
      }

      "return a 404 if the key is not found" in {
        Delete(keyUrl + UUID.randomUUID.toString) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === NotFound
          responseAs[String] must contain("API key not found")
        }
      }

      "return a 200 if the key is found and sufficient privileges" in {
        Delete(keyUrl + readKey) ~> addHeader("api_key", superKey) ~>
        sealRoute(routes) ~> check {
          status === OK
          responseAs[String] must contain("API key successfully deleted")
        }
      }
    }

    "for DELETE requests with vendor prefix" should {

      "return a 401 if the key provided is not super" in {
        Delete(vendorPrefixUrl + vendorPrefix) ~>
        addHeader("api_key", notSuperKey) ~> sealRoute(routes) ~> check {
          status === Unauthorized
          responseAs[String] must
            contain("You do not have sufficient privileges")
        }
      }

      "return a 200 if there are keys associated with this vendor prefix" in {
        Delete(vendorPrefixUrl + vendorPrefix) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === OK
          responseAs[String] must contain("API key deleted for ")
        }
      }

      "return a 404 if there are no keys associated with this vendor prefix" in
      {
        Delete(vendorPrefixUrl + vendorPrefix) ~>
        addHeader("api_key", superKey) ~> sealRoute(routes) ~> check {
          status === NotFound
          responseAs[String] must contain("Vendor prefix not found")
        }
      }
    }
  }
}
