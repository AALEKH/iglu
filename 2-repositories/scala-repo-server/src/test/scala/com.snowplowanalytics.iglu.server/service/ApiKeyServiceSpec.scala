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

  val uidRegex =
    "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}".r

  val superKey = "d0ca1d61-f6a8-4b40-a421-dbec5b9cdbad"
  val notSuperKey = "6eadba20-9b9f-4648-9c23-770272f8d627"
  val notUuidKey = "6ead20-9b9f-4648-9c23-770272f8d627"

  var readKey = ""
  var writeKey = ""

  val start = "/api/apikeys/"

  val vendorPrefixUrl = s"${start}vendorprefixes/"
  val keyUrl = s"${start}keys/"

  val vendorPrefix = "com.test.dont.take.this"
  val faultyVendorPrefix = "com.test.dont"
  val vendorPrefix2 = "com.unittest"
  val faultyVendorPrefix2 = "com.unit"
  val vendorPrefix3 = "com.no.idea"
  val faultyVendorPrefix3 = "com.no"

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
          val map = parse(response).extract[Map[String, String]]
          readKey = map getOrElse("read", "")
          writeKey = map getOrElse("write", "")
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
