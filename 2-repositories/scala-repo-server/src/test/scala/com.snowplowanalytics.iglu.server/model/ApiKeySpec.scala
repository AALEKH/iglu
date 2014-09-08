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
package test.model

// This project
import model.ApiKeyDAO
import util.IgluPostgresDriver.simple._

// Java
import java.util.UUID

// Json4s
import org.json4s._
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._

// Slick
import scala.slick.driver.JdbcDriver.backend.Database.dynamicSession
import scala.slick.jdbc.{ StaticQuery => Q }

// Specs2
import org.specs2.mutable.Specification

// Spray
import spray.http.StatusCodes._

class ApiKeySpec extends Specification with SetupAndDestroy {

  val apiKey = new ApiKeyDAO(database)

  //case classes for json formatting
  case class ResApiKey(vendorPrefix: String, key: String, metadata: Metadata)
  case class Metadata(permission: String, createdAt: String)

  implicit val formats = DefaultFormats

  val tableName = "apikeys"
  val vendorPrefix = "com.unittest"
  val otherVendorPrefix = "com.benben"
  val faultyVendorPrefix = "com.unit"
  val notUid = "this-is-not-an-uuid"

  var readKey = ""
  var readKey2 = ""
  var writeKey = ""
  var writeKey2 = ""

  sequential

  "ApiKeyDAO" should {

    "for initTable" should {

      "create the apikeys table" in {
        apiKey.initTable
        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from pg_catalog.pg_tables
            where tablename = '${tableName}';""").first === 1
        }
      }

      "create the super api key" in {
        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where permission = 'super';""").first === 1
        }
      }
    }

    "for addReadWrite" should {

      "return a 201 and add the API keys properly" in {
        val (status, res) = apiKey.addReadWrite(vendorPrefix)
        val list = parse(res).extract[List[ResApiKey]]

        readKey = list.find(k => k.metadata.permission == "read") match {
          case Some(k) => k.key
          case None => ""
        }
        writeKey = list.find(k => k.metadata.permission == "write") match {
          case Some(k) => k.key
          case None => ""
        }

        status === Created
        res must contain("read") and contain("write")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${readKey}';""").first === 1
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${writeKey}';""").first === 1
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${vendorPrefix}';""").first === 2
        }
      }

      "return a 401 and not add API keys if the vendor prefix is conflicting" +
      "with an existing one" in {
        val (status, res) = apiKey.addReadWrite(faultyVendorPrefix)
        status === Unauthorized
        res must
          contain("This vendor prefix is conflicting with an existing one")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${faultyVendorPrefix}';""").first === 0
        }
      }
    }

    "for regenerate" should {

      "return a 401 if the vendor prefix is conflicting" in {
        val (status, res) = apiKey.regenerate(faultyVendorPrefix)
        status === Unauthorized
        res must
          contain("This vendor prefix is conflicting with an existing one")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${faultyVendorPrefix}';""").first === 0
        }
      }

      "return a 201 if the prefix doesnt already exist" in {
        val (status, res) = apiKey.regenerate(otherVendorPrefix)
        val list = parse(res).extract[List[ResApiKey]]

        readKey2 = list.find(k => k.metadata.permission == "read") match {
          case Some(k) => k.key
          case None => ""
        }
        writeKey2 = list.find(k => k.metadata.permission == "write") match {
          case Some(k) => k.key
          case None => ""
        }

        status === Created
        res must contain("read") and contain("write")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${readKey2}';""").first === 1
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${writeKey2}';""").first === 1
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${otherVendorPrefix}';""").first === 2
        }
      }

      "return a 200 if both keys exist" in {
        val (status, res) = apiKey.regenerate(otherVendorPrefix)
        val list = parse(res).extract[List[ResApiKey]]

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

        newReadKey must not be equalTo(readKey2)
        readKey2 = newReadKey
        newWriteKey must not be equalTo(writeKey2)
        writeKey2 = newWriteKey

        status === OK
        res must contain("read") and contain("write")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${readKey2}';""").first === 1
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${writeKey2}';""").first === 1
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${otherVendorPrefix}';""").first === 2
        }
      }

      "return a 200 if one key exists" in {
        apiKey.delete(UUID.fromString(readKey2))

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${otherVendorPrefix}';""").first === 1
        }

        val (status, res) = apiKey.regenerate(otherVendorPrefix)
        val list = parse(res).extract[List[ResApiKey]]

        val newReadKey = list.find(k => k.metadata.permission == "read") match {
          case Some(k) => k.key
          case None => ""
        }
        val newWriteKey =
          list.find(k => k.metadata.permission == "write") match {
            case Some(k) => k.key
            case None => ""
          }

        newReadKey must not be equalTo(readKey2)
        readKey2 = newReadKey
        newWriteKey must not be equalTo(writeKey2)
        writeKey2 = newWriteKey

        status === OK
        res must contain("read") and contain("write")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${readKey2}';""").first === 1
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${writeKey2}';""").first === 1
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${otherVendorPrefix}';""").first === 2
        }
      }
    }

    "for get (auth)" should {

      "properly retrieve the API key" in {
        apiKey.get(readKey) match {
          case Some((vendorPrefix, permission)) =>
            vendorPrefix must contain(vendorPrefix)
            permission must contain("read")
          case _ => failure
        }

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${readKey}';""").first === 1
        }
      }

      "return None if the API key is not in the db" in {
        val uid = UUID.randomUUID.toString
        apiKey.get(uid) match {
          case None => success
          case _ => failure
        }

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${uid}';""").first === 0
        }
      }

      "return None if the API key is not a uuid" in {
        apiKey.get(notUid) match {
          case None => success
          case _ => failure
        }
      }
    }

    "for get" should {

      "return a 200 and info about the api key" in {
        val (status, res) = apiKey.get(List(UUID.fromString(readKey)))
        status === OK
        res must contain(readKey) and contain("read") and contain(vendorPrefix)

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${readKey}';""").first === 1
        }
      }

      "return a 200 and info about the api keys" in {
        val (status, res) =
          apiKey.get(List(UUID.fromString(readKey), UUID.fromString(writeKey)))
        status === OK
        res must contain(readKey) and contain("read") and
          contain(writeKey) and contain("write") and
          contain(vendorPrefix)

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${readKey}' or uid = '${writeKey}';""").first === 2
        }
      }

      "return a 404 if the key is not in the db" in {
        val uid = UUID.randomUUID
        val (status, res) = apiKey.get(List(uid))
        status === NotFound
        res must contain("API key not found")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${uid.toString}';""").first === 0
        }
      }
    }

    "for getFromVendorPrefix" should {

      "return a 200 and info about the api keys" in {
        val (status, res) = apiKey.getFromVendorPrefix(List(vendorPrefix))
        status === OK
        res must contain(readKey) and contain(writeKey) and
          contain(vendorPrefix)

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${vendorPrefix}';""").first === 2
        }
      }

      "return a 200 and info about the api keys for those vendor prefixes" in {
        val (status, res) =
          apiKey.getFromVendorPrefix(List(vendorPrefix, otherVendorPrefix))
        status === OK
        res must contain(vendorPrefix) and contain(otherVendorPrefix) and
          contain(readKey) and contain(writeKey) and
          contain(readKey2) and contain(writeKey2)

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${vendorPrefix}' or
              vendor_prefix = '${otherVendorPrefix}';""").first === 4
        }
      }

      "return a 404 if there are no api keys associated with this vendor" in {
        val (status, res) = apiKey.getFromVendorPrefix(List(faultyVendorPrefix))
        status === NotFound
        res must contain("Vendor prefix not found")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${faultyVendorPrefix}';""").first === 0
        }
      }
    }

    "for delete" should {

      "return a 200 and delete an API key" in {
        val (status, res) = apiKey.delete(UUID.fromString(readKey))
        status === OK
        res must contain("API key successfully deleted")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${readKey}';""").first === 0
        }
      }

      "return a 404 if the API key is not in the database" in {
        val (status, res) = apiKey.delete(UUID.fromString(readKey))
        status === NotFound
        res must contain("API key not found")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where uid = '${readKey}';""").first === 0
        }
      }
    }

    "for deleteFromVendorPrefix" should {

      "return a 200 and delete API keys associated with a vendor prefix" in {
        val (status, res) = apiKey.deleteFromVendorPrefix(vendorPrefix)
        status === OK
        res must contain("API key deleted for " + vendorPrefix)

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${vendorPrefix}';""").first === 0
        }
      }

      "return a 404 if there are no API keys associated with this vendor" +
      "prefix" in {
        val (status, res) = apiKey.deleteFromVendorPrefix(vendorPrefix)
        status === NotFound
        res must contain("Vendor prefix not found")

        database withDynSession {
          Q.queryNA[Int](
            s"""select count(*)
            from ${tableName}
            where vendor_prefix = '${vendorPrefix}';""").first === 0
        }
      }
    }
  }
}
