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
package model

// This project
import util.IgluPostgresDriver.simple._

// Java
import java.util.UUID

// Joda
import org.joda.time.LocalDateTime

// Json4s
import org.json4s.jackson.Serialization.writePretty

// Slick
import Database.dynamicSession

// Spray
import spray.http.StatusCode
import spray.http.StatusCodes._

/**
 * DAO for accessing the apikeys table in the database
 * @constructor create an API key DAO with a reference to the database
 * @param db a reference to a ``Database``
 */
class ApiKeyDAO(val db: Database) extends DAO {

  private val uidRegex =
    "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"

  /**
   * Case class representing an API key in the database.
   * @constructor create an API key object from required data
   * @param uid API key uuid serving as primary key
   * @param vendorPrefix of the API key
   * @param permission API key permission in (read, write, super)
   * @param createdAt date at which point the API key was created
   */
  case class ApiKey(
    uid: UUID,
    vendorPrefix: String,
    permission: String,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime
  )

  /**
   * Schema for the apikeys table.
   */
  class ApiKeys(tag: Tag) extends Table[ApiKey](tag, "apikeys") {
    def uid = column[UUID]("uid", O.PrimaryKey, O.DBType("uuid"))
    def vendorPrefix = column[String]("vendor_prefix", O.DBType("varchar(200)"),
      O.NotNull)
    def permission = column[String]("permission",
      O.DBType("varchar(20)"), O.NotNull, O.Default[String]("read"))
    def createdAt = column[LocalDateTime]("createdat", O.DBType("timestamp"),
      O.NotNull)
    def updatedAt = column[LocalDateTime]("updatedat", O.DBType("timestamp"),
      O.NotNull)

    def * = (uid, vendorPrefix, permission, createdAt, updatedAt) <>
      (ApiKey.tupled, ApiKey.unapply)
  }

  //Object used to access the table
  val apiKeys = TableQuery[ApiKeys]

  //Case classes for JSON formatting
  case class ResApiKey(vendorPrefix: String, key: String, metadata: Metadata)
  case class Metadata(permission: String, createdAt: String, updatedAt: String)

  /**
   * Creates the apikeys table.
   */
  private def createTable = db withDynSession { apiKeys.ddl.create }

  /**
   * Deletes the apikeys table.
   */
  def dropTable = db withDynSession { apiKeys.ddl.drop }

  /**
   * Creates the apikeys table and inserts the super API key.
   */
  def initTable = {
    createTable
    add(".", "super")
  }

  /**
   * Gets the vendor prefix and permission associated with an API key.
   * @param uid the API key's UUID
   * @return an option containing a (vendor prefix, permission) pair
   */
  def get(uid: String): Option[(String, String)] = {
    if (uid matches uidRegex) {
      db withDynSession {
        val l: List[(String, String)] =
          apiKeys
            .filter(_.uid === UUID.fromString(uid))
            .map(k => (k.vendorPrefix, k.permission))
            .list

        if (l.length == 1) {
          Some(l(0))
        } else {
          None
        }
      }
    } else {
      None
    }
  }

  /**
   * Gets the vendor prefix associated with an uuid.
   * @param permission API key's permission
   * @param uids the API keys' uuids
   * @return a status code and json pair containing information about API keys
   * having these UUIDs.
   */
  def get(permission: String, uids: List[UUID]): (StatusCode, String) =
    if (permission == "super") {
      db withDynSession {
        val l: List[ResApiKey] =
          (for {
            k <- apiKeys if k.uid inSet uids
          } yield k)
            .list
            .map(k => ResApiKey(k.vendorPrefix, k.uid.toString,
              Metadata(k.permission,
                k.createdAt.toString("MM/dd/yyyy HH:mm:ss"),
                k.updatedAt.toString("MM/dd/yyyy HH:mm:ss"))))

        if (l.length == 0) {
          (NotFound, result(404, "API key not found"))
        } else if (l.length == 1) {
          (OK, writePretty(l(0)))
        } else {
          (OK, writePretty(l))
        }
      }
    } else {
      (Unauthorized, result(401, "You do not have sufficient privileges"))
    }

  /**
   * Gets every API key associated with the given vendor prefix.
   * @param permission API key's permission
   * @param vendorPrefixes list of vendor prefix of the API keys to be retrieved
   * @return a status code and json pair containing information about API keys
   * having these UUIDs.
   */
  def getFromVendorPrefix(permission: String, vendorPrefixes: List[String]):
  (StatusCode, String) =
    if (permission == "write") {
      db withDynSession {
        val l: List[ResApiKey] = (for {
          k <- apiKeys if k.vendorPrefix inSet vendorPrefixes
        } yield k)
          .list
          .map(k => ResApiKey(k.vendorPrefix, k.uid.toString,
            Metadata(k.permission,
              k.createdAt.toString("MM/dd/yyyy HH:mm:ss"),
              k.updatedAt.toString("MM/dd/yyyy HH:mm:ss"))))

        if (l.length == 0) {
          (NotFound, result(404, "Vendor prefix not found"))
        } else if (l.length == 1) {
          (OK, writePretty(l(0)))
        } else {
          (OK, writePretty(l))
        }
      }
    } else {
      (Unauthorized, result(401, "You do not have sufficient privileges"))
    }

  /**
   * Deletes an API key from its uuid.
   * @param permission API key's permission
   * @param uid the API key's uuid
   * @return a status code and json response pair
   */
  def delete(permission: String, uid: UUID): (StatusCode, String) =
    if (permission == "super") {
      db withDynSession {
        apiKeys.filter(_.uid === uid).delete match {
          case 0 => (NotFound, result(404, "API key not found"))
          case 1 => (OK, result(200, "API key successfully deleted"))
          case _ => (InternalServerError, result(500, "Something went wrong"))
        }
      }
    } else {
      (Unauthorized, result(401, "You do not have sufficient privileges"))
    }

  /**
   * Deletes all API keys having the specified vendor prefix.
   * @param permission API key's permission
   * @param vendorPrefix vendor prefix of the API keys we want to delete
   * @return a (status code, json response) pair
   */
  def deleteFromVendorPrefix(permission: String, vendorPrefix: String):
  (StatusCode, String) =
    if (permission == "super") {
      db withDynSession {
        apiKeys.filter(_.vendorPrefix === vendorPrefix).delete match {
          case 0 => (NotFound, result(404, "Vendor prefix not found"))
          case 1 => (OK, result(200, "API key deleted for " + vendorPrefix))
          case n => (OK, result(200, "API keys deleted for " + vendorPrefix))
        }
      }
    } else {
      (Unauthorized, result(401, "You do not have sufficient privileges"))
    }

  /**
   * Updates the API keys having the specified vendor prefix.
   * @param permission API key's permission
   * @param vendorPrefix vendor prefix of the API keys we want to update
   * @return a (status code, json response) pair
   */
  def regenerate(permission: String, vendorPrefix: String):
  (StatusCode, String) =
    if (permission == "super") {
      db withDynSession {
        val l = (for {
          k <- apiKeys if k.vendorPrefix === vendorPrefix
        } yield k)
          .map(k => (k.uid, k.permission))
          .list

        (l.length: @unchecked) match {
          case 0 => addReadWrite(permission, vendorPrefix)
          case 1 => (l(0)._2: @unchecked) match {
            case "read" =>
              apiKeys
                .filter(k => k.vendorPrefix === vendorPrefix &&
                  k.permission === "read")
                .map(k => (k.uid, k.updatedAt))
                .update(UUID.randomUUID, new LocalDateTime)
              add(vendorPrefix, "write")
              getFromVendorPrefix(permission, List(vendorPrefix))
            case "write" =>
              apiKeys
                .filter(k => k.vendorPrefix === vendorPrefix &&
                  k.permission === "write")
                .map(k => (k.uid, k.updatedAt))
                .update(UUID.randomUUID, new LocalDateTime)
              add(vendorPrefix, "read")
              getFromVendorPrefix(permission, List(vendorPrefix))
            case "super" =>
              apiKeys
                .filter(k => k.vendorPrefix === vendorPrefix &&
                  k.permission === "super")
                .map(k => (k.uid, k.updatedAt))
                .update(UUID.randomUUID, new LocalDateTime)
              getFromVendorPrefix(permission, List(vendorPrefix))
          }
          case 2 =>
            apiKeys
              .filter(k => k.vendorPrefix === vendorPrefix &&
                k.permission === "read")
              .map(k => (k.uid, k.updatedAt))
              .update(UUID.randomUUID, new LocalDateTime)
            apiKeys
              .filter(k => k.vendorPrefix === vendorPrefix &&
                k.permission === "write")
              .map(k => (k.uid, k.updatedAt))
              .update(UUID.randomUUID, new LocalDateTime)
            getFromVendorPrefix(permission, List(vendorPrefix))
        }
      }
    } else {
      (Unauthorized, result(401, "You do not have sufficient privileges"))
    }

  /**
   * Adds both read and write API keys for a vendor prefix after validating it.
   * @param permission API key's permission
   * @param vendorPrefix vendorPrefix of the new pair of keys
   * @returns a status code and a json containing the pair of API keys.
   */
  def addReadWrite(permission: String, vendorPrefix: String):
  (StatusCode, String) =
    if (permission == "super") {
      db withDynSession {
        if (validate(vendorPrefix)) {
          val (statusRead, keyRead) = add(vendorPrefix, "read")
          val (statusWrite, keyWrite) = add(vendorPrefix, "write")

          if(statusRead == InternalServerError ||
            statusWrite == InternalServerError) {
              delete(permission, UUID.fromString(keyRead))
              delete(permission, UUID.fromString(keyWrite))
              (InternalServerError, result(500, "Something went wrong"))
            } else {
              val now = new LocalDateTime().toString("MM/dd/yyyy HH:mm:ss")
              (Created, writePretty(List(
                ResApiKey(vendorPrefix, keyRead, Metadata("read", now, now)),
                ResApiKey(vendorPrefix, keyWrite, Metadata("write", now, now))
              )))
            }
        } else {
          (Unauthorized,
            result(401, "This vendor prefix is conflicting with an existing one"))
        }
      }
    } else {
      (Unauthorized, result(401, "You do not have sufficient privileges"))
    }

  /**
   * Adds a new API key.
   * @param vendorPrefix vendorPrefix of the new API key
   * @param permission permission of the new API key
   * @return a status code and a json response pair
   */
  private def add(vendorPrefix: String, permission: String):
  (StatusCode, String) =
    db withDynSession {
      val uid = UUID.randomUUID()
      val now = new LocalDateTime
      apiKeys.insert(ApiKey(uid, vendorPrefix, permission, now, now)) match {
          case 0 => (InternalServerError, "Something went wrong")
          case n => (OK, uid.toString)
        }
    }

  /**
   * Validates that a new vendorPrefix is not conflicting with an existing one
   * (same prefix).
   * @param vendorPrefix vendorPrefix of the new API keys being validated
   * @return a boolean indicating whether or not we allow this new API key
   * vendor prefix
   */
  private def validate(vendorPrefix: String): Boolean =
    db withDynSession {
      apiKeys
        .map(_.vendorPrefix)
        .list
        .filter(v => v.startsWith(vendorPrefix) || vendorPrefix.startsWith(v) ||
          v == vendorPrefix)
        .length == 0
    }
}
