/*
* Copyright (c) 2014 Snowplow Analytics Ltd. All rights reserved.
*
* This program is licensed to you under the Apache License Version 2.0,
* and you may not use this file except in compliance with the
* Apache License Version 2.0.
* You may obtain a copy of the Apache License Version 2.0 at
* http://www.apache.org/licenses/LICENSE-2.0.
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the Apache License Version 2.0 is distributed on
* an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
* express or implied.  See the Apache License Version 2.0 for the specific
* language governing permissions and limitations there under.
*/
package com.snowplowanalytics.iglu.server
package actor

// This project
import model.ApiKeyDAO
import util.ServerConfig

// Akka
import akka.actor.Actor

// Java
import java.util.UUID

/**
 * Object regrouping every message the ``ApiKeyActor`` can receive.
 */
object ApiKeyActor {

  /**
   * Message to send in order to add a (read, write) pair of keys for the
   * specified vendor prefix if it is not conflicting with an existing one.
   * @param vendorPrefix the API keys to be generated will have this prefix
   * prefix
   */
  case class AddReadWriteKeys(vendorPrefix: String)

  /**
   * Message to send in order to regenerate a (read, write) pair of keys for the
   * specified vendor prefix.
   * @param vendorPrefix the vendor prefix for which the API keys need to be
   * regenerated
   */
  case class RegenerateKeys(vendorPrefix: String)

  /**
   * Message to send in order to retrieve a (vendorPrefix, permission) pair if
   * the key exists.
   * @param uid identifier for the API key to be retrieved
   */
  case class Auth(uid: String)

  /**
   * Message to send in order to retrieve information about the API key from its
   * UUID.
   * @param uid API key's UUID
   */
  case class GetKey(uid: UUID)

  /**
   * Message to send in order to retrieve information about the API keys having
   * the specified vendor prefix.
   * @param vendorPrefix vendor prefix of the API keys to be retrieved
   */
  case class GetKeys(vendorPrefix: String)

  /**
   * Message to send in order to delete a key specifying its uuid.
   * @param uid identifier of the API key to be deleted
   */
  case class DeleteKey(uid: UUID)

  /**
   * Message to send in order to delete every keys belonging to the specified
   * vendor prefix.
   * @param vendorPrefix the API keys having this vendor prefix will be deleted
   */
  case class DeleteKeys(vendorPrefix: String)
}

/**
 * ApiKey actor interfacing between the services and the API key model.
 */
class ApiKeyActor extends Actor {

  import ApiKeyActor._

  // ApiKey model
  val apiKey = new ApiKeyDAO(ServerConfig.db)

  /**
   * Method specifying how the actor should handle the incoming messages.
   */
  def receive = {

    case AddReadWriteKeys(vendorPrefix) =>
      sender ! apiKey.addReadWrite(vendorPrefix)

    case RegenerateKeys(vendorPrefix) =>
      sender ! apiKey.regenerate(vendorPrefix)

    case Auth(uid) => sender ! apiKey.get(uid)

    case GetKey(uid) => sender ! apiKey.get(uid)

    case GetKeys(vendorPrefix) =>
      sender ! apiKey.getFromVendorPrefix(vendorPrefix)

    case DeleteKey(uid) => sender ! apiKey.delete(uid)

    case DeleteKeys(vendorPrefix) =>
      sender ! apiKey.deleteFromVendorPrefix(vendorPrefix)
  }
}
