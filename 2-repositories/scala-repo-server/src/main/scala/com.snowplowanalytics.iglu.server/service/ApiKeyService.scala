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
package service

// This project
import actor.ApiKeyActor._
import util.TokenAuthenticator

// Akka
import akka.actor.ActorRef
import akka.pattern.ask

// Scala
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

// Spray
import spray.http.MediaTypes._
import spray.http.StatusCode
import spray.http.StatusCodes._
import spray.routing._
import spray.routing.PathMatcher.Lift

// Swagger
import com.wordnik.swagger.annotations._
import javax.ws.rs.Path

/**
 * Service to interact with API keys.
 * @constructor create a new API key generation service with an apiKey actor
 * @param apiKeyActor a reference to a ``ApiKeyActor``
 */
@Api(value = "/api/apikeys", position = 2,
  description = """Operations dealing with API key generation and deletion,
  requires a super API key""")
class ApiKeyService(apiKeyActor: ActorRef)
(implicit executionContext: ExecutionContext) extends Directives with Service {

  /**
   * Creates a ``TokenAuthenticator`` to extract the api_key http header and
   * validates it against the database.
   */
  val authenticator = TokenAuthenticator[(String, String)]("api_key") {
    key => (apiKeyActor ? Auth(key)).mapTo[Option[(String, String)]]
  }

  /**
   * Directive to authenticate a user using the authenticator.
   */
  def auth: Directive1[(String, String)] = authenticate(authenticator)

  /**
   * API key generation service's route
   */
  lazy val routes =
    rejectEmptyResponse {
      respondWithMediaType(`application/json`) {
        auth { authPair =>
          if (authPair._2 == "super") {
            pathPrefix("keys") {
              delete {
                deleteKeyRoute
              } ~
              get {
                readKeyRoute
              }
            } ~
            pathPrefix("vendorprefixes") {
              post {
                addRoute
              } ~
              put {
                regenerateRoute
              } ~
              delete {
                deleteKeysRoute
              } ~
              get {
                readKeysRoute
              }
            }
          } else {
            complete(Unauthorized, "You do not have sufficient privileges")
          }
        }
      }
    }

  /**
   * Route to generate a pair of read and write API keys.
   */
  @Path(value = "/vendorprefixes/{vendorPrefix}")
  @ApiOperation(value = "Generates a pair of read and read/write API keys",
    notes = "Returns a pair of API keys", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "vendorPrefix",
      value = "Vendor prefix of the API keys", required = true,
      dataType = "string", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 401,
      message = "This vendor prefix is conflicting with an existing one"),
    new ApiResponse(code = 401,
      message = "You do not have sufficient privileges"),
    new ApiResponse(code = 401,
      message = "The supplied authentication is invalid"),
    new ApiResponse(code = 401, message = """The resource requires
      authentication, which was not supplied with the request"""),
    new ApiResponse(code = 500, message = "Something went wrong")
  ))
  def addRoute =
    path(Segment) { vendorPrefix =>
      complete {
        (apiKeyActor ? AddReadWriteKeys(vendorPrefix))
          .mapTo[(StatusCode, String)]
      }
    }

  /**
   * Route to regenerate a pair of a read and write API keys.
   */
  @Path(value = "/vendorprefixes/{vendorPrefix}")
  @ApiOperation(value = "Regenerates a pair of read and read/write API keys",
    notes = "Returns a new pair of API keys", httpMethod = "PUT")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "vendorPrefix",
      value = "Vendor prefix of the API keys", required = true,
      dataType = "string", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 401,
      message = "This vendor prefix is conflicting with an existing one"),
    new ApiResponse(code = 401,
      message = "You do not have sufficient privileges"),
    new ApiResponse(code = 401,
      message = "The supplied authentication is invalid"),
    new ApiResponse(code = 401, message = """The resource requires
      authentication, which was not supplied with the request"""),
    new ApiResponse(code = 500, message = "Something went wrong")
  ))
  def regenerateRoute =
    path(Segment) { vendorPrefix =>
      complete {
        (apiKeyActor ? RegenerateKeys(vendorPrefix)).mapTo[(StatusCode, String)]
      }
    }

  /**
   * Route to delete every API key having a specific vendor prefix.
   */
  @Path(value = "/vendorprefixes/{vendorPrefix}")
  @ApiOperation(value = "Deletes every API key having this vendor prefix",
    httpMethod = "DELETE")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "vendorPrefix",
      value = "API keys' vendor prefix", required = true, dataType = "string",
      paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200,
      message = "API key deleted for the vendor prefix"),
    new ApiResponse(code = 401,
      message = "You do not have sufficient privileges"),
    new ApiResponse(code = 401,
      message = "The supplied authentication is invalid"),
    new ApiResponse(code = 401, message = """The resource requires
      authentication, which was not supplied with the request"""),
    new ApiResponse(code = 404, message = "Vendor prefix not found")
  ))
  def deleteKeysRoute =
    path(Segment) { vendorPrefix =>
      complete {
        (apiKeyActor ? DeleteKeys(vendorPrefix)).mapTo[(StatusCode, String)]
      }
    }

  /**
   * Route to retrieve every API key having a vendor prefix in the list of
   * vendor prefixes.
   */
  @Path(value = "/vendorprefixes/{vendorPrefixes}")
  @ApiOperation(value = "Retrieves every API key having this vendor prefix",
    httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "vendorPrefixes",
      value = "Comma-separated list of API keys' vendor prefix",
      required = true, dataType = "string", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 401,
      message = "You do not have sufficient privileges"),
    new ApiResponse(code = 401,
      message = "The supplied authentication is invalid"),
    new ApiResponse(code = 401, message = """The resource requires
      authentication, which was not supplied with the request"""),
    new ApiResponse(code = 404, message = "Vendor prefix not found")
  ))
  def readKeysRoute =
    path(Segment.repeat(separator = ",")) { vendorPrefixes =>
      complete {
        (apiKeyActor ? GetKeys(vendorPrefixes)).mapTo[(StatusCode, String)]
      }
    }

  /**
   * Route to delete a single API key.
   */
  @Path(value = "/keys/{key}")
  @ApiOperation(value = "Deletes a single API key", httpMethod = "DELETE")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "key", value = "API key to be deleted",
      required = true, dataType = "string", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "API key successfully deleted"),
    new ApiResponse(code = 401,
      message = "You do not have sufficient privileges"),
    new ApiResponse(code = 401,
      message = "The supplied authentication is invalid"),
    new ApiResponse(code = 401, message = """The resource requires
      authentication, which was not supplied with the request"""),
    new ApiResponse(code = 401,
      message = "The API key provided is not and UUID"),
    new ApiResponse(code = 404, message = "API key not found"),
    new ApiResponse(code = 500, message = "Something went wrong")
  ))
  def deleteKeyRoute =
    path(JavaUUID) { key =>
      complete {
        (apiKeyActor ? DeleteKey(key)).mapTo[(StatusCode, String)]
      }
    }

  /**
   * Route to retrieve a list of API keys through their keys.
   */
  @Path(value = "/keys/{keys}")
  @ApiOperation(value = "Retrieves a single API key", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "keys",
      value = "Comma-separated list of API keys to be retrieved",
      required = true, dataType = "string", paramType = "path")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 401,
      message = "You do not have sufficient privileges"),
    new ApiResponse(code = 401,
      message = "The supplied authentication is invalid"),
    new ApiResponse(code = 401, message = """The resource requires
      authentication, which was not supplied with the request"""),
    new ApiResponse(code = 401,
      message = "The API key provided is not and UUID"),
    new ApiResponse(code = 404, message = "API key not found"),
    new ApiResponse(code = 500, message = "Something went wrong")
  ))
  def readKeyRoute =
    path(JavaUUID.repeat(separator = ",")) { keys =>
      complete {
        (apiKeyActor ? GetKey(keys)).mapTo[(StatusCode, String)]
      }
    }
}