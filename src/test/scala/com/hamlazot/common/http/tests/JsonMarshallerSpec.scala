package com.hamlazot.common.http.tests

import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.util.UUID

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{decodeRequest, entity}
import com.hamlazot.common.http.JsonMarshalling
import com.hamlazot.common.http.tests.MockEnum.MockEnum
import com.hamlazot.common.macros.Macros.{Serializer, Mapper}
import org.json4s.JsonAST.JObject

import scala.util.{Failure, Success}

/**
 * Created by yoav on 1/25/16.
 */
class JsonMarshallerSpec
  extends HamlazotHttpSpec
  with JsonMarshalling {

  object impl extends NestingTrait {
    type Trustees = Request //Map[UUID, Int]
  }


  //  lazy val route = pathPrefix("users") {
  //    post {
  //      decodeRequest {
  //        entity(asTry[impl.NestedCaseClassWithShitInIt]) { tryRequest =>
  //          complete {
  //            tryRequest match {
  //              case Success(a) =>
  //                StatusCodes.OK
  //              case Failure(e) =>
  //                println(s"exception: $e, stack trace: ${e.getStackTrace.toList.mkString("\n")}")
  //                StatusCodes.UnprocessableEntity
  //            }
  //          }
  //        }
  //
  //
  //      }
  //    }
  //  }

  def routi[A: Manifest](implicit serializer: Serializer[A]) = pathPrefix("users") {

    post {
      decodeRequest {
        entity(asTry[A]) { tryRequest =>
          complete {
            tryRequest match {
              case Success(a) =>
                StatusCodes.OK
              case Failure(e) =>
                println(s"exception: $e, stack trace: ${e.getStackTrace.toList.mkString("\n")}")
                StatusCodes.UnprocessableEntity
            }
          }
        }


      }
    }
  }


  "JsonMarshaller " should {
    "unmarshall string to case object" in {
      registerEnumForMarshalling(MockEnum)
      val request = Request("myRequest1", "jojo", MockEnum.Private)
      val serialized = serialize(request)
      parse(serialized).asInstanceOf[JObject].values.keys.toList(1) shouldEqual "the_name"
      val des = deserialize[Request](serialized)
      des shouldEqual request
    }

    "serialize object without char escapes" in {
      registerEnumForMarshalling(MockEnum)
      val request = Request("myRequest1", "request", MockEnum.Private)
      val serializedRequest = serialize(request)
      val deserializedRequest = deserialize[Request](serializedRequest)
      deserializedRequest shouldEqual request
    }

    "serialize ZonedDateTime" in {

      val container = ZonedDateTime.ofInstant(Instant.now, ZoneOffset.UTC)
      val serializedContainer = serialize(container)
      val deserializedContainer = deserialize[ZonedDateTime](serializedContainer)

      deserializedContainer shouldEqual container.truncatedTo(ChronoUnit.SECONDS)
    }

    "serialize and deserialize a template object " in {

      registerEnumForMarshalling(MockEnum)
      val request = CollectionResponse[Request](List(Request("myRequest1", "request", MockEnum.Private), Request("myRequest2", "request", MockEnum.Public)), None, 2)
      val serializedRequest = serialize(request)
      val deserializedRequest = deserialize[CollectionResponse[Request]](serializedRequest)
      deserializedRequest shouldEqual request
    }

    "serialize and deserialize a template object" in {
      registerEnumForMarshalling(MockEnum)
      val request = CollectionResponse[Request](List(Request("myRequest1", "request", MockEnum.Private), Request("myRequest2", "request", MockEnum.Public)), None, 2)
      val serializedRequest = serialize(request)
      val deserializedRequest = deserialize[CollectionResponse[Request]](serializedRequest)
      deserializedRequest shouldEqual request
    }

    "serialize and deserialize a template object with nestedObject " in {
      registerEnumForMarshalling(MockEnum)
      val request = CollectionResponse[NestedRequest](List(NestedRequest("myRequest1", "request", MockEnum.Private, Request("myRequest2", "request", MockEnum.Public))), None, 2)
      val serializedRequest = serialize(request)
      val deserializedRequest = deserialize[CollectionResponse[NestedRequest]](serializedRequest)
      deserializedRequest shouldEqual request
    }



    "parsing failures include descriptive error messages on wrong values in fields" in {
      registerEnumForMarshalling(MockEnum)
      try {
        deserialize[RequestNum]( """{ "_id":"myRequest1","the_name":"jojo","world_mode": "private" }""")
        0 === 1
      }
      catch {
        case e: Exception =>
          convertMappingExceptionMessage(e) === s"""Error in mode parameter: jojo is not a valid value”. Explanation: when sending "the_name" {"jojo"} which is not int"""
      }
    }

    "parsing failures include descriptive error messages on missing fields" in {
      registerEnumForMarshalling(MockEnum)
      try {
        deserialize[Request]( """{ "_id":"myRequest1", "world_mode": "private" }""")
        0 === 1
      }
      catch {
        case e: Exception =>
          convertMappingExceptionMessage(e) === "Missing input parameter: the_name"
      }
    }

    "serialize nested case classes with abstract shit in it in a generic manner" in {

      object impli extends NestingTrait {
        type Trustees = String //Map[UUID, Int]
      }

      val nestedCaseClass = impli.NestedCaseClassWithShitInIt("Jojo", 35, UUID.randomUUID.toString)

      implicit def serializer = seriamap[impli.NestedCaseClassWithShitInIt]
      Post("/users", nestedCaseClass) ~>
        routi[impli.NestedCaseClassWithShitInIt] ~> check {
        status === StatusCodes.OK
      }

    }

    "serialize nested case classes with abstract classes in it in a generic manner" in {
      object impli extends NestingTrait {
        type Trustees = Request //Map[UUID, Int]
      }

      registerEnumForMarshalling(MockEnum)
      val nestedCaseClass = impli.NestedCaseClassWithShitInIt("Jojo", 35, Request(UUID.randomUUID().toString, "MyName", MockEnum.Private))
      implicit val siri = implicitly[Serializer[impli.NestedCaseClassWithShitInIt]] //new Serializer[impli.NestedCaseClassWithShitInIt] {
      //
      //       import java.lang.String;
      //       import scala.math.BigInt;
      //       //import com.hamlazot.common.http.tests.Request;
      //       def deserializ(jsonStr: String): impli.NestedCaseClassWithShitInIt = {
      //         val json = parse(jsonStr).asInstanceOf[JObject].children;
      //         val result = impli.NestedCaseClassWithShitInIt(json(0).camelizeKeys.extract[String], json(1).camelizeKeys.extract[BigInt], json(2).camelizeKeys.extract[Request]);
      //         result
      //       }
      //     }          //implicitly[Serializer[impli.NestedCaseClassWithShitInIt]]

      implicit def ser = seriamap[impli.NestedCaseClassWithShitInIt]

      val serialized = seriamap[impli.NestedCaseClassWithShitInIt](nestedCaseClass)
      Post("/users", nestedCaseClass) ~>
        routi[impli.NestedCaseClassWithShitInIt] ~> check {
        status === StatusCodes.OK
      }

    }

  }
}

object MockEnum extends Enumeration {
  type MockEnum = Value

  val Private = Value("private")
  val Public = Value("public")
  val Secret = Value("secret")
}

case class Request(_id: String, theName: String, worldMode: MockEnum)

case class RequestNum(_id: String, theName: Int, mode: MockEnum)

case class NestedRequest(_id: String, name: String, worldMode: MockEnum, mested: Request)

case class DateTimeContainer(zdt: ZonedDateTime = ZonedDateTime.now.truncatedTo(ChronoUnit.SECONDS))

case class CollectionResponse[A](list: List[A], nextPage: Option[String], totalCount: Int)

trait NestingTrait {
  type Trustees

  case class NestedCaseClassWithShitInIt(str: String, i: BigInt, trustees: Trustees)

  case class NestedCaseClass(str: String, i: BigInt)

}
