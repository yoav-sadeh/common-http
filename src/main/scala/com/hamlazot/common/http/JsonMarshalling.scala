package com.hamlazot.common.http

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.{FromRequestUnmarshaller, Unmarshaller}
import akka.stream.Materializer
import com.hamlazot.common.macros.serialization.MacroSerializer
import com.hamlazot.common.macros.Macros.{Mapper, Serializer}
import com.hamlazot.common.serialization.{CamelcaseDeseiralizationTransformer, JsonSerializer, SnakecaseSerializationTransformer}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
 * Created by yoav on 1/18/16.
 */

trait JsonMarshalling
  extends MacroSerializer
  with CamelcaseDeseiralizationTransformer
  with SnakecaseSerializationTransformer {

  val jsonMediaType = MediaTypes.`application/json`
  val jsonContentType = ContentTypes.`application/json`


  implicit def anyRefMarsheller[A <: AnyRef]: ToEntityMarshaller[A] = {
    Marshaller.withFixedContentType(jsonMediaType) {
      (anyRef: AnyRef) =>
        HttpEntity(ContentType(jsonMediaType), serialize(anyRef))
    }
  }

  def seriamap[A: Mapper]: ToEntityMarshaller[A] = {
    val mapper = implicitly[Mapper[A]]
    Marshaller.withFixedContentType[A, akka.http.scaladsl.model.MessageEntity](jsonMediaType) {
      (a: A) =>
        val map = mapper.toMap(a)
        HttpEntity(ContentType(jsonMediaType), serialize(map))
    }
  }


  implicit def anyValMarsheller[A <: AnyVal]: ToEntityMarshaller[A] = {
    Marshaller.withFixedContentType(jsonMediaType) {
      (anyVal: AnyVal) => HttpEntity(ContentType(jsonMediaType), anyVal.toString)
    }
  }

  implicit def anyRefUnmarshaller[A: Manifest]: FromRequestUnmarshaller[A] = {
    new Unmarshaller[HttpRequest, A] {
      override def apply(value: HttpRequest)(implicit ec: ExecutionContext, materializer: Materializer): Future[A] = {

        value.entity.withContentType(jsonContentType).toStrict(5 seconds).map(_.data.toArray).map(x => {
          deserialize[A](new String(x))
        })

      }
    }
  }

  def asVal[A: Manifest](implicit um: Serializer[A]): akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller[A] = new Unmarshaller[HttpRequest, A] {
    override def apply(value: HttpRequest)(implicit ec: ExecutionContext, materializer: Materializer): Future[A] = {
      val castable = implicitly[Serializer[A]]
      value.entity.withContentType(jsonContentType).toStrict(5 seconds).map(_.data.toArray).map(x => {
        val jsonStr = new String(x)
        val result = castable.deserializ(jsonStr)
        result
      })
    }
  }

  def asTry[A: Manifest](implicit um: Serializer[A]): akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller[Try[A]] = new Unmarshaller[HttpRequest, Try[A]] {
    import com.hamlazot.common.macros.Macros.Serializer
    override def apply(value: HttpRequest)(implicit ec: ExecutionContext, materializer: Materializer): Future[Try[A]] = {

      value.entity.withContentType(jsonContentType).toStrict(5 seconds).map(_.data.toArray).map(x => {
        val jsonStr = new String(x)
        Try {
          val result = marshal[A](jsonStr)
          result
        }
      })
    }
  }

  implicit def anyRefTryMarsheller[A <: AnyRef]: ToEntityMarshaller[Try[A]] = {
    Marshaller.withFixedContentType(jsonMediaType) {
      (anyRef: AnyRef) => HttpEntity(ContentType(jsonMediaType), serialize(anyRef))
    }
  }

  def convertMappingExceptionMessage(e: Exception) = {
    e match {
      case e: org.json4s.MappingException =>
        "No usable value for \\w+".r findFirstIn e.getMessage match {
          case Some(extracted) =>
            val fieldName = underscore(extracted.split(" ").last)
            val conversionFaultMessage = "Do not know how to convert [\\w()]+ into [\\w()]+".r findFirstIn e.getMessage match {
              case Some(conversionfault) =>
                val conversionfaultSubs = ("[\\w()]+ into [\\w()]+".r findFirstIn conversionfault).get.split(" into ")
                val headValue = "\\(\\w+\\)".r findFirstIn conversionfaultSubs.head match {
                  case Some(jsonValue) => jsonValue.replaceAll("[()]", "")
                  case None => conversionfaultSubs.head
                }
                Some( s"""Error in mode parameter: ${headValue} is not a valid value”. Explanation: when sending "${fieldName}" {"${headValue}"} which is not ${conversionfaultSubs.last}""")
              case None => None
            }
            val enumerationFaultMessage = if (("No usable value for [\\w\\$]+".r findAllMatchIn e.getMessage length) > 1) {
              Option(e.getMessage)
            }
            else {
              None
            }
            conversionFaultMessage.getOrElse(enumerationFaultMessage.getOrElse(s"Missing input parameter: ${underscore(extracted.split(" ").last)}"))

          case None => e.getMessage
        }


      case _ => e.getMessage
    }
  }

  private def underscore(word: String): String = {
    val spacesPattern = "[-\\s]".r
    val firstPattern = "([A-Z]+)([A-Z][a-z])".r
    val secondPattern = "([a-z\\d])([A-Z])".r
    val replacementPattern = "$1_$2"
    spacesPattern.replaceAllIn(
      secondPattern.replaceAllIn(
        firstPattern.replaceAllIn(
          word, replacementPattern), replacementPattern), "_").toLowerCase
  }

}