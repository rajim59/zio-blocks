package zio.schema

import zio.json._
import java.time._
import java.time.format.DateTimeFormatter
import java.util.{Currency, UUID}

package object codec {

  implicit val instantDecoder: JsonDecoder[Instant] =
    JsonDecoder[String].mapOrFail(s =>
      try Right(Instant.parse(s)) catch { case e: Exception => Left(e.getMessage) }
    )
  implicit val instantEncoder: JsonEncoder[Instant] =
    JsonEncoder[String].contramap(_.toString)

  implicit val localDateDecoder: JsonDecoder[LocalDate] =
    JsonDecoder[String].mapOrFail(s =>
      try Right(LocalDate.parse(s, DateTimeFormatter.ISO_LOCAL_DATE))
      catch { case e: Exception => Left(e.getMessage) }
    )
  implicit val localDateEncoder: JsonEncoder[LocalDate] =
    JsonEncoder[String].contramap(_.format(DateTimeFormatter.ISO_LOCAL_DATE))

  implicit val localDateTimeDecoder: JsonDecoder[LocalDateTime] =
    JsonDecoder[String].mapOrFail(s =>
      try Right(LocalDateTime.parse(s, DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      catch { case e: Exception => Left(e.getMessage) }
    )
  implicit val localDateTimeEncoder: JsonEncoder[LocalDateTime] =
    JsonEncoder[String].contramap(_.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))

  implicit val localTimeDecoder: JsonDecoder[LocalTime] =
    JsonDecoder[String].mapOrFail(s =>
      try Right(LocalTime.parse(s, DateTimeFormatter.ISO_LOCAL_TIME))
      catch { case e: Exception => Left(e.getMessage) }
    )
  implicit val localTimeEncoder: JsonEncoder[LocalTime] =
    JsonEncoder[String].contramap(_.format(DateTimeFormatter.ISO_LOCAL_TIME))

  implicit val monthDecoder: JsonDecoder[Month] =
    JsonDecoder[String].mapOrFail(s =>
      try Right(Month.valueOf(s)) catch { case e: Exception => Left(e.getMessage) }
    )
  implicit val monthEncoder: JsonEncoder[Month] =
    JsonEncoder[String].contramap(_.toString)

  implicit val uuidDecoder: JsonDecoder[UUID] =
    JsonDecoder[String].mapOrFail(s =>
      try Right(UUID.fromString(s)) catch { case e: Exception => Left(e.getMessage) }
    )
  implicit val uuidEncoder: JsonEncoder[UUID] =
    JsonEncoder[String].contramap(_.toString)

  implicit val currencyDecoder: JsonDecoder[Currency] =
    JsonDecoder[String].mapOrFail(s =>
      try Right(Currency.getInstance(s)) catch { case e: Exception => Left(e.getMessage) }
    )
  implicit val currencyEncoder: JsonEncoder[Currency] =
    JsonEncoder[String].contramap(_.getCurrencyCode)

  implicit val zoneIdDecoder: JsonDecoder[ZoneId] =
    JsonDecoder[String].mapOrFail(s =>
      try Right(ZoneId.of(s)) catch { case e: Exception => Left(e.getMessage) }
    )
  implicit val zoneIdEncoder: JsonEncoder[ZoneId] =
    JsonEncoder[String].contramap(_.getId)

  implicit val zoneOffsetDecoder: JsonDecoder[ZoneOffset] =
    JsonDecoder[String].mapOrFail(s =>
      try Right(ZoneOffset.of(s)) catch { case e: Exception => Left(e.getMessage) }
    )
  implicit val zoneOffsetEncoder: JsonEncoder[ZoneOffset] =
    JsonEncoder[String].contramap(_.getId)

