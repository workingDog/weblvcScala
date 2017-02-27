package com.kodekutters

import com.kodekutters.WebLvc._

import scala.language.implicitConversions

/**
 * set of implicits
 */
object WeblvcImplicits {

  // ------------------X to Option[X]--------------------------------------------------

  implicit def ArrDoubleToArrDoubleOp(value: Array[Double]): Option[Array[Double]] = Option(value)

  implicit def ArrIntToArrIntOp(value: Array[Int]): Option[Array[Int]] = Option(value)

  implicit def StringToStringOp(value: String): Option[String] = Option(value)

  implicit def DoubleToDoubleOp(value: Double): Option[Double] = Option(value)

  implicit def IntToIntOp(value: Int): Option[Int] = Option(value)

  implicit def BoolToBoolOp(value: Boolean): Option[Boolean] = Option(value)

  implicit def EitherToOp(value: Either[String, Long]): Option[Either[String, Long]] = Option(value)

  implicit def CoordinatesToOp(value: Coordinates): Option[Coordinates] = Option(value)

  implicit def ServerDeadReckoningToOp(value: ServerDeadReckoning): Option[ServerDeadReckoning] = Option(value)

  implicit def ObjectBoundsToOp(value: ObjectBounds): Option[ObjectBounds] = Option(value)

  implicit def AttributeUpdateToOp(value: AttributeUpdate): Option[AttributeUpdate] = Option(value)

  implicit def ArrayWeblvcMsgToOp(value: Array[WeblvcMsg]): Option[Array[WeblvcMsg]] = Option(value)

  implicit def KeyValueMapToOp(value: AttributesMap): Option[AttributesMap] = Option(value)

  // ------------------X to Option[Either[x,y]]--------------------------------------------------

  implicit def StringToEither(value: String): Option[Either[String, Long]] = Option(Left(value))

  implicit def LongToEither(value: Long): Option[Either[String, Long]] = Option(Right(value))

  // ------------------X to WeblvcMsg -----------------------------------------------------------

  implicit def SubscribeObjectToWeblvcMsg(value: SubscribeObject): WeblvcMsg = value.asInstanceOf[WeblvcMsg]

  implicit def SubscribeInteractionToWeblvcMsg(value: SubscribeInteraction): WeblvcMsg = value.asInstanceOf[WeblvcMsg]


}
