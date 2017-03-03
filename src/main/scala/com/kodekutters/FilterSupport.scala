package com.kodekutters

import scala.util.control.Breaks._
import com.kodekutters.WebLvc.Filter
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.Writes._
import play.api.libs.json.{JsValue, _}
import play.api.libs.json._

import scala.language.implicitConversions
import scala.language.postfixOps

// todo All this must be redone
/**
  * the filters.
  *
  * due to type erasure when using generic Arrays this turned into this mad and ugly code.
  *
  */
object FilterSupport {

  sealed trait FilterType

  object FilterType {

    val theReads = new Reads[FilterType] {
      def reads(json: JsValue): JsResult[FilterType] = {
        StringArrayFilter.fmt.reads(json) |
          IntArrayFilter.fmt.reads(json) |
          BooleanArrayFilter.fmt.reads(json) |
          DoubleArrayFilter.fmt.reads(json) |
          MinMaxString.fmt.reads(json) |
          MinMaxInt.fmt.reads(json) |
          MinMaxDouble.fmt.reads(json) |
          MinMaxArrayDouble.fmt.reads(json) |
          MinMaxArrayInt.fmt.reads(json) |
          MinMaxArrayString.fmt.reads(json) |
          ArrayOfMinMaxInt.fmt.reads(json) |
          ArrayOfMinMaxDouble.fmt.reads(json) |
          ArrayOfMinMaxString.fmt.reads(json) |
          ArrayOfMinMaxArrayString.fmt.reads(json) |
          ArrayOfMinMaxArrayInt.fmt.reads(json) |
          ArrayOfMinMaxArrayDouble.fmt.reads(json) |
          ArrayFilterType.fmt.reads(json) |
          NestedFilterType.fmt.reads(json)
      }
    }

    val theWrites = new Writes[FilterType] {
      def writes(msg: FilterType) = {
        msg match {
          case s: StringArrayFilter => StringArrayFilter.fmt.writes(s)
          case s: IntArrayFilter => IntArrayFilter.fmt.writes(s)
          case s: DoubleArrayFilter => DoubleArrayFilter.fmt.writes(s)
          case s: BooleanArrayFilter => BooleanArrayFilter.fmt.writes(s)

          case s: MinMaxString => MinMaxString.fmt.writes(s)
          case s: MinMaxInt => MinMaxInt.fmt.writes(s)
          case s: MinMaxDouble => MinMaxDouble.fmt.writes(s)

          case s: MinMaxArrayDouble => MinMaxArrayDouble.fmt.writes(s)
          case s: MinMaxArrayInt => MinMaxArrayInt.fmt.writes(s)
          case s: MinMaxArrayString => MinMaxArrayString.fmt.writes(s)

          case s: ArrayOfMinMaxInt => ArrayOfMinMaxInt.fmt.writes(s)
          case s: ArrayOfMinMaxDouble => ArrayOfMinMaxDouble.fmt.writes(s)
          case s: ArrayOfMinMaxString => ArrayOfMinMaxString.fmt.writes(s)

          case s: ArrayOfMinMaxArrayString => ArrayOfMinMaxArrayString.fmt.writes(s)
          case s: ArrayOfMinMaxArrayInt => ArrayOfMinMaxArrayInt.fmt.writes(s)
          case s: ArrayOfMinMaxArrayDouble => ArrayOfMinMaxArrayDouble.fmt.writes(s)

          case s: ArrayFilterType => ArrayFilterType.fmt.writes(s)
          case s: NestedFilterType => NestedFilterType.fmt.writes(s)

          case _ => JsNull
        }
      }
    }

    implicit val fmt: Format[FilterType] = Format(theReads, theWrites)
  }

  case class StringArrayFilter(value: Array[String]) extends FilterType

  object StringArrayFilter {
    implicit def fmt: Format[StringArrayFilter] = new Format[StringArrayFilter] {
      def reads(json: JsValue): JsResult[StringArrayFilter] = {
        json.asOpt[Array[String]] match {
          case Some(x) => JsSuccess(new StringArrayFilter(x))
          case None => JsError("could not read StringArrayFilter: " + json)
        }
      }

      def writes(f: StringArrayFilter) = Json.toJson(f.value)
    }
  }

  case class IntArrayFilter(value: Array[Int]) extends FilterType

  object IntArrayFilter {
    implicit def fmt: Format[IntArrayFilter] = new Format[IntArrayFilter] {
      def reads(json: JsValue): JsResult[IntArrayFilter] =
        json.asOpt[Array[Int]] match {
          case Some(x) => JsSuccess(new IntArrayFilter(x))
          case None => JsError("could not read IntArrayFilter: " + json)
        }

      def writes(f: IntArrayFilter) = Json.toJson(f.value)
    }
  }

  case class DoubleArrayFilter(value: Array[Double]) extends FilterType

  object DoubleArrayFilter {
    implicit def fmt: Format[DoubleArrayFilter] = new Format[DoubleArrayFilter] {
      def reads(json: JsValue): JsResult[DoubleArrayFilter] = {
        json.asOpt[Array[Double]] match {
          case Some(x) => JsSuccess(new DoubleArrayFilter(x))
          case None => JsError("could not read DoubleArrayFilter: " + json)
        }
      }

      def writes(f: DoubleArrayFilter) = Json.toJson(f.value)
    }
  }

  case class BooleanArrayFilter(value: Array[Boolean]) extends FilterType

  object BooleanArrayFilter {
    implicit def fmt: Format[BooleanArrayFilter] = new Format[BooleanArrayFilter] {
      def reads(json: JsValue): JsResult[BooleanArrayFilter] = {
        json.asOpt[Array[Boolean]] match {
          case Some(x) => JsSuccess(new BooleanArrayFilter(x))
          case None => JsError("could not read BooleanArrayFilter: " + json)
        }
      }

      def writes(f: BooleanArrayFilter) = Json.toJson(f.value)
    }
  }

  /**
    * to use in filters for testing values between the min and max values
    */
  //  case class MinMaxRange[T](min: T, max: T) extends FilterType
  //
  //  object MinMaxRange {
  //    implicit def fmt[T](implicit fmt: Format[T]): Format[MinMaxRange[T]] = new Format[MinMaxRange[T]] {
  //      def reads(json: JsValue): JsResult[MinMaxRange[T]] = JsSuccess(new MinMaxRange[T]((json \ "min").as[T], (json \ "max").as[T]))
  //
  //      def writes(r: MinMaxRange[T]) = JsObject(Seq("min" -> Json.toJson(r.min), "max" -> Json.toJson(r.max)))
  //    }
  //  }

  /**
    * to use in filters for testing values between the min and max values
    *
    * @param min the minimum lexicographical value
    * @param max the maximum lexicographical value
    */
  case class MinMaxString(min: String, max: String) extends FilterType {

    def isInRange(test: String): Boolean = {
      test.compare(min) match {
        case 0 | 1 =>
          test.compare(max) match {
            case 0 | -1 => true
            case 1 => false
          }
        case -1 => false
      }
    }

  }

  object MinMaxString {
    implicit val fmt = Json.format[MinMaxString]
  }

  case class MinMaxArrayString(min: Array[String], max: Array[String]) extends FilterType

  object MinMaxArrayString {
    implicit val fmt = Json.format[MinMaxArrayString]
  }

  case class ArrayOfMinMaxArrayString(v: Array[MinMaxArrayString]) extends FilterType

  object ArrayOfMinMaxArrayString {

    val theReads = new Reads[ArrayOfMinMaxArrayString] {
      def reads(js: JsValue): JsResult[ArrayOfMinMaxArrayString] = {
        js.asOpt[Array[MinMaxArrayString]] match {
          case Some(x) => JsSuccess(new ArrayOfMinMaxArrayString(x))
          case None => JsError(s"Error reading ArrayOfMinMaxArrayString: $js")
        }
      }
    }

    val theWrites = new Writes[ArrayOfMinMaxArrayString] {
      def writes(arr: ArrayOfMinMaxArrayString) = JsArray(for (i <- arr.v) yield Json.toJson(i))
    }

    implicit val fmt: Format[ArrayOfMinMaxArrayString] = Format(theReads, theWrites)
  }

  case class ArrayOfMinMaxString(value: Array[MinMaxString]) extends FilterType

  object ArrayOfMinMaxString {

    val theReads = new Reads[ArrayOfMinMaxString] {
      def reads(js: JsValue): JsResult[ArrayOfMinMaxString] = {
        js.asOpt[Array[MinMaxString]] match {
          case Some(x) => JsSuccess(new ArrayOfMinMaxString(x))
          case None => JsError(s"Error reading ArrayOfMinMaxString: $js")
        }
      }
    }

    val theWrites = new Writes[ArrayOfMinMaxString] {
      def writes(arr: ArrayOfMinMaxString) = JsArray(for (i <- arr.value) yield Json.toJson(i))
    }

    implicit val fmt: Format[ArrayOfMinMaxString] = Format(theReads, theWrites)
  }

  /**
    * to use in filters for testing values between the min and max values
    *
    * @param min the minimum value
    * @param max the maximum value
    */

  case class MinMaxInt(min: Int, max: Int) extends FilterType {

    def isInRange(test: Int): Boolean = if (test >= min && test <= max) true else false

  }

  object MinMaxInt {
    implicit val fmt = Json.format[MinMaxInt]


  }

  case class ArrayOfMinMaxInt(value: Array[MinMaxInt]) extends FilterType

  object ArrayOfMinMaxInt {

    val theReads = new Reads[ArrayOfMinMaxInt] {
      def reads(js: JsValue): JsResult[ArrayOfMinMaxInt] = {
        js.asOpt[Array[MinMaxInt]] match {
          case Some(x) => JsSuccess(new ArrayOfMinMaxInt(x))
          case None => JsError(s"Error reading ArrayOfMinMaxInt: $js")
        }
      }
    }

    val theWrites = new Writes[ArrayOfMinMaxInt] {
      def writes(arr: ArrayOfMinMaxInt) = JsArray(for (i <- arr.value) yield Json.toJson(i))
    }

    implicit val fmt: Format[ArrayOfMinMaxInt] = Format(theReads, theWrites)
  }

  case class MinMaxArrayInt(min: Array[Int], max: Array[Int]) extends FilterType {

    def isInRange(test: Int): Boolean = {
      var result = false
      breakable {
        // assume min.length == max.length
        for (i <- 0 to min.length) {
          if (test >= min(i) && test <= max(i)) result = true else {
            result = false
            break
          }
        }
      }
      result
    }

  }

  object MinMaxArrayInt {
    implicit val fmt = Json.format[MinMaxArrayInt]
  }

  case class ArrayOfMinMaxArrayInt(value: Array[MinMaxArrayInt]) extends FilterType

  object ArrayOfMinMaxArrayInt {

    val theReads = new Reads[ArrayOfMinMaxArrayInt] {
      def reads(js: JsValue): JsResult[ArrayOfMinMaxArrayInt] = {
        js.asOpt[Array[MinMaxArrayInt]] match {
          case Some(x) => JsSuccess(new ArrayOfMinMaxArrayInt(x))
          case None => JsError(s"Error reading ArrayOfMinMaxArrayInt: $js")
        }
      }
    }

    val theWrites = new Writes[ArrayOfMinMaxArrayInt] {
      def writes(arr: ArrayOfMinMaxArrayInt) = JsArray(for (i <- arr.value) yield Json.toJson(i))
    }

    implicit val fmt: Format[ArrayOfMinMaxArrayInt] = Format(theReads, theWrites)
  }

  /**
    * to use in filters for testing values between the min and max values
    *
    * @param min the minimum array values
    * @param max the maximum array values
    */
  case class MinMaxDouble(min: Double, max: Double) extends FilterType {
    def isInRange(test: Double): Boolean = if (test >= min && test <= max) true else false
  }

  object MinMaxDouble {
    implicit val fmt = Json.format[MinMaxDouble]
  }

  case class ArrayOfMinMaxDouble(value: Array[MinMaxDouble]) extends FilterType

  object ArrayOfMinMaxDouble {

    val theReads = new Reads[ArrayOfMinMaxDouble] {
      def reads(js: JsValue): JsResult[ArrayOfMinMaxDouble] = {
        js.asOpt[Array[MinMaxDouble]] match {
          case Some(x) => JsSuccess(new ArrayOfMinMaxDouble(x))
          case None => JsError(s"Error reading ArrayOfMinMaxDouble: $js")
        }
      }
    }

    val theWrites = new Writes[ArrayOfMinMaxDouble] {
      def writes(arr: ArrayOfMinMaxDouble) = JsArray(for (i <- arr.value) yield Json.toJson(i))
    }

    implicit val fmt: Format[ArrayOfMinMaxDouble] = Format(theReads, theWrites)
  }

  /**
    * to use in filters for testing values between the min and max values
    *
    * @param min the minimum array values
    * @param max the maximum array values
    */
  case class MinMaxArrayDouble(min: Array[Double], max: Array[Double]) extends FilterType

  object MinMaxArrayDouble {
    implicit val fmt = Json.format[MinMaxArrayDouble]
  }

  case class ArrayOfMinMaxArrayDouble(v: Array[MinMaxArrayDouble]) extends FilterType

  object ArrayOfMinMaxArrayDouble {

    val theReads = new Reads[ArrayOfMinMaxArrayDouble] {
      def reads(js: JsValue): JsResult[ArrayOfMinMaxArrayDouble] = {
        js.asOpt[Array[MinMaxArrayDouble]] match {
          case Some(x) => JsSuccess(new ArrayOfMinMaxArrayDouble(x))
          case None => JsError(s"Error reading ArrayOfMinMaxArrayDouble: $js")
        }
      }
    }

    val theWrites = new Writes[ArrayOfMinMaxArrayDouble] {
      def writes(arr: ArrayOfMinMaxArrayDouble) = JsArray(for (i <- arr.v) yield Json.toJson(i))
    }

    implicit val fmt: Format[ArrayOfMinMaxArrayDouble] = Format(theReads, theWrites)
  }

  /**
    * nested filters
    *
    * @param value
    */
  case class NestedFilterType(value: Filter) extends FilterType

  object NestedFilterType {

    val theReads = new Reads[NestedFilterType] {
      def reads(js: JsValue): JsResult[NestedFilterType] = {
        js.asOpt[Filter] match {
          case Some(x) => JsSuccess(new NestedFilterType(x))
          case None => JsError(s"Error reading NestedFilterType: $js")
        }
      }
    }

    val theWrites = new Writes[NestedFilterType] {
      def writes(x: NestedFilterType) = Json.toJson(x)
    }

    implicit val fmt: Format[NestedFilterType] = Format(theReads, theWrites)
  }

  /**
    * nested array of filters
    *
    * @param value
    */
  case class ArrayFilterType(value: Array[Filter]) extends FilterType

  object ArrayFilterType {

    val theReads = new Reads[ArrayFilterType] {
      def reads(js: JsValue): JsResult[ArrayFilterType] = {
        js.asOpt[Array[Filter]] match {
          case Some(x) => JsSuccess(new ArrayFilterType(x))
          case None => JsError(s"Error reading ArrayFilterType: $js")
        }
      }
    }

    val theWrites = new Writes[ArrayFilterType] {
      def writes(arr: ArrayFilterType) = JsArray(for (i <- arr.value) yield Json.toJson(i))
    }

    implicit val fmt: Format[ArrayFilterType] = Format(theReads, theWrites)
  }


  //-------------------------------------------------------------------------------------
  //------------------filter expressions-------------------------------------------------
  //-------------------------------------------------------------------------------------

  sealed trait FilterExpType

  /**
    * "and" filter expression accept an array of filters,
    * and passes if and only if every filter passes.
    *
    * @param value an Array of Filters
    */
  case class FilterAnd(value: Array[Filter]) extends FilterExpType {
    val key = FilterAnd.key
  }

  object FilterAnd {
    val key = "and"

    val theReads = new Reads[FilterAnd] {
      def reads(js: JsValue): JsResult[FilterAnd] = {
        js.asOpt[Array[Filter]] match {
          case Some(filterArr) => JsSuccess(new FilterAnd(filterArr))
          case None => JsError("could not read FilterAnd: " + js)
        }
      }
    }

    val theWrites = new Writes[FilterAnd] {
      def writes(arr: FilterAnd) = JsArray(for (i <- arr.value) yield Json.toJson(i))
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * "or" filter expression accept an array of filters,
    * and passes if and only if at least one filter passes.
    *
    * @param value an Array of Filters
    */
  case class FilterOr(value: Array[Filter]) extends FilterExpType {
    val key = FilterAnd.key
  }

  object FilterOr {
    val key = "or"

    val theReads = new Reads[FilterOr] {
      def reads(js: JsValue): JsResult[FilterOr] = {
        js.asOpt[Array[Filter]] match {
          case Some(filterArr) => JsSuccess(new FilterOr(filterArr))
          case None => JsError("could not read FilterOr: " + js)
        }
      }
    }

    val theWrites = new Writes[FilterOr] {
      def writes(arr: FilterOr) = JsArray(for (i <- arr.value) yield Json.toJson(i))
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * "or" filter expression accept an array of filters,
    * and passes if and only if every filter passes.
    *
    * @param value an Array of Filters
    */
  case class FilterNot(value: Filter) extends FilterExpType {
    val key = FilterAnd.key
  }

  object FilterNot {
    val key = "not"

    val theReads = new Reads[FilterNot] {
      def reads(js: JsValue): JsResult[FilterNot] = {
        js.asOpt[Filter] match {
          case Some(filter) => JsSuccess(new FilterNot(filter))
          case None => JsError("could not read FilterNot: " + js)
        }
      }
    }

    val theWrites = new Writes[FilterNot] {
      def writes(arr: FilterNot) = Json.toJson[Filter](arr.value)
    }

    implicit val fmt = Format(theReads, theWrites)
  }


}
