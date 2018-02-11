package com.kodekutters

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.Writes._
import play.api.libs.json.{JsValue, _}
import au.id.jazzy.play.geojson._
import play.api.libs.json._
import scala.language.implicitConversions
import scala.language.postfixOps


/**
  * WebLVC standard object model protocol
  *
  * ref: "WebLVC Draft Protocol Specification Version 0.5 of 22 July 2016"
  * see SISO: https://www.sisostds.org/DigitalLibrary.aspx?EntryId=45483
  *
  * Author: R. Wathelet
  * version: 0.5
  */

package object WebLvc {

  object WebLvcSupport {

    // OGC coordinate reference system URN, see Configure Message
    val defaultECEFCartesian = "urn:ogc:def:crs:EPSG::4978"
    val webMercator = "urn:ogc:def:crs:EPSG::3857"
    val geodedicWGS84 = "urn:ogc:def:crs:EPSG::4326"

    // Object Types defined by the Standard WebLVC Object Model
    val phyEntity = "WebLVC:PhysicalEntity"
    val aggEntity = "WebLVC:AggregateEntity"
    val envEntity = "WebLVC:EnvironmentalEntity"
    val radioTrans = "WebLVC:RadioTransmitter"
    val stdObjectTypes = Seq(phyEntity, aggEntity, envEntity, radioTrans)

    // Interaction Types defined by Standard WebLVC Object Model
    val weaponFire = "WebLVC:WeaponFire"
    val munitionDetonation = "WebLVC:MunitionDetonation"
    val startResume = "WebLVC:StartResume"
    val stopFreeze = "WebLVC:StopFreeze"
    val radioSignal = "WebLVC:RadioSignal"
    val transferOwnership = "WebLVC:TransferOwnership"
    val stdInteractionTypes = Seq(weaponFire, munitionDetonation, startResume, stopFreeze, radioSignal, transferOwnership)

    // list of admin messages
    val adminKindSet = Seq("Connect", "ConnectResponse", "Configure", "ConfigureResponse")

    // list of all message kinds
    val msgKindSet = Seq("AttributeUpdate", "ObjectDeleted", "SubscribeObject", "UnsubscribeObject",
      "SubscribeInteraction", "UnsubscribeInteraction", "StatusLogRequest",
      "StatusLogResponse", "Interaction") ++ adminKindSet

    implicit def eitherReads[A, B](implicit Ax: Reads[A], Bx: Reads[B]): Reads[Either[A, B]] =
      Reads[Either[A, B]] { json =>
        Ax.reads(json) match {
          case JsSuccess(value, path) => JsSuccess(Left(value), path)
          case JsError(e1) => Bx.reads(json) match {
            case JsSuccess(value, path) => JsSuccess(Right(value), path)
            case JsError(e2) => JsError(JsError.merge(e1, e2))
          }
        }
      }

    implicit def eitherWrites[A, B](implicit Ax: Writes[A], Bx: Writes[B]): Writes[Either[A, B]] =
      Writes[Either[A, B]] {
        case Left(a) => Ax.writes(a)
        case Right(b) => Bx.writes(b)
      }

  }

  import FilterSupport._
  import WebLvcSupport._

  //------------------------------------------------------------------------------------
  //------------------- Common Properties and Datatypes --------------------------------
  //------------------------------------------------------------------------------------

  /**
    * status log of server implementation dependent information
    *
    * @param Timestamp a string containing the time and date the log was recorded - measured by the server’s local time
    *                  formatted according to ISO 8601.
    * @param Index     a number corresponding to the order the message was logged. The first message for the current starts
    *                  at Index 0, the next is Index 1, and so on.
    * @param Message   a string containing the text of the log entry.
    */
  case class StatusLog(Timestamp: String, Index: Int, Message: String)

  object StatusLog {
    implicit val fmt = Json.format[StatusLog]
  }

  sealed trait Coordinates

  object Coordinates {

    val theReads = new Reads[Coordinates] {
      def reads(json: JsValue): JsResult[Coordinates] = {
        CoordinatesECEF.fmt.reads(json) | CoordinatesGeod.fmt.reads(json)
      }
    }

    val theWrites = new Writes[Coordinates] {
      def writes(coord: Coordinates) = {
        coord match {
          case s: CoordinatesECEF => CoordinatesECEF.fmt.writes(s)
          case s: CoordinatesGeod => CoordinatesGeod.fmt.writes(s)
          case _ => JsNull
        }
      }
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * geocentric CRS, such as ECEF in DIS
    */
  case class CoordinatesECEF(DeadReckoningAlgorithm: Option[Int] = None, WorldLocation: Option[Array[Double]] = None,
                             VelocityVector: Option[Array[Double]] = None, AccelerationVector: Option[Array[Double]] = None,
                             Orientation: Option[Array[Double]] = None, AngularVelocity: Option[Array[Double]] = None) extends Coordinates

  object CoordinatesECEF {
    implicit val fmt = Json.format[CoordinatesECEF]
  }

  /**
    * geodetic CRS (as in EPSG::3857 or EPSG::4362)
    */
  case class CoordinatesGeod(WorldLocation: Option[Array[Double]] = None, VelocityVector: Option[Array[Double]] = None, Orientation: Option[Array[Double]] = None) extends Coordinates

  object CoordinatesGeod {
    implicit val fmt = Json.format[CoordinatesGeod]
  }

  /**
    * server dead reckoning parameters
    *
    * @param Enable
    * @param PositionThreshold
    * @param MaximumRate
    */
  case class ServerDeadReckoning(Enable: Option[Boolean], PositionThreshold: Option[Double], MaximumRate: Option[Double])

  object ServerDeadReckoning {
    implicit val fmt = Json.format[ServerDeadReckoning]
  }

  /**
    * specifies the name of a WebLVC object, and a range around the object within which the client is interested in updates
    * i.e. the client wants to watch only things within Range (m) of ObjectName
    *
    * @param ObjectName the reference object from which to start the range
    * @param Range      in meters
    */
  case class ObjectBounds(ObjectName: String, Range: Int)

  object ObjectBounds {
    implicit val fmt = Json.format[ObjectBounds]
  }

  /**
    * a generic (key,value) dictionary,
    * with key = an attribute name, and
    * value = the attribute JsValue
    */
  case class AttributesMap(nodes: Map[String, JsValue])

  object AttributesMap {

    def readAttributes(js: JsValue, omitList: List[String]): Option[AttributesMap] = {
      js match {
        case json: JsObject =>
          // get all fields of js, but not the fields in the omitList
          val fList = json.fields.filterNot(p => omitList.contains(p._1))
          if (fList.isEmpty) None else Some(new AttributesMap(fList.toMap))

        case x => JsError(s"Could not read AttributesMap: $x"); None
      }
    }

    val theReads = new Reads[AttributesMap] {
      def reads(json: JsValue): JsResult[AttributesMap] = {
        json match {
          case js: JsObject => JsSuccess(new AttributesMap(js.fields.toMap))
          case x => JsError(s"Error in AttributesMap could not read KeyValue: $x")
        }
      }
    }

    val theWrites = new Writes[AttributesMap] {
      def writes(keyval: AttributesMap): JsObject = JsObject(keyval.nodes)
    }

    implicit val fmt: Format[AttributesMap] = Format(theReads, theWrites)
  }

  /**
    * a filter.
    *
    * Note this is for used only in SubscribeObject message
    *
    * @param key   "all" or "any"
    * @param value a (String, FilterType) tuple with FilterType values in an array of either:
    *              a single value of String, Double, Int or Boolean
    *              multiple values of String, Double, Int or Boolean
    *              an object defining a minimum/maximum range
    *              an object defining “any” or “all” nested properties
    */
  case class Filter(key: String, value: Tuple2[String, FilterType])

  object Filter {

    val theReads = new Reads[Filter] {
      def reads(js: JsValue): JsResult[Filter] = {
        (js \ "all").toOption match {
          case Some(x) =>
            val theHead = x.asInstanceOf[JsObject].fields.head
            theHead._2.asOpt[FilterType] match {
              case Some(z) => JsSuccess(new Filter("all", (theHead._1, z)))
              case None => JsError("could not read filter: " + js)
            }

          case None =>
            (js \ "any").toOption match {
              case Some(x) =>
                val theHead = x.asInstanceOf[JsObject].fields.head
                theHead._2.asOpt[FilterType] match {
                  case Some(z) => JsSuccess(new Filter("any", (theHead._1, z)))
                  case None => JsError("could not read filter: " + js)
                }

              case None => JsError("could not read filter: " + js)
            }
        }
      }
    }

    val theWrites = new Writes[Filter] {
      def writes(f: Filter) = {
        val jsarr = Json.toJson[FilterType](f.value._2)
        Json.obj(f.key -> Json.obj(f.value._1 -> jsarr))
      }
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * Filter expressions combine different filters into a single result.
    * Boolean operators “and”, “or” and “not” are available. The Boolean operators
    * are themselves filters allowing composition of Boolean expressions of filters.
    *
    * Note this is for used only in SubscribeObject message
    *
    * @param key   "and" "or" "not"
    * @param value a FilterExpType
    */
  case class FilterExpression(key: String, value: FilterExpType)

  object FilterExpression {

    val theReads = new Reads[FilterExpression] {
      def reads(json: JsValue): JsResult[FilterExpression] = {
        (json \ "and").asOpt[FilterAnd] match {
          case Some(x) => JsSuccess(new FilterExpression(FilterAnd.key, x))
          case None =>
            (json \ "or").asOpt[FilterOr] match {
              case Some(x) => JsSuccess(new FilterExpression(FilterOr.key, x))
              case None =>
                (json \ "not").asOpt[FilterNot] match {
                  case Some(x) => JsSuccess(new FilterExpression(FilterNot.key, x))
                  case None => JsError("could not read FilterExpression: " + json)
                }
            }
        }
      }
    }

    val theWrites = new Writes[FilterExpression] {

      def writes(fx: FilterExpression) = {
        fx.value match {
          case s: FilterAnd => Json.obj("and" -> FilterAnd.fmt.writes(s))
          case s: FilterOr => Json.obj("or" -> FilterOr.fmt.writes(s))
          case s: FilterNot => Json.obj("not" -> FilterNot.fmt.writes(s))
          case _ => JsNull
        }
      }
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * represents a generic weblvc message
    */
  sealed trait WeblvcMsg {
    val MessageKind: String
  }

  /**
    * represents an attribute update message
    */
  sealed trait AttributeUpdateMsg {
    val ObjectType: String
    val ObjectName: String
    val Timestamp: Option[Either[String, Long]]
  }

  /**
    * represents an Interaction message.
    * Interaction messages are used to convey parameters of a simulation event or interaction.
    */
  sealed trait InteractionMsg {
    val InteractionType: String
    val Timestamp: Option[Either[String, Long]]
  }

  //------------------------------------------------------------------------------------
  //-------------------Admin messages--------------------------------------------------
  //------------------------------------------------------------------------------------

  /**
    * client request a WebLVC connection with the server
    *
    * @param ClientName    name of the client
    * @param WebLVCVersion version to use
    * @param Messages      set of Weblvc messages, typically to customise the server (e.g. filters etc...)
    */
  case class Connect(ClientName: String, WebLVCVersion: Option[Double] = None, Messages: Option[Array[WeblvcMsg]] = None) extends WeblvcMsg {
    val MessageKind = Connect.MessageKind
  }

  object Connect {
    val MessageKind = "Connect"

    val fmtx = Json.format[Connect]

    val theReads = new Reads[Connect] {
      def reads(js: JsValue): JsResult[Connect] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[Connect] {
      def writes(c: Connect) = Json.obj("MessageKind" -> MessageKind) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * server response to a client Connect request.
    *
    * @param Connected     whether the connection was accepted
    * @param WebLVCVersion version to use
    * @param Errors        possible set of errors encountered
    */
  case class ConnectResponse(Connected: Boolean,
                             WebLVCVersion: Option[Double] = None,
                             Errors: Option[Array[StatusLog]] = None) extends WeblvcMsg {
    val MessageKind = ConnectResponse.MessageKind
  }

  object ConnectResponse {
    val MessageKind = "ConnectResponse"

    val fmtx = Json.format[ConnectResponse]

    val theReads = new Reads[ConnectResponse] {
      def reads(js: JsValue): JsResult[ConnectResponse] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[ConnectResponse] {
      def writes(c: ConnectResponse) = Json.obj("MessageKind" -> MessageKind) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * Client configuration request to the server
    *
    * @param TimestampFormat           0 for string Default, Hex ASCII string as DIS/RPR,
    *                                  (DIS timestamp field format converted into hexadecimal ASCII)
    *                                  1 for number of Seconds since simulation began, double precision
    *                                  2 for number of Milliseconds since January 1, 1970, as integer
    * @param CoordinateReferenceSystem defaultECEFCartesian
    * @param ServerDeadReckoning       DR parameters to use
    * @param WorldBounds               a GeoJSON geometry object of type “Polygon” or “MultiPolygon”
    * @param ObjectBounds              specifies the name of a WebLVC object, and a range around the object within which the client is interested in updates
    */
  case class Configure(TimestampFormat: Option[Int] = None,
                       CoordinateReferenceSystem: Option[String] = Some(defaultECEFCartesian),
                       ServerDeadReckoning: Option[ServerDeadReckoning] = None,
                       WorldBounds: Option[Either[Polygon[LngLatAlt], MultiPolygon[LngLatAlt]]] = None,
                       ObjectBounds: Option[ObjectBounds] = None,
                       attributes: Option[AttributesMap] = None) extends WeblvcMsg {

    val MessageKind = Configure.MessageKind

    def this(TimestampFormat: Option[Int],
             CoordinateReferenceSystem: Option[String],
             ServerDeadReckoning: Option[ServerDeadReckoning],
             WorldBounds: Polygon[LngLatAlt],
             ObjectBounds: Option[ObjectBounds],
             attributes: Option[AttributesMap]) = this(TimestampFormat, CoordinateReferenceSystem,
      ServerDeadReckoning, Option(Left(WorldBounds)), ObjectBounds, attributes)

    def this(TimestampFormat: Option[Int],
             CoordinateReferenceSystem: Option[String],
             ServerDeadReckoning: Option[ServerDeadReckoning],
             WorldBounds: MultiPolygon[LngLatAlt],
             ObjectBounds: Option[ObjectBounds],
             attributes: Option[AttributesMap]) = this(TimestampFormat, CoordinateReferenceSystem,
      ServerDeadReckoning, Option(Right(WorldBounds)), ObjectBounds, attributes)
  }

  object Configure {
    val MessageKind = "Configure"

    // the list of all field names but not "attributes"
    private val omitList = (for (f <- Configure().getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[Configure] {
      def reads(js: JsValue): JsResult[Configure] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          JsSuccess(new Configure(
            (js \ "TimestampFormat").asOpt[Int],
            (js \ "CoordinateReferenceSystem").asOpt[String],
            (js \ "ServerDeadReckoning").asOpt[ServerDeadReckoning],
            (js \ "WorldBounds").asOpt[Either[Polygon[LngLatAlt], MultiPolygon[LngLatAlt]]],
            (js \ "ObjectBounds").asOpt[ObjectBounds],
            AttributesMap.readAttributes(js, omitList)))
        }
        else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[Configure] {
      def writes(p: Configure): JsValue = {
        val base = Json.obj("MessageKind" -> p.MessageKind)
        val theList = JsObject(List(
          p.TimestampFormat.map("TimestampFormat" -> JsNumber(_)),
          p.CoordinateReferenceSystem.map("CoordinateReferenceSystem" -> JsString(_)),
          p.ServerDeadReckoning.map("ServerDeadReckoning" -> Json.toJson(_)),
          p.WorldBounds.map("WorldBounds" -> Json.toJson(_)),
          p.ObjectBounds.map("ObjectBounds" -> Json.toJson(_))).flatten)
        p.attributes match {
          case Some(att) => base ++ theList ++ Json.toJson[AttributesMap](att).asInstanceOf[JsObject]
          case None => base ++ theList
        }
      }
    }

    implicit val fmt: Format[Configure] = Format(theReads, theWrites)
  }

  /**
    * server response to a client Configure request.
    * set of Booleans indicating success of the configurations.
    */
  case class ConfigureResponse(TimestampFormat: Option[Boolean] = None,
                               CoordinateReferenceSystem: Option[Boolean] = None,
                               ServerDeadReckoning: Option[Boolean] = None,
                               WorldBounds: Option[Boolean] = None,
                               ObjectBounds: Option[Boolean] = None,
                               attributes: Option[AttributesMap] = None) extends WeblvcMsg {

    val MessageKind = ConfigureResponse.MessageKind
  }

  object ConfigureResponse {
    val MessageKind = "ConfigureResponse"

    // the list of all field names but not "attributes"
    private val omitList = (for (f <- ConfigureResponse().getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[ConfigureResponse] {
      def reads(js: JsValue): JsResult[ConfigureResponse] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          JsSuccess(new ConfigureResponse(
            (js \ "TimestampFormat").asOpt[Boolean],
            (js \ "CoordinateReferenceSystem").asOpt[Boolean],
            (js \ "ServerDeadReckoning").asOpt[Boolean],
            (js \ "WorldBounds").asOpt[Boolean],
            (js \ "ObjectBounds").asOpt[Boolean],
            AttributesMap.readAttributes(js, omitList)))
        }
        else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[ConfigureResponse] {
      def writes(p: ConfigureResponse): JsValue = {
        val base = Json.obj("MessageKind" -> p.MessageKind)
        val theList = JsObject(List(
          p.TimestampFormat.map("TimestampFormat" -> JsBoolean(_)),
          p.CoordinateReferenceSystem.map("CoordinateReferenceSystem" -> JsBoolean(_)),
          p.ServerDeadReckoning.map("ServerDeadReckoning" -> JsBoolean(_)),
          p.WorldBounds.map("WorldBounds" -> JsBoolean(_)),
          p.ObjectBounds.map("ObjectBounds" -> JsBoolean(_))).flatten)
        p.attributes match {
          case Some(att) => base ++ theList ++ Json.toJson[AttributesMap](att).asInstanceOf[JsObject]
          case None => base ++ theList
        }
      }
    }

    implicit val fmt: Format[ConfigureResponse] = Format(theReads, theWrites)
  }

  //------------------------------------------------------------------------------------
  //-------------------weblvc messages--------------------------------------------------
  //------------------------------------------------------------------------------------

  /**
    * general attribute update message that holds the values of the attributes of a WebLVC object.
    *
    * @param ObjectName uniquely identifies the WebLVC object
    * @param ObjectType indicates the Object Type of the WebLVC Object for which the message is conveying an update
    * @param Timestamp  the timestamp of the update
    * @param attributes other possible attributes as a map of (key,value)
    */
  case class AttributeUpdate(ObjectName: String, ObjectType: String,
                             Timestamp: Option[Either[String, Long]] = None,
                             attributes: Option[AttributesMap] = None) extends WeblvcMsg with AttributeUpdateMsg {

    def this(ObjectName: String, ObjectType: String, Timestamp: String, attributes: AttributesMap) = this(ObjectName, ObjectType, Option(Left(Timestamp)), Option(attributes))

    def this(ObjectName: String, ObjectType: String, Timestamp: Long, attributes: AttributesMap) = this(ObjectName, ObjectType, Option(Right(Timestamp)), Option(attributes))

    val MessageKind = AttributeUpdate.MessageKind
  }

  object AttributeUpdate {
    val MessageKind = "AttributeUpdate"

    // the list of all field names but not "attributes"
    private val omitList = (for (f <- AttributeUpdate("", "").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[AttributeUpdate] {
      def reads(js: JsValue): JsResult[AttributeUpdate] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          JsSuccess(new AttributeUpdate(
            (js \ "ObjectType").as[String],
            (js \ "ObjectName").as[String],
            (js \ "Timestamp").asOpt[Either[String, Long]],
            AttributesMap.readAttributes(js, omitList)))
        }
        else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[AttributeUpdate] {
      def writes(p: AttributeUpdate): JsValue = {
        val base = Json.obj("MessageKind" -> p.MessageKind, "ObjectType" -> p.ObjectType, "ObjectName" -> p.ObjectName)
        val theList = JsObject(List(p.Timestamp.map("Timestamp" -> Json.toJson(_))).flatten)
        p.attributes match {
          case Some(att) => base ++ theList ++ Json.toJson[AttributesMap](att).asInstanceOf[JsObject]
          case None => base ++ theList
        }
      }
    }

    implicit val fmt: Format[AttributeUpdate] = Format(theReads, theWrites)
  }

  /**
    * carry the values of the attributes of a PhysicalEntity
    */
  case class PhysicalEntity(ObjectName: String,
                            Timestamp: Option[Either[String, Long]] = None,
                            EntityType: Option[Array[Int]] = None,
                            EntityIdentifier: Option[Array[Int]] = None,
                            Coordinates: Option[Coordinates] = None,
                            ForceIdentifier: Option[Int] = None,
                            Marking: Option[String] = None,
                            attributes: Option[AttributesMap] = None) extends WeblvcMsg with AttributeUpdateMsg {

    val MessageKind = PhysicalEntity.MessageKind
    val ObjectType = PhysicalEntity.ObjectType
  }

  object PhysicalEntity {
    val ObjectType = phyEntity
    val MessageKind = "AttributeUpdate"

    // the list of all field names but not "attributes"
    private val omitList = (for (f <- PhysicalEntity("").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[PhysicalEntity] {
      def reads(js: JsValue): JsResult[PhysicalEntity] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind) &&
          (js \ "ObjectType").asOpt[String].contains(ObjectType)) {
          JsSuccess(new PhysicalEntity(
            (js \ "ObjectName").as[String],
            (js \ "Timestamp").asOpt[Either[String, Long]],
            (js \ "EntityType").asOpt[Array[Int]],
            (js \ "EntityIdentifier").asOpt[Array[Int]],
            (js \ "Coordinates").asOpt[Coordinates],
            (js \ "ForceIdentifier").asOpt[Int],
            (js \ "Marking").asOpt[String],
            AttributesMap.readAttributes(js, omitList)))
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[PhysicalEntity] {
      def writes(p: PhysicalEntity): JsValue = {
        val base = Json.obj("MessageKind" -> p.MessageKind, "ObjectType" -> p.ObjectType, "ObjectName" -> p.ObjectName)
        val theList = JsObject(List(
          p.Timestamp.map("Timestamp" -> Json.toJson(_)),
          p.EntityType.map("EntityType" -> Json.toJson(_)),
          p.EntityIdentifier.map("EntityIdentifier" -> Json.toJson(_)),
          p.Coordinates.map("Coordinates" -> Json.toJson(_)),
          p.ForceIdentifier.map("ForceIdentifier" -> JsNumber(_)),
          p.Marking.map("Marking" -> JsString(_))).flatten
        )
        p.attributes match {
          case Some(att) => base ++ theList ++ Json.toJson[AttributesMap](att).asInstanceOf[JsObject]
          case None => base ++ theList
        }
      }
    }

    implicit val fmt: Format[PhysicalEntity] = Format(theReads, theWrites)
  }

  /**
    * represent the type and number of subordinate entities or aggregates that are
    * not represented by individual object instances. Can also represent the damage
    * status of unpublished subordinate entity types.
    *
    * @param ObjectType  the main object type
    * @param count       the number of subordinate entities or aggregates
    * @param DamageState damage status, default None
    */
  case class SilentType(ObjectType: String, count: Int, DamageState: Option[String] = None)

  object SilentType {
    implicit val fmt = Json.format[SilentType]
  }

  /**
    * carry the values of the attributes of an AggregateEntity
    */
  case class AggregateEntity(ObjectName: String,
                             Timestamp: Option[Either[String, Long]] = None,
                             EntityType: Option[Array[Int]] = None,
                             EntityIdentifier: Option[Array[Int]] = None,
                             Coordinates: Option[Coordinates] = None,
                             ForceIdentifier: Option[Int] = None,
                             Marking: Option[String] = None,
                             AggregateState: Option[Int] = None,
                             Formation: Option[Int] = None,
                             Dimensions: Option[Array[Double]] = None,
                             Subordinates: Option[Array[String]] = None,
                             AggregateSubordinates: Option[Array[String]] = None,
                             SilentEntities: Option[Array[SilentType]] = None,
                             SilentAggregates: Option[Array[SilentType]] = None,
                             SilentEntitiesDamageState: Option[Array[SilentType]] = None,
                             attributes: Option[AttributesMap] = None) extends WeblvcMsg with AttributeUpdateMsg {

    val MessageKind = AggregateEntity.MessageKind
    val ObjectType = AggregateEntity.ObjectType
  }

  object AggregateEntity {
    val ObjectType = aggEntity
    val MessageKind = "AttributeUpdate"

    // the list of all field names but not "attributes"
    private val omitList = (for (f <- AggregateEntity("").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[AggregateEntity] {
      def reads(js: JsValue): JsResult[AggregateEntity] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind) &&
          (js \ "ObjectType").asOpt[String].contains(ObjectType)) {
          JsSuccess(new AggregateEntity(
            (js \ "ObjectName").as[String],
            (js \ "Timestamp").asOpt[Either[String, Long]],
            (js \ "EntityType").asOpt[Array[Int]],
            (js \ "EntityIdentifier").asOpt[Array[Int]],
            (js \ "Coordinates").asOpt[Coordinates],
            (js \ "ForceIdentifier").asOpt[Int],
            (js \ "Marking").asOpt[String],
            (js \ "AggregateState").asOpt[Int],
            (js \ "Formation").asOpt[Int],
            (js \ "Dimensions").asOpt[Array[Double]],
            (js \ "Subordinates").asOpt[Array[String]],
            (js \ "AggregateSubordinates").asOpt[Array[String]],
            (js \ "SilentEntities").asOpt[Array[SilentType]],
            (js \ "SilentAggregates").asOpt[Array[SilentType]],
            (js \ "SilentEntitiesDamageState").asOpt[Array[SilentType]],
            AttributesMap.readAttributes(js, omitList)))
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[AggregateEntity] {
      def writes(p: AggregateEntity): JsValue = {
        val base = Json.obj("MessageKind" -> p.MessageKind, "ObjectType" -> p.ObjectType, "ObjectName" -> p.ObjectName)
        val theList = JsObject(List(
          p.Timestamp.map("Timestamp" -> Json.toJson(_)),
          p.EntityType.map("EntityType" -> Json.toJson(_)),
          p.EntityIdentifier.map("EntityIdentifier" -> Json.toJson(_)),
          p.Coordinates.map("Coordinates" -> Json.toJson(_)),
          p.ForceIdentifier.map("ForceIdentifier" -> JsNumber(_)),
          p.Marking.map("Marking" -> JsString(_)),
          p.AggregateState.map("AggregateState" -> JsNumber(_)),
          p.Formation.map("Formation" -> JsNumber(_)),
          p.Dimensions.map("Dimensions" -> Json.toJson(_)),
          p.Subordinates.map("Subordinates" -> Json.toJson(_)),
          p.AggregateSubordinates.map("AggregateSubordinates" -> Json.toJson(_)),
          p.SilentEntities.map("SilentEntities" -> Json.toJson(_)),
          p.SilentAggregates.map("SilentAggregates" -> Json.toJson(_)),
          p.SilentEntitiesDamageState.map("SilentEntitiesDamageState" -> Json.toJson(_))).flatten)

        p.attributes match {
          case Some(att) => base ++ theList ++ Json.toJson[AttributesMap](att).asInstanceOf[JsObject]
          case None => base ++ theList
        }
      }
    }

    implicit val fmt: Format[AggregateEntity] = Format(theReads, theWrites)
  }

  /**
    * used in EnvironmentalEntity to represent a geometry record
    *
    * @param `Type`      either “LineString”, “Polygon”, “Point”, or “Line”.
    * @param coordinates The coordinates of the vertex or vertices of the object
    */
  case class GeometryRecord(`Type`: String, coordinates: Option[Either[Array[Double], Array[Array[Double]]]] = None) {

    def this(geomType: String, coordinates: Array[Double]) = this(geomType, Option(Left(coordinates)))

    def this(geomType: String, coordinates: Array[Array[Double]]) = this(geomType, Option(Right(coordinates)))
  }

  object GeometryRecord {
    implicit val fmt = Json.format[GeometryRecord]
  }

  /**
    * carry the values of the attributes of a EnvironmentalEntity
    */
  case class EnvironmentalEntity(ObjectName: String,
                                 Timestamp: Option[Either[String, Long]] = None,
                                 `Type`: Option[Array[Int]] = None,
                                 ProcessIdentifier: Option[Array[Int]] = None,
                                 ModelType: Option[Int] = None,
                                 EnvironmentProcessActive: Option[Boolean] = None,
                                 SequenceNumber: Option[Int] = None,
                                 GeometryRecords: Option[Array[GeometryRecord]] = None,
                                 attributes: Option[AttributesMap] = None) extends WeblvcMsg with AttributeUpdateMsg {

    val MessageKind = EnvironmentalEntity.MessageKind
    val ObjectType = EnvironmentalEntity.ObjectType
  }

  object EnvironmentalEntity {
    val ObjectType = envEntity
    val MessageKind = "AttributeUpdate"

    // the list of all field names but not "attributes"
    private val omitList = (for (f <- EnvironmentalEntity("").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[EnvironmentalEntity] {
      def reads(js: JsValue): JsResult[EnvironmentalEntity] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind) &&
          (js \ "ObjectType").asOpt[String].contains(ObjectType)) {
          JsSuccess(new EnvironmentalEntity(
            (js \ "ObjectName").as[String],
            (js \ "Timestamp").asOpt[Either[String, Long]],
            (js \ "Type").asOpt[Array[Int]],
            (js \ "ProcessIdentifier").asOpt[Array[Int]],
            (js \ "ModelType").asOpt[Int],
            (js \ "EnvironmentProcessActive").asOpt[Boolean],
            (js \ "SequenceNumber").asOpt[Int],
            (js \ "GeometryRecords").asOpt[Array[GeometryRecord]],
            AttributesMap.readAttributes(js, omitList)))
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[EnvironmentalEntity] {
      def writes(p: EnvironmentalEntity): JsValue = {
        val base = Json.obj("MessageKind" -> p.MessageKind, "ObjectType" -> p.ObjectType, "ObjectName" -> p.ObjectName)
        val theList = JsObject(List(
          p.Timestamp.map("Timestamp" -> Json.toJson(_)),
          p.`Type`.map("Type" -> Json.toJson(_)),
          p.ProcessIdentifier.map("ProcessIdentifier" -> Json.toJson(_)),
          p.ModelType.map("ModelType" -> JsNumber(_)),
          p.EnvironmentProcessActive.map("EnvironmentProcessActive" -> JsBoolean(_)),
          p.SequenceNumber.map("SequenceNumber" -> JsNumber(_)),
          p.GeometryRecords.map("GeometryRecords" -> Json.toJson(_))).flatten)

        p.attributes match {
          case Some(att) => base ++ theList ++ Json.toJson[AttributesMap](att).asInstanceOf[JsObject]
          case None => base ++ theList
        }
      }
    }

    implicit val fmt: Format[EnvironmentalEntity] = Format(theReads, theWrites)
  }

  /**
    * represents the type of modulation used for radio transmission.
    */
  case class ModulationType(SpreadSpectrum: Option[Int] = None, Major: Option[Int] = None,
                            Detail: Option[Int] = None, System: Option[Int] = None)

  object ModulationType {
    implicit val fmt = Json.format[ModulationType]
  }

  /**
    * the Antenna Pattern Parameters
    */
  case class AntennaPatternParameters(BeamDirection: Option[Array[Double]] = None, AzimuthBeamwidth: Option[Double] = None,
                                      ElevationBeamwidth: Option[Double] = None, ReferenceSystem: Option[Int] = None,
                                      Ez: Option[Double] = None, Ex: Option[Double] = None, Phase: Option[Double] = None)

  object AntennaPatternParameters {
    implicit val fmt = Json.format[AntennaPatternParameters]
  }

  // todo check attributes
  /**
    * carry the values of the attributes of a RadioTransmitter
    */
  case class RadioTransmitter(ObjectName: String,
                              Timestamp: Option[Either[String, Long]] = None,
                              EntityIdentifier: Option[Array[Int]] = None,
                              HostObjectName: Option[String] = None,
                              RadioIndex: Option[Int] = None,
                              RadioEntityType: Option[Array[Int]] = None,
                              TransmitState: Option[Int] = None,
                              InputSource: Option[Array[Double]] = None,
                              WorldAntennaLocation: Option[Array[Double]] = None,
                              RelativeAntennaLocation: Option[Array[Double]] = None,
                              AntennaPatternType: Option[Int] = None,
                              Frequency: Option[Int] = None,
                              TransmitBandwidth: Option[Double] = None,
                              Power: Option[Double] = None,
                              ModulationType: Option[ModulationType] = None,
                              CryptoMode: Option[Int] = None,
                              CryptoSystem: Option[Int] = None,
                              CryptoKey: Option[Long] = None,
                              AntennaPatternParameters: Option[AntennaPatternParameters] = None,
                              FrequencyHopInUse: Option[Boolean] = None,
                              PseudoNoiseInUse: Option[Boolean] = None,
                              TimeHopInUse: Option[Boolean] = None,
                              attributes: Option[AttributesMap] = None) extends WeblvcMsg with AttributeUpdateMsg {

    val MessageKind = RadioTransmitter.MessageKind
    val ObjectType = RadioTransmitter.ObjectType
  }

  object RadioTransmitter {
    val ObjectType = radioTrans
    val MessageKind = "AttributeUpdate"

    // the list of all field names but not "attributes"
    private val omitList = (for (f <- RadioTransmitter("").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[RadioTransmitter] {
      def reads(js: JsValue): JsResult[RadioTransmitter] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind) &&
          (js \ "ObjectType").asOpt[String].contains(ObjectType)) {
          JsSuccess(new RadioTransmitter(
            (js \ "ObjectName").as[String],
            (js \ "Timestamp").asOpt[Either[String, Long]],
            (js \ "EntityIdentifier").asOpt[Array[Int]],
            (js \ "HostObjectName").asOpt[String],
            (js \ "RadioIndex").asOpt[Int],
            (js \ "RadioEntityType").asOpt[Array[Int]],
            (js \ "TransmitState").asOpt[Int],
            (js \ "InputSource").asOpt[Array[Double]],
            (js \ "WorldAntennaLocation").asOpt[Array[Double]],
            (js \ "RelativeAntennaLocation").asOpt[Array[Double]],
            (js \ "AntennaPatternType").asOpt[Int],
            (js \ "Frequency").asOpt[Int],
            (js \ "TransmitBandwidth").asOpt[Double],
            (js \ "Power").asOpt[Double],
            (js \ "ModulationType").asOpt[ModulationType],
            (js \ "CryptoMode").asOpt[Int],
            (js \ "CryptoSystem").asOpt[Int],
            (js \ "CryptoKey").asOpt[Long],
            (js \ "AntennaPatternParameters").asOpt[AntennaPatternParameters],
            (js \ "FrequencyHopInUse").asOpt[Boolean],
            (js \ "PseudoNoiseInUse").asOpt[Boolean],
            (js \ "TimeHopInUse").asOpt[Boolean],
            AttributesMap.readAttributes(js, omitList)))
        } else {
          JsError(s"Error reading RadioTransmitter message: $js")
        }
      }
    }

    val theWrites = new Writes[RadioTransmitter] {
      def writes(p: RadioTransmitter): JsValue = {
        val base = Json.obj("MessageKind" -> p.MessageKind, "ObjectType" -> p.ObjectType, "ObjectName" -> p.ObjectName)
        val theList = JsObject(List(
          p.Timestamp.map("Timestamp" -> Json.toJson(_)),
          p.EntityIdentifier.map("EntityIdentifier" -> Json.toJson(_)),
          p.HostObjectName.map("HostObjectName" -> JsString(_)),
          p.RadioIndex.map("RadioIndex" -> JsNumber(_)),
          p.RadioEntityType.map("RadioEntityType" -> Json.toJson(_)),
          p.TransmitState.map("TransmitState" -> JsNumber(_)),
          p.InputSource.map("InputSource" -> Json.toJson(_)),
          p.WorldAntennaLocation.map("WorldAntennaLocation" -> Json.toJson(_)),
          p.RelativeAntennaLocation.map("RelativeAntennaLocation" -> Json.toJson(_)),
          p.AntennaPatternType.map("AntennaPatternType" -> JsNumber(_)),
          p.Frequency.map("Frequency" -> JsNumber(_)),
          p.TransmitBandwidth.map("TransmitBandwidth" -> JsNumber(_)),
          p.Power.map("Power" -> JsNumber(_)),
          p.ModulationType.map("ModulationType" -> Json.toJson(_)),
          p.CryptoMode.map("CryptoMode" -> JsNumber(_)),
          p.CryptoSystem.map("CryptoSystem" -> JsNumber(_)),
          p.CryptoKey.map("CryptoKey" -> JsNumber(_)),
          p.AntennaPatternParameters.map("AntennaPatternParameters" -> Json.toJson(_)),
          p.FrequencyHopInUse.map("FrequencyHopInUse" -> JsBoolean(_)),
          p.PseudoNoiseInUse.map("PseudoNoiseInUse" -> JsBoolean(_)),
          p.TimeHopInUse.map("TimeHopInUse" -> JsBoolean(_))).flatten)

        p.attributes match {
          case Some(att) => base ++ theList ++ Json.toJson[AttributesMap](att).asInstanceOf[JsObject]
          case None => base ++ theList
        }
      }
    }

    implicit val fmt: Format[RadioTransmitter] = Format(theReads, theWrites)
  }

  /**
    * message to inform the recipient that an object it has been updating no longer exists,
    * or no longer matches subscription filters.
    *
    * @param ObjectName the object name
    * @param Timestamp  the time at which the data is valid
    */
  case class ObjectDeleted(ObjectName: String, Timestamp: Option[Either[String, Long]] = None) extends WeblvcMsg {

    val MessageKind = ObjectDeleted.MessageKind

    def this(ObjectName: String, Timestamp: String) = this(ObjectName, Option(Left(Timestamp)))

    def this(ObjectName: String, Timestamp: Long) = this(ObjectName, Option(Right(Timestamp)))

  }

  object ObjectDeleted {
    val MessageKind = "ObjectDeleted"

    val fmtx = Json.format[ObjectDeleted]

    val theReads = new Reads[ObjectDeleted] {
      def reads(js: JsValue): JsResult[ObjectDeleted] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[ObjectDeleted] {
      def writes(c: ObjectDeleted) = Json.obj("MessageKind" -> MessageKind) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  //------------------------------------------------------------------------------------
  //-------------------interaction messages--------------------------------------------------
  //------------------------------------------------------------------------------------

  /**
    * general notification of simulation events interactions
    *
    * @param InteractionType indicates the type of the Interaction conveyed by a WebLVC message, similar to HLA InteractionClass or DIS PDU Kind
    * @param Timestamp       the time at which the data is valid.
    * @param attributes      dynamic set of attributes
    */

  case class Interaction(InteractionType: String,
                         Timestamp: Option[Either[String, Long]] = None,
                         attributes: Option[AttributesMap] = None) extends WeblvcMsg with InteractionMsg {

    val MessageKind = Interaction.MessageKind
  }

  object Interaction {
    val MessageKind = "Interaction"

    // the list of all field names but not "attributes"
    private val omitList = (for (f <- Interaction("").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[Interaction] {
      def reads(js: JsValue): JsResult[Interaction] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          JsSuccess(new Interaction(
            (js \ "InteractionType").as[String],
            (js \ "Timestamp").asOpt[Either[String, Long]],
            AttributesMap.readAttributes(js, omitList)))
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val pWrites: Writes[Interaction] =
      ((JsPath \ "InteractionType").write[String] and
        (JsPath \ "Timestamp").writeNullable[Either[String, Long]] and
        JsPath.writeNullable[AttributesMap]) (unlift(Interaction.unapply))

    val theWrites = new Writes[Interaction] {
      def writes(p: Interaction) = {
        p.attributes match {
          case Some(att) => Json.obj("MessageKind" -> MessageKind) ++ Json.toJson(att).asInstanceOf[JsObject] ++ pWrites.writes(p).asInstanceOf[JsObject]
          case None => Json.obj("MessageKind" -> MessageKind) ++ pWrites.writes(p).asInstanceOf[JsObject]
        }
      }
    }

    implicit val fmt: Format[Interaction] = Format(theReads, theWrites)
  }

  /**
    * WeaponFire interaction message
    */
  case class WeaponFire(Timestamp: Option[Either[String, Long]] = None,
                        AttackerId: Option[String] = None,
                        TargetId: Option[String] = None,
                        MunitionType: Option[Array[Int]] = None,
                        MunitionId: Option[String] = None,
                        EventId: Option[String] = None,
                        WarheadType: Option[Int] = None,
                        FireMissionIndex: Option[Long] = None,
                        FuseType: Option[Int] = None,
                        Quantity: Option[Int] = None,
                        Rate: Option[Int] = None,
                        Range: Option[Long] = None,
                        Coordinates: Option[Coordinates] = None) extends WeblvcMsg with InteractionMsg {

    val MessageKind = WeaponFire.MessageKind
    val InteractionType = WeaponFire.InteractionType
  }

  object WeaponFire {
    val InteractionType = weaponFire
    val MessageKind = "Interaction"

    val fmtx = Json.format[WeaponFire]

    val theReads = new Reads[WeaponFire] {
      def reads(js: JsValue): JsResult[WeaponFire] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind) &&
          (js \ "InteractionType").asOpt[String].contains(InteractionType)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[WeaponFire] {
      def writes(c: WeaponFire) = Json.obj("MessageKind" -> MessageKind, "InteractionType" -> InteractionType) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)

  }

  /**
    * Munition Detonation interaction message
    */
  case class MunitionDetonation(Timestamp: Option[Either[String, Long]] = None,
                                AttackerId: Option[String] = None,
                                TargetId: Option[String] = None,
                                MunitionType: Option[Array[Int]] = None,
                                MunitionId: Option[String] = None,
                                EventId: Option[String] = None,
                                WarheadType: Option[Int] = None,
                                Result: Option[Int] = None,
                                FuseType: Option[Int] = None,
                                Quantity: Option[Int] = None,
                                Rate: Option[Int] = None,
                                EntityLocation: Option[Array[Double]] = None,
                                Coordinates: Option[Coordinates] = None) extends WeblvcMsg with InteractionMsg {

    val MessageKind = MunitionDetonation.MessageKind
    val InteractionType = MunitionDetonation.InteractionType
  }

  object MunitionDetonation {
    val InteractionType = munitionDetonation
    val MessageKind = "Interaction"

    val fmtx = Json.format[MunitionDetonation]

    val theReads = new Reads[MunitionDetonation] {
      def reads(js: JsValue): JsResult[MunitionDetonation] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind) &&
          (js \ "InteractionType").asOpt[String].contains(InteractionType)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[MunitionDetonation] {
      def writes(c: MunitionDetonation) = Json.obj("MessageKind" -> MessageKind, "InteractionType" -> InteractionType) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * Start Resume interaction message. directs one or more simulators to start or resume simulating,
    * or start or resume simulation of a specific entity.
    */
  case class StartResume(Timestamp: Option[Either[String, Long]] = None,
                         ReceivingEntity: Array[Int],
                         RequestIdentifier: Int,
                         RealWorldTime: Double,
                         SimulationTime: Double,
                         OriginatingEntity: Option[Array[Int]] = None) extends WeblvcMsg with InteractionMsg {

    val MessageKind = StartResume.MessageKind
    val InteractionType = StartResume.InteractionType
  }

  object StartResume {
    val InteractionType = startResume
    val MessageKind = "Interaction"

    val fmtx = Json.format[StartResume]

    val theReads = new Reads[StartResume] {
      def reads(js: JsValue): JsResult[StartResume] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind) &&
          (js \ "InteractionType").asOpt[String].contains(InteractionType)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[StartResume] {
      def writes(c: StartResume) = Json.obj("MessageKind" -> MessageKind, "InteractionType" -> InteractionType) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * directs one or more simulators to stop simulating, or stop simulation of a specific entity.
    */
  case class StopFreeze(Timestamp: Option[Either[String, Long]] = None,
                        ReceivingEntity: Array[Int],
                        OriginatingEntity: Option[Array[Int]] = None,
                        RealWorldTime: Option[Double] = None,
                        Reason: Option[Int] = None,
                        ReflectValues: Option[Boolean] = None,
                        RunInternalSimulationClock: Option[Boolean] = None,
                        UpdateAttributes: Option[Boolean] = None) extends WeblvcMsg with InteractionMsg {

    val MessageKind = StopFreeze.MessageKind
    val InteractionType = StopFreeze.InteractionType
  }

  object StopFreeze {
    val InteractionType = stopFreeze
    val MessageKind = "Interaction"

    val fmtx = Json.format[StopFreeze]

    val theReads = new Reads[StopFreeze] {
      def reads(js: JsValue): JsResult[StopFreeze] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind) &&
          (js \ "InteractionType").asOpt[String].contains(InteractionType)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[StopFreeze] {
      def writes(c: StopFreeze) = Json.obj("MessageKind" -> MessageKind, "InteractionType" -> InteractionType) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * represents the wireless transmission and reception of audio or digital data via electromagnetic waves.
    */
  case class RadioSignal(Timestamp: Option[Either[String, Long]] = None,
                         RadioIdentifier: Option[String] = None,
                         EncodingClass: Option[Int] = None,
                         EncodingType: Option[Int] = None,
                         TDLType: Option[Int] = None,
                         SampleRate: Option[Int] = None,
                         SampleCount: Option[Int] = None,
                         SampleData: Option[String] = None,
                         DatabaseIndex: Option[Int] = None,
                         UserProtocolID: Option[Int] = None) extends WeblvcMsg with InteractionMsg {

    val MessageKind = RadioSignal.MessageKind
    val InteractionType = RadioSignal.InteractionType
  }

  object RadioSignal {
    val InteractionType = radioSignal
    val MessageKind = "Interaction"

    val fmtx = Json.format[RadioSignal]

    val theReads = new Reads[RadioSignal] {
      def reads(js: JsValue): JsResult[RadioSignal] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind) &&
          (js \ "InteractionType").asOpt[String].contains(InteractionType)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[RadioSignal] {
      def writes(c: RadioSignal) = Json.obj("MessageKind" -> MessageKind, "InteractionType" -> InteractionType) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  //------------------------------------------------------------------------------------
  //-------------------subscription messages--------------------------------------------------
  //------------------------------------------------------------------------------------

  /**
    * Subscribe to AttributeUpdate type messages for the desired object types.
    * Request sent by a client to notify the server of its interest in a subset of objects,
    * based on filter parameters supplied in the message.
    *
    * @param ObjectType type of objects to which to subscribe. An exact match is required.
    * @param Filters    properties which specify attribute filters
    */
  case class SubscribeObject(ObjectType: String,
                             Filters: Option[Either[Filter, FilterExpression]] = None)
    extends WeblvcMsg {
    val MessageKind = SubscribeObject.MessageKind
  }

  object SubscribeObject {
    val MessageKind = "SubscribeObject"

    val theReads = new Reads[SubscribeObject] {
      def reads(js: JsValue): JsResult[SubscribeObject] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          JsSuccess(new SubscribeObject((js \ "ObjectType").as[String],
            Json.fromJson[Either[Filter, FilterExpression]](js).asOpt))
        } else {
          JsError(s"Error reading SubscribeObject message: $js")
        }
      }
    }

    val theWrites = new Writes[SubscribeObject] {
      def writes(p: SubscribeObject) = {
        val base = Json.obj("MessageKind" -> p.MessageKind, "ObjectType" -> p.ObjectType)
        p.Filters match {
          case Some(f) => base ++ Json.toJson(f).asInstanceOf[JsObject]
          case None => base
        }
      }
    }

    implicit val fmt: Format[SubscribeObject] = Format(theReads, theWrites)
  }

  /**
    * remove object subscriptions request.
    *
    * @param ObjectType type of object to which to unsubscribe
    */
  case class UnsubscribeObject(ObjectType: String) extends WeblvcMsg {

    val MessageKind = UnsubscribeObject.MessageKind
  }

  object UnsubscribeObject {
    val MessageKind = "UnsubscribeObject"

    val fmtx = Json.format[UnsubscribeObject]

    val theReads = new Reads[UnsubscribeObject] {
      def reads(js: JsValue): JsResult[UnsubscribeObject] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[UnsubscribeObject] {
      def writes(c: UnsubscribeObject) = Json.obj("MessageKind" -> MessageKind) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * a client request specifying which WebLVC Interaction messages are sent to it by the server
    *
    * @param InteractionType
    * @param Timestamp
    * @param Filters
    */
  case class SubscribeInteraction(InteractionType: String,
                                  Timestamp: Option[Either[String, Long]] = None,
                                  Filters: Option[Either[Filter, FilterExpression]] = None) extends WeblvcMsg {

    val MessageKind = SubscribeInteraction.MessageKind
  }

  object SubscribeInteraction {
    val MessageKind = "SubscribeInteraction"

    val theReads = new Reads[SubscribeInteraction] {
      def reads(js: JsValue): JsResult[SubscribeInteraction] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          JsSuccess(new SubscribeInteraction((js \ "InteractionType").as[String],
            (js \ "Timestamp").asOpt[Either[String, Long]],
            Json.fromJson[Either[Filter, FilterExpression]](js).asOpt))
        } else {
          JsError(s"Error reading SubscribeInteraction message: $js")
        }
      }
    }

    val theWrites = new Writes[SubscribeInteraction] {
      def writes(p: SubscribeInteraction) = {
        val base = Json.obj("MessageKind" -> p.MessageKind, "InteractionType" -> p.InteractionType, "Timestamp" -> p.Timestamp)
        p.Filters match {
          case Some(f) => base ++ Json.toJson(f).asInstanceOf[JsObject]
          case None => base
        }
      }
    }

    implicit val fmt: Format[SubscribeInteraction] = Format(theReads, theWrites)
  }

  /**
    * remove the SubscribeInteraction
    *
    * @param InteractionType
    */
  case class UnsubscribeInteraction(InteractionType: String) extends WeblvcMsg {

    val MessageKind = UnsubscribeInteraction.MessageKind
  }

  object UnsubscribeInteraction {
    val MessageKind = "UnsubscribeInteraction"

    val fmtx = Json.format[UnsubscribeInteraction]

    val theReads = new Reads[UnsubscribeInteraction] {
      def reads(js: JsValue): JsResult[UnsubscribeInteraction] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[UnsubscribeInteraction] {
      def writes(c: UnsubscribeInteraction) = Json.obj("MessageKind" -> MessageKind) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * client request for the server status log of the client
    *
    * @param Length
    * @param Offset
    */
  case class StatusLogRequest(Length: Option[Int] = None, Offset: Option[Int] = None) extends WeblvcMsg {

    val MessageKind = StatusLogRequest.MessageKind
  }

  object StatusLogRequest {
    val MessageKind = "StatusLogRequest"

    val fmtx = Json.format[StatusLogRequest]

    val theReads = new Reads[StatusLogRequest] {
      def reads(js: JsValue): JsResult[StatusLogRequest] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[StatusLogRequest] {
      def writes(c: StatusLogRequest) = Json.obj("MessageKind" -> MessageKind) ++ fmtx.writes(c)
    }

    implicit val fmt = Format(theReads, theWrites)
  }

  /**
    * the server response to the client StatusLogRequest
    *
    * @param Status
    */
  case class StatusLogResponse(Status: Array[StatusLog]) extends WeblvcMsg {

    val MessageKind = StatusLogResponse.MessageKind
  }

  object StatusLogResponse {
    val MessageKind = "StatusLogResponse"

    val fmtx = Json.format[StatusLogResponse]

    val theReads = new Reads[StatusLogResponse] {
      def reads(js: JsValue): JsResult[StatusLogResponse] = {
        if ((js \ "MessageKind").asOpt[String].contains(MessageKind)) {
          fmtx.reads(js)
        } else {
          JsError(s"Error reading message: $js")
        }
      }
    }

    val theWrites = new Writes[StatusLogResponse] {
      def writes(c: StatusLogResponse) = Json.obj("MessageKind" -> MessageKind) ++ fmtx.writes(c)
    }

    implicit val fmt: Format[StatusLogResponse] = Format(theReads, theWrites)
  }

  /**
    * the Interaction message object
    */
  object InteractionMsg {

    val theReads = new Reads[InteractionMsg] {
      def reads(json: JsValue): JsResult[InteractionMsg] = {
        (json \ "MessageKind").asOpt[String].map({
          case "Interaction" =>
            (json \ "InteractionType").asOpt[String].map({
              case `weaponFire` => WeaponFire.fmt.reads(json)
              case `munitionDetonation` => MunitionDetonation.fmt.reads(json)
              case `startResume` => StartResume.fmt.reads(json)
              case `stopFreeze` => StopFreeze.fmt.reads(json)
              case `radioSignal` => RadioSignal.fmt.reads(json)
              // case `transferOwnership` => TransferOwnership.fmt.reads(json)     // todo
              case _ => Interaction.fmt.reads(json)
            }).getOrElse(JsError("Error no InteractionType"))

          case err => JsError(s"Error unknown Interaction message kind: $err")
        }).getOrElse(JsError("Error no MessageKind"))
      }
    }

    val theWrites = new Writes[InteractionMsg] {
      def writes(msg: InteractionMsg) = {
        msg match {
          case s: WeaponFire => WeaponFire.fmt.writes(s)
          case s: MunitionDetonation => MunitionDetonation.fmt.writes(s)
          case s: StartResume => StartResume.fmt.writes(s)
          case s: StopFreeze => StopFreeze.fmt.writes(s)
          case s: RadioSignal => RadioSignal.fmt.writes(s)
          // case s: TransferOwnership => TransferOwnership.fmt.writes(s)  // todo
          case s: Interaction => Interaction.fmt.writes(s)
          case _ => JsNull
        }
      }
    }

    implicit val fmt: Format[InteractionMsg] = Format(theReads, theWrites)
  }

  /**
    * the AttributeUpdateMsg message object
    */
  object AttributeUpdateMsg {

    val theReads = new Reads[AttributeUpdateMsg] {
      def reads(json: JsValue): JsResult[AttributeUpdateMsg] = {
        (json \ "MessageKind").asOpt[String].map({
          case "AttributeUpdate" =>
            (json \ "ObjectType").asOpt[String].map({
              case `phyEntity` => PhysicalEntity.fmt.reads(json)
              case `aggEntity` => AggregateEntity.fmt.reads(json)
              case `envEntity` => EnvironmentalEntity.fmt.reads(json)
              case `radioTrans` => RadioTransmitter.fmt.reads(json)
              case _ => AttributeUpdate.fmt.reads(json)
            }).getOrElse(JsError("Error no ObjectType"))

          case err => JsError(s"Error unknown AttributeUpdate message kind: $err")
        }).getOrElse(JsError("Error no MessageKind"))
      }
    }

    val theWrites = new Writes[AttributeUpdateMsg] {
      def writes(msg: AttributeUpdateMsg) = {
        msg match {
          case s: EnvironmentalEntity => EnvironmentalEntity.fmt.writes(s)
          case s: PhysicalEntity => PhysicalEntity.fmt.writes(s)
          case s: AggregateEntity => AggregateEntity.fmt.writes(s)
          case s: RadioTransmitter => RadioTransmitter.fmt.writes(s)
          case s: AttributeUpdate => AttributeUpdate.fmt.writes(s)
          case _ => JsNull
        }
      }
    }

    implicit val fmt: Format[AttributeUpdateMsg] = Format(theReads, theWrites)
  }

  /**
    * the weblvc message object
    */
  object WeblvcMsg {

    val theReads = new Reads[WeblvcMsg] {
      def reads(json: JsValue): JsResult[WeblvcMsg] = {
        (json \ "MessageKind").asOpt[String].map({
          case "Connect" => Connect.fmt.reads(json)
          case "ConnectResponse" => ConnectResponse.fmt.reads(json)
          case "Configure" => Configure.fmt.reads(json)
          case "ConfigureResponse" => ConfigureResponse.fmt.reads(json)
          case "AttributeUpdate" => AttributeUpdateMsg.fmt.reads(json).asInstanceOf[JsResult[WeblvcMsg]]
          case "ObjectDeleted" => ObjectDeleted.fmt.reads(json)
          case "SubscribeObject" => SubscribeObject.fmt.reads(json)
          case "SubscribeInteraction" => SubscribeInteraction.fmt.reads(json)
          case "UnsubscribeObject" => UnsubscribeObject.fmt.reads(json)
          case "UnsubscribeInteraction" => UnsubscribeInteraction.fmt.reads(json)
          case "StatusLogRequest" => StatusLogRequest.fmt.reads(json)
          case "StatusLogResponse" => StatusLogResponse.fmt.reads(json)
          case "Interaction" => InteractionMsg.fmt.reads(json).asInstanceOf[JsResult[WeblvcMsg]]
          case err => JsError(s"Error unknown Weblvc message kind: $err")
        }).getOrElse(JsError("Error no MessageKind"))
      }
    }

    val theWrites = new Writes[WeblvcMsg] {
      def writes(msg: WeblvcMsg) = {
        msg match {
          case s: Connect => Connect.fmt.writes(s)
          case s: ConnectResponse => ConnectResponse.fmt.writes(s)
          case s: Configure => Configure.fmt.writes(s)
          case s: ConfigureResponse => ConfigureResponse.fmt.writes(s)
          case s: AttributeUpdateMsg => AttributeUpdateMsg.fmt.writes(s)
          case s: ObjectDeleted => ObjectDeleted.fmt.writes(s)
          case s: SubscribeObject => SubscribeObject.fmt.writes(s)
          case s: SubscribeInteraction => SubscribeInteraction.fmt.writes(s)
          case s: UnsubscribeObject => UnsubscribeObject.fmt.writes(s)
          case s: UnsubscribeInteraction => UnsubscribeInteraction.fmt.writes(s)
          case s: StatusLogRequest => StatusLogRequest.fmt.writes(s)
          case s: StatusLogResponse => StatusLogResponse.fmt.writes(s)
          case s: InteractionMsg => InteractionMsg.fmt.writes(s)
          case _ => JsNull
        }
      }
    }

    implicit val fmt: Format[WeblvcMsg] = Format(theReads, theWrites)

    // convenience
    def toJsonString(msg: WeblvcMsg) = Json.toJson[WeblvcMsg](msg).toString()
  }


}
