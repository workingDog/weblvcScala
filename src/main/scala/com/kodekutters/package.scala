package com.kodekutters

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.extras.geojson._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * WebLVC standard object model protocol
 *
 * ref:  "WebLVC Draft Protocol Specification Version 0.4"
 * see SISO discussions: https//discussions.sisostds.org/index.htm?A2=SAC-PDG-WebLVC;7bd8624.1503
 *
 * Author: R. Wathelet March 2016
 * version: 0.4
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

  import WebLvcSupport._

  /**
   * the weblvc message base class
   *
   * @param MessageKind the string specifying the weblvc message kind. Required for all weblvc messages.
   */
  sealed abstract class WeblvcMsg(val MessageKind: String)

  object WeblvcMsg {

    val theReads = new Reads[WeblvcMsg] {
      def reads(json: JsValue): JsResult[WeblvcMsg] = {
        val msgK = (json \ "MessageKind").as[String]
        msgK match {
          case "Connect" => Connect.fmt.reads(json)
          case "ConnectResponse" => ConnectResponse.fmt.reads(json)
          case "Configure" => Configure.fmt.reads(json)
          case "ConfigureResponse" => ConfigureResponse.fmt.reads(json)
          case "AttributeUpdate" =>
            // specify on ObjectType
            (json \ "ObjectType").as[String] match {
              case `phyEntity` => PhysicalEntity.fmt.reads(json)
              case `aggEntity` => AggregateEntity.fmt.reads(json)
              case `envEntity` => EnvironmentalEntity.fmt.reads(json)
              case `radioTrans` => RadioTransmitter.fmt.reads(json)
              case _ => AttributeUpdate.fmt.reads(json)
            }
          case "ObjectDeleted" => ObjectDeleted.fmt.reads(json)
          case "SubscribeObject" => SubscribeObject.fmt.reads(json)
          case "SubscribeInteraction" => SubscribeInteraction.fmt.reads(json)
          case "UnsubscribeObject" => UnsubscribeObject.fmt.reads(json)
          case "UnsubscribeInteraction" => UnsubscribeInteraction.fmt.reads(json)
          case "StatusLogRequest" => StatusLogRequest.fmt.reads(json)
          case "StatusLogResponse" => StatusLogResponse.fmt.reads(json)
          case "Interaction" =>
            // specify on InteractionType
            (json \ "InteractionType").as[String] match {
              case `weaponFire` => WeaponFire.fmt.reads(json)
              case `munitionDetonation` => MunitionDetonation.fmt.reads(json)
              case `startResume` => StartResume.fmt.reads(json)
              case `stopFreeze` => StopFreeze.fmt.reads(json)
              case `radioSignal` => RadioSignal.fmt.reads(json)
              // case `transferOwnership` => TransferOwnership.fmt.reads(json)     // todo
              case _ => Interaction.fmt.reads(json)
            }

          case err => JsError(s"Error unknown Weblvc message kind: $err")
        }
      }
    }

    val theWrites = new Writes[WeblvcMsg] {
      def writes(msg: WeblvcMsg) = {
        val jsVal: JsValue = msg match {
          case s: Connect => Connect.fmt.writes(s)
          case s: ConnectResponse => ConnectResponse.fmt.writes(s)
          case s: Configure => Configure.fmt.writes(s)
          case s: ConfigureResponse => ConfigureResponse.fmt.writes(s)
          case s: EnvironmentalEntity => EnvironmentalEntity.fmt.writes(s)
          case s: PhysicalEntity => PhysicalEntity.fmt.writes(s)
          case s: AggregateEntity => AggregateEntity.fmt.writes(s)
          case s: RadioTransmitter => RadioTransmitter.fmt.writes(s)
          case s: AttributeUpdate => AttributeUpdate.fmt.writes(s)
          case s: ObjectDeleted => ObjectDeleted.fmt.writes(s)
          case s: SubscribeObject => SubscribeObject.fmt.writes(s)
          case s: SubscribeInteraction => SubscribeInteraction.fmt.writes(s)
          case s: UnsubscribeObject => UnsubscribeObject.fmt.writes(s)
          case s: UnsubscribeInteraction => UnsubscribeInteraction.fmt.writes(s)
          case s: StatusLogRequest => StatusLogRequest.fmt.writes(s)
          case s: StatusLogResponse => StatusLogResponse.fmt.writes(s)
          case s: WeaponFire => WeaponFire.fmt.writes(s)
          case s: MunitionDetonation => MunitionDetonation.fmt.writes(s)
          case s: StartResume => StartResume.fmt.writes(s)
          case s: StopFreeze => StopFreeze.fmt.writes(s)
          case s: RadioSignal => RadioSignal.fmt.writes(s)
          // case s: TransferOwnership => TransferOwnership.fmt.writes(s)  // todo
          case s: Interaction => Interaction.fmt.writes(s)
          case _ => JsObject(Seq.empty)
        }
        Json.obj("MessageKind" -> JsString(msg.MessageKind)) ++ jsVal.asInstanceOf[JsObject]
      }
    }

    implicit val fmt: Format[WeblvcMsg] = Format(theReads, theWrites)

    // convenience
    def toJsonString(msg: WeblvcMsg) = Json.toJson[WeblvcMsg](msg).toString()
  }

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

  sealed trait MinMax

  // todo
//  case class MinMax[T](min: T, max: T) extends MinMax
//
//  object MinMax {
//    implicit val fmt = Json.format[MinMax[T]]
//  }

  /**
   * to use in filters for testing values between the min and max values
   *
   * @param min the minimum lexicographical value
   * @param max the maximum lexicographical value
   */
  case class MinMaxString(min: String, max: String) extends MinMax

  object MinMaxString {
    implicit val fmt = Json.format[MinMaxString]
  }

  /**
   * to use in filters for testing values between the min and max values
   *
   * @param min the minimum value
   * @param max the maximum value
   */
  case class MinMaxDouble(min: Double, max: Double) extends MinMax

  object MinMaxDouble {
    implicit val fmt = Json.format[MinMaxDouble]
  }

  /**
   * to use in filters for testing values between the min and max values
   *
   * @param min the minimum array values
   * @param max the maximum array values
   */
  case class MinMaxArrayDouble(min: Array[Double], max: Array[Double]) extends MinMax

  object MinMaxArrayDouble {
    implicit val fmt = Json.format[MinMaxArrayDouble]
  }

  /**
   * to use in filters for testing values between the min and max values
   *
   * @param min the minimum array values
   * @param max the maximum array values
   */
  case class MinMaxArrayInt(min: Array[Int], max: Array[Int]) extends MinMax

  object MinMaxArrayInt {
    implicit val fmt = Json.format[MinMaxArrayInt]
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

    implicit val fmt: Format[Coordinates] = Format(theReads, theWrites)
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
   * a generic (key,value) dictionary, with key = an attribute name, and value = the attribute value
   */
  case class AttributesMap(nodes: mutable.ListMap[String, Any])

  object AttributesMap {

    def readAttributes(js: JsValue, omitList: List[String]): Option[AttributesMap] = {
      js match {
        case json: JsObject =>
          // read all fields but not the fields in the omitList
          val fList = json.fields.filterNot(p => omitList.contains(p._1))
          val theListMap = mutable.ListMap.empty ++= fList.collect {
            case (key, JsString(value)) => key -> value
            case (key, JsNumber(value)) => key -> value // BigDecimal <----- todo
            case (key, JsBoolean(value)) => key -> value
            case (key, JsArray(value)) => key -> value
          }
          if (theListMap.isEmpty) None else Some(new AttributesMap(theListMap))

        case x => JsError(s"Could not read KeyValue: $x"); None
      }
    }

    val theReads = new Reads[AttributesMap] {
      def reads(json: JsValue): JsResult[AttributesMap] = {
        json match {
          case js: JsObject =>
            val theListMap = mutable.ListMap.empty ++= js.fields.collect {
              case (key, JsString(value)) => key -> value
              case (key, JsNumber(value)) => key -> value // BigDecimal ?<----- todo
              case (key, JsBoolean(value)) => key -> value
              case (key, JsArray(value)) => key -> value
            }
            JsSuccess(new AttributesMap(theListMap))

          case x => JsError(s"Could not read KeyValue: $x")
        }
      }
    }

    val theWrites = new Writes[AttributesMap] {
      def writes(keyval: AttributesMap) = {
        val list = for ((k, v) <- keyval.nodes) yield {
          v match {
            case x: String => k -> JsString(x)
            case x: Double => k -> JsNumber(x)
            case x: Int => k -> JsNumber(x)
            case x: Float => k -> JsNumber(x.toDouble)
            case x: Long => k -> JsNumber(x)
            case x: Boolean => k -> JsBoolean(x)
            case x: BigDecimal => k -> JsNumber(x)
            case x: Array[Int] => k -> JsArray(for (i <- x.toSeq) yield JsNumber(i))
            case x: Array[Double] => k -> JsArray(for (i <- x.toSeq) yield JsNumber(i))
            case x: Array[BigDecimal] => k -> JsArray(for (i <- x.toSeq) yield JsNumber(i))
            case x: Array[String] => k -> JsArray(for (i <- x.toSeq) yield JsString(i))
            case x: Array[Boolean] => k -> JsArray(for (i <- x.toSeq) yield JsBoolean(i))
            case x => k -> JsNull //  <------ todo other types
          }
        }
        JsObject(list)
      }
    }

    implicit val fmt: Format[AttributesMap] = Format(theReads, theWrites)
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
  case class Connect(
    ClientName: String,
    WebLVCVersion: Option[Double] = None,
    Messages: Option[Array[WeblvcMsg]] = None
  ) extends WeblvcMsg("Connect")

  object Connect {
    implicit val fmt = Json.format[Connect]
  }

  /**
   * server response to a client Connect request.
   *
   * @param Connected     whether the connection was accepted
   * @param WebLVCVersion version to use
   * @param Errors        possible set of errors encountered
   */
  case class ConnectResponse(
    Connected: Boolean,
    WebLVCVersion: Option[Double] = None,
    Errors: Option[Array[StatusLog]] = None
  ) extends WeblvcMsg("ConnectResponse")

  object ConnectResponse {
    implicit val fmt = Json.format[ConnectResponse]
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
  case class Configure(
      TimestampFormat: Option[Int] = None,
      CoordinateReferenceSystem: Option[String] = Some(defaultECEFCartesian),
      ServerDeadReckoning: Option[ServerDeadReckoning] = None,
      WorldBounds: Option[Either[Polygon[LngLatAlt], MultiPolygon[LngLatAlt]]] = None,
      ObjectBounds: Option[ObjectBounds] = None
  ) extends WeblvcMsg("Configure") {

    def this(
      TimestampFormat: Option[Int],
      CoordinateReferenceSystem: Option[String],
      ServerDeadReckoning: Option[ServerDeadReckoning],
      WorldBounds: Polygon[LngLatAlt],
      ObjectBounds: Option[ObjectBounds]
    ) = this(TimestampFormat, CoordinateReferenceSystem,
      ServerDeadReckoning, Option(Left(WorldBounds)), ObjectBounds)

    def this(
      TimestampFormat: Option[Int],
      CoordinateReferenceSystem: Option[String],
      ServerDeadReckoning: Option[ServerDeadReckoning],
      WorldBounds: MultiPolygon[LngLatAlt],
      ObjectBounds: Option[ObjectBounds]
    ) = this(TimestampFormat, CoordinateReferenceSystem,
      ServerDeadReckoning, Option(Right(WorldBounds)), ObjectBounds)
  }

  object Configure {
    implicit val fmt = Json.format[Configure]
  }

  /**
   * server response to a client Configure request.
   * set of Booleans indicating success of the configurations.
   */
  case class ConfigureResponse(
    TimestampFormat: Option[Boolean] = None,
    CoordinateReferenceSystem: Option[Boolean] = None,
    ServerDeadReckoning: Option[Boolean] = None,
    WorldBounds: Option[Boolean] = None,
    ObjectBounds: Option[Boolean] = None
  ) extends WeblvcMsg("ConfigureResponse")

  object ConfigureResponse {
    implicit val fmt = Json.format[ConfigureResponse]
  }

  //------------------------------------------------------------------------------------
  //-------------------weblvc messages--------------------------------------------------
  //------------------------------------------------------------------------------------

  /**
   * carry the values of the attributes of a WebLVC object.
   *
   * @param ObjectName uniquely identifies the WebLVC object
   * @param ObjectType indicates the Object Type of the WebLVC Object for which the message is conveying an update
   * @param Timestamp  the timestamp of the update
   * @param attributes other possible attributes as a map of (key,value)
   */
  case class AttributeUpdate(ObjectName: String, ObjectType: String,
      Timestamp: Option[Either[String, Double]] = None,
      attributes: Option[AttributesMap] = None) extends WeblvcMsg("AttributeUpdate") {

    def this(ObjectName: String, ObjectType: String, Timestamp: String) = this(ObjectName, ObjectType, Option(Left(Timestamp)))

    def this(ObjectName: String, ObjectType: String, Timestamp: Double) = this(ObjectName, ObjectType, Option(Right(Timestamp)))

  }

  object AttributeUpdate {

    // "MessageKind" plus the list of field names but not "attributes"
    private val omitList = List("MessageKind") ++
      (for (f <- AttributeUpdate("", "").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[AttributeUpdate] {
      def reads(js: JsValue): JsResult[AttributeUpdate] = {
        JsSuccess(new AttributeUpdate(
          (js \ "ObjectType").as[String],
          (js \ "ObjectName").as[String],
          (js \ "Timestamp").asOpt[Either[String, Double]],
          AttributesMap.readAttributes(js, omitList)
        ))
      }
    }

    val theWrites: Writes[AttributeUpdate] =
      ((JsPath \ "ObjectType").write[String] and
        (JsPath \ "ObjectName").write[String] and
        (JsPath \ "Timestamp").writeNullable[Either[String, Double]] and
        JsPath.writeNullable[AttributesMap])(unlift(AttributeUpdate.unapply))

    implicit val fmt: Format[AttributeUpdate] = Format(theReads, theWrites)
  }

  /**
   * carry the values of the attributes of a PhysicalEntity
   */
  case class PhysicalEntity(ObjectType: String = phyEntity, ObjectName: String,
    Timestamp: Option[Either[String, Double]] = None,
    EntityType: Option[Array[Int]] = None,
    EntityIdentifier: Option[Array[Int]] = None,
    Coordinates: Option[Coordinates] = None,
    ForceIdentifier: Option[Int] = None,
    Marking: Option[String] = None,
    attributes: Option[AttributesMap] = None) extends WeblvcMsg("AttributeUpdate")

  object PhysicalEntity {

    // "MessageKind" plus the list of field names but not "attributes"
    private val omitList = List("MessageKind") ++
      (for (f <- PhysicalEntity("", "").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[PhysicalEntity] {
      def reads(js: JsValue): JsResult[PhysicalEntity] = {
        JsSuccess(new PhysicalEntity(
          (js \ "ObjectType").as[String],
          (js \ "ObjectName").as[String],
          (js \ "Timestamp").asOpt[Either[String, Double]],
          (js \ "EntityType").asOpt[Array[Int]],
          (js \ "EntityIdentifier").asOpt[Array[Int]],
          (js \ "Coordinates").asOpt[Coordinates],
          (js \ "ForceIdentifier").asOpt[Int],
          (js \ "Marking").asOpt[String],
          AttributesMap.readAttributes(js, omitList)
        ))
      }
    }

    val theWrites: Writes[PhysicalEntity] =
      ((JsPath \ "ObjectType").write[String] and
        (JsPath \ "ObjectName").write[String] and
        (JsPath \ "Timestamp").writeNullable[Either[String, Double]] and
        (JsPath \ "EntityType").writeNullable[Array[Int]] and
        (JsPath \ "EntityIdentifier").writeNullable[Array[Int]] and
        (JsPath \ "Coordinates").writeNullable[Coordinates] and
        (JsPath \ "ForceIdentifier").writeNullable[Int] and
        (JsPath \ "Marking").writeNullable[String] and
        JsPath.writeNullable[AttributesMap])(unlift(PhysicalEntity.unapply))

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
  case class AggregateEntity(ObjectType: String = aggEntity, ObjectName: String,
    Timestamp: Option[Either[String, Double]] = None,
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
    attributes: Option[AttributesMap] = None) extends WeblvcMsg("AttributeUpdate")

  object AggregateEntity {

    // "MessageKind" plus the list of field names but not "attributes"
    private val omitList = List("MessageKind") ++
      (for (f <- AggregateEntity("", "").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[AggregateEntity] {
      def reads(js: JsValue): JsResult[AggregateEntity] = {
        JsSuccess(new AggregateEntity(
          (js \ "ObjectType").as[String],
          (js \ "ObjectName").as[String],
          (js \ "Timestamp").asOpt[Either[String, Double]],
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
          AttributesMap.readAttributes(js, omitList)
        ))
      }
    }

    val theWrites: Writes[AggregateEntity] =
      ((JsPath \ "ObjectType").write[String] and
        (JsPath \ "ObjectName").write[String] and
        (JsPath \ "Timestamp").writeNullable[Either[String, Double]] and
        (JsPath \ "EntityType").writeNullable[Array[Int]] and
        (JsPath \ "EntityIdentifier").writeNullable[Array[Int]] and
        (JsPath \ "Coordinates").writeNullable[Coordinates] and
        (JsPath \ "ForceIdentifier").writeNullable[Int] and
        (JsPath \ "Marking").writeNullable[String] and
        (JsPath \ "AggregateState").writeNullable[Int] and
        (JsPath \ "Formation").writeNullable[Int] and
        (JsPath \ "Dimensions").writeNullable[Array[Double]] and
        (JsPath \ "Subordinates").writeNullable[Array[String]] and
        (JsPath \ "AggregateSubordinates").writeNullable[Array[String]] and
        (JsPath \ "SilentEntities").writeNullable[Array[SilentType]] and
        (JsPath \ "SilentAggregates").writeNullable[Array[SilentType]] and
        (JsPath \ "SilentEntitiesDamageState").writeNullable[Array[SilentType]] and
        JsPath.writeNullable[AttributesMap])(unlift(AggregateEntity.unapply))

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
  case class EnvironmentalEntity(ObjectType: String = envEntity, ObjectName: String,
    Timestamp: Option[Either[String, Double]] = None,
    `Type`: Option[Array[Int]] = None,
    ProcessIdentifier: Option[Array[Int]] = None,
    ModelType: Option[Int] = None,
    EnvironmentProcessActive: Option[Boolean] = None,
    SequenceNumber: Option[Int] = None,
    GeometryRecords: Option[Array[GeometryRecord]] = None,
    attributes: Option[AttributesMap] = None) extends WeblvcMsg("AttributeUpdate")

  object EnvironmentalEntity {

    // "MessageKind" plus the list of field names but not "attributes"
    private val omitList = List("MessageKind") ++
      (for (f <- EnvironmentalEntity("", "").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[EnvironmentalEntity] {
      def reads(js: JsValue): JsResult[EnvironmentalEntity] = {
        JsSuccess(new EnvironmentalEntity(
          (js \ "ObjectType").as[String],
          (js \ "ObjectName").as[String],
          (js \ "Timestamp").asOpt[Either[String, Double]],
          (js \ "Type").asOpt[Array[Int]],
          (js \ "ProcessIdentifier").asOpt[Array[Int]],
          (js \ "ModelType").asOpt[Int],
          (js \ "EnvironmentProcessActive").asOpt[Boolean],
          (js \ "SequenceNumber").asOpt[Int],
          (js \ "GeometryRecords").asOpt[Array[GeometryRecord]],
          AttributesMap.readAttributes(js, omitList)
        ))
      }
    }

    val theWrites: Writes[EnvironmentalEntity] =
      ((JsPath \ "ObjectType").write[String] and
        (JsPath \ "ObjectName").write[String] and
        (JsPath \ "Timestamp").writeNullable[Either[String, Double]] and
        (JsPath \ "Type").writeNullable[Array[Int]] and
        (JsPath \ "ProcessIdentifier").writeNullable[Array[Int]] and
        (JsPath \ "ModelType").writeNullable[Int] and
        (JsPath \ "EnvironmentProcessActive").writeNullable[Boolean] and
        (JsPath \ "SequenceNumber").writeNullable[Int] and
        (JsPath \ "GeometryRecords").writeNullable[Array[GeometryRecord]] and
        JsPath.writeNullable[AttributesMap])(unlift(EnvironmentalEntity.unapply))

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

  /**
   * carry the values of the attributes of a RadioTransmitter
   */
  case class RadioTransmitter(ObjectType: String, ObjectName: String,
    Timestamp: Option[Either[String, Double]] = None,
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
    attributes: Option[AttributesMap] = None) extends WeblvcMsg("AttributeUpdate")

  object RadioTransmitter {

    // "MessageKind" plus the list of field names but not "attributes"
    private val omitList = List("MessageKind") ++
      (for (f <- RadioTransmitter("", "").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[RadioTransmitter] {
      def reads(js: JsValue): JsResult[RadioTransmitter] = {
        JsSuccess(new RadioTransmitter(
          (js \ "ObjectType").as[String],
          (js \ "ObjectName").as[String],
          (js \ "Timestamp").asOpt[Either[String, Double]],
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
          AttributesMap.readAttributes(js, omitList)
        ))
      }
    }

    val theWrites = new Writes[RadioTransmitter] {
      def writes(radio: RadioTransmitter) = {
        val theList = ListBuffer[Option[(String, JsValue)]](
          Option("ObjectType" -> JsString(radio.ObjectType)),
          Option("ObjectName" -> JsString(radio.ObjectName)),
          radio.Timestamp.map("Timestamp" -> Json.toJson(_)),
          radio.EntityIdentifier.map("EntityIdentifier" -> Json.toJson(_)),
          radio.HostObjectName.map("HostObjectName" -> JsString(_)),
          radio.RadioIndex.map("RadioIndex" -> JsNumber(_)),
          radio.RadioEntityType.map("RadioEntityType" -> Json.toJson(_)),
          radio.TransmitState.map("TransmitState" -> JsNumber(_)),
          radio.InputSource.map("InputSource" -> Json.toJson(_)),
          radio.WorldAntennaLocation.map("WorldAntennaLocation" -> Json.toJson(_)),
          radio.RelativeAntennaLocation.map("RelativeAntennaLocation" -> Json.toJson(_)),
          radio.AntennaPatternType.map("AntennaPatternType" -> JsNumber(_)),
          radio.Frequency.map("Frequency" -> JsNumber(_)),
          radio.TransmitBandwidth.map("TransmitBandwidth" -> JsNumber(_)),
          radio.Power.map("Power" -> JsNumber(_)),
          radio.ModulationType.map("ModulationType" -> Json.toJson(_)),
          radio.CryptoMode.map("CryptoMode" -> JsNumber(_)),
          radio.CryptoSystem.map("CryptoSystem" -> JsNumber(_)),
          radio.CryptoKey.map("CryptoKey" -> JsNumber(_)),
          radio.AntennaPatternParameters.map("AntennaPatternParameters" -> Json.toJson(_)),
          radio.FrequencyHopInUse.map("FrequencyHopInUse" -> JsBoolean(_)),
          radio.PseudoNoiseInUse.map("PseudoNoiseInUse" -> JsBoolean(_)),
          radio.TimeHopInUse.map("TimeHopInUse" -> JsBoolean(_))
        )
        radio.attributes match {
          case Some(att) => JsObject(theList.flatten) ++ Json.toJson(att).asInstanceOf[JsObject]
          case None => JsObject(theList.flatten)
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
  case class ObjectDeleted(ObjectName: String, Timestamp: Option[Either[String, Double]] = None) extends WeblvcMsg("ObjectDeleted") {

    def this(ObjectName: String, Timestamp: String) = this(ObjectName, Option(Left(Timestamp)))

    def this(ObjectName: String, Timestamp: Double) = this(ObjectName, Option(Right(Timestamp)))

  }

  object ObjectDeleted {
    implicit val fmt = Json.format[ObjectDeleted]
  }

  //------------------------------------------------------------------------------------
  //-------------------interaction messages--------------------------------------------------
  //------------------------------------------------------------------------------------

  /**
   * notification of simulation events interactions
   *
   * @param InteractionType indicates the type of the Interaction conveyed by a WebLVC message, similar to HLA InteractionClass or DIS PDU Kind
   * @param Timestamp       the time at which the data is valid.
   * @param attributes      dynamic set of attributes
   */
  case class Interaction(
    InteractionType: String,
    Timestamp: Option[Either[String, Double]] = None,
    attributes: Option[AttributesMap] = None
  ) extends WeblvcMsg("Interaction")

  object Interaction {

    // "MessageKind" plus the list of field names but not "attributes"
    private val omitList = List("MessageKind") ++
      (for (f <- Interaction("").getClass.getDeclaredFields) yield f.getName).toList.filterNot(_ == "attributes")

    val theReads = new Reads[Interaction] {
      def reads(js: JsValue): JsResult[Interaction] = {
        JsSuccess(new Interaction(
          (js \ "InteractionType").as[String],
          (js \ "Timestamp").asOpt[Either[String, Double]],
          AttributesMap.readAttributes(js, omitList)
        ))
      }
    }

    val theWrites: Writes[Interaction] =
      ((JsPath \ "InteractionType").write[String] and
        (JsPath \ "Timestamp").writeNullable[Either[String, Double]] and
        JsPath.writeNullable[AttributesMap])(unlift(Interaction.unapply))

    implicit val fmt: Format[Interaction] = Format(theReads, theWrites)
  }

  /**
   * WeaponFire interaction message
   */
  case class WeaponFire(
    InteractionType: String = weaponFire,
    Timestamp: Option[Either[String, Double]] = None,
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
    Coordinates: Option[Coordinates] = None
  ) extends WeblvcMsg("Interaction")

  object WeaponFire {
    implicit val fmt = Json.format[WeaponFire]
  }

  /**
   * Munition Detonation interaction message
   */
  case class MunitionDetonation(
    InteractionType: String = munitionDetonation,
    Timestamp: Option[Either[String, Double]] = None,
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
    Coordinates: Option[Coordinates] = None
  ) extends WeblvcMsg("Interaction")

  object MunitionDetonation {
    implicit val fmt = Json.format[MunitionDetonation]
  }

  /**
   * Start Resume interaction message. directs one or more simulators to start or resume simulating,
   * or start or resume simulation of a specific entity.
   */
  case class StartResume(
    InteractionType: String = startResume,
    Timestamp: Option[Either[String, Double]] = None,
    ReceivingEntity: Array[Int],
    RequestIdentifier: Int,
    RealWorldTime: Double,
    SimulationTime: Double,
    OriginatingEntity: Option[Array[Int]] = None
  ) extends WeblvcMsg("Interaction")

  object StartResume {
    implicit val fmt = Json.format[StartResume]
  }

  /**
   * directs one or more simulators to stop simulating, or stop simulation of a specific entity.
   */
  case class StopFreeze(
    InteractionType: String = stopFreeze,
    Timestamp: Option[Either[String, Double]] = None,
    ReceivingEntity: Array[Int],
    OriginatingEntity: Option[Array[Int]] = None,
    RealWorldTime: Option[Double] = None,
    Reason: Option[Int] = None,
    ReflectValues: Option[Boolean] = None,
    RunInternalSimulationClock: Option[Boolean] = None,
    UpdateAttributes: Option[Boolean] = None
  ) extends WeblvcMsg("Interaction")

  object StopFreeze {
    implicit val fmt = Json.format[StopFreeze]
  }

  /**
   * represents the wireless transmission and reception of audio or digital data via electromagnetic waves.
   */
  case class RadioSignal(
    InteractionType: String = radioSignal,
    Timestamp: Option[Either[String, Double]] = None,
    RadioIdentifier: Option[String] = None,
    EncodingClass: Option[Int] = None,
    EncodingType: Option[Int] = None,
    TDLType: Option[Int] = None,
    SampleRate: Option[Int] = None,
    SampleCount: Option[Int] = None,
    SampleData: Option[String] = None,
    DatabaseIndex: Option[Int] = None,
    UserProtocolID: Option[Int] = None
  ) extends WeblvcMsg("Interaction")

  object RadioSignal {
    implicit val fmt = Json.format[RadioSignal]
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
  case class SubscribeObject(ObjectType: String, Filters: Option[AttributesMap] = None) extends WeblvcMsg("SubscribeObject")

  object SubscribeObject {

    val theReads: Reads[SubscribeObject] =
      ((JsPath \ "ObjectType").read[String] and
        JsPath.readNullable[AttributesMap])((ob, fil) => SubscribeObject(ob, fil))

    val theWrites: Writes[SubscribeObject] =
      ((JsPath \ "ObjectType").write[String] and
        JsPath.writeNullable[AttributesMap])(unlift(SubscribeObject.unapply))

    implicit val fmt: Format[SubscribeObject] = Format(theReads, theWrites)
  }

  /**
   * remove object subscriptions request.
   *
   * @param ObjectType type of object to which to unsubscribe
   */
  case class UnsubscribeObject(ObjectType: String) extends WeblvcMsg("UnsubscribeObject")

  object UnsubscribeObject {
    implicit val fmt = Json.format[UnsubscribeObject]
  }

  /**
   * a client request specifying which WebLVC Interaction messages are sent to it by the server
   *
   * @param InteractionType
   * @param Timestamp
   * @param Filters
   */
  case class SubscribeInteraction(
    InteractionType: String,
    Timestamp: Option[Either[String, Double]] = None,
    Filters: Option[AttributesMap] = None
  ) extends WeblvcMsg("SubscribeInteraction")

  object SubscribeInteraction {

    val theReads: Reads[SubscribeInteraction] =
      ((JsPath \ "InteractionType").read[String] and
        (JsPath \ "Timestamp").readNullable[Either[String, Double]] and
        JsPath.readNullable[AttributesMap])((ob, at, fil) => SubscribeInteraction(ob, at, fil))

    val theWrites: Writes[SubscribeInteraction] =
      ((JsPath \ "InteractionType").write[String] and
        (JsPath \ "Timestamp").writeNullable[Either[String, Double]] and
        JsPath.writeNullable[AttributesMap])(unlift(SubscribeInteraction.unapply))

    implicit val fmt: Format[SubscribeInteraction] = Format(theReads, theWrites)
  }

  /**
   * remove the SubscribeInteraction
   *
   * @param InteractionType
   */
  case class UnsubscribeInteraction(InteractionType: String) extends WeblvcMsg("UnsubscribeInteraction")

  object UnsubscribeInteraction {
    implicit val fmt = Json.format[UnsubscribeInteraction]
  }

  /**
   * client request for the server status log for the client
   *
   * @param Length
   * @param Offset
   */
  case class StatusLogRequest(Length: Option[Int] = None, Offset: Option[Int] = None) extends WeblvcMsg("StatusLogRequest")

  object StatusLogRequest {
    implicit val fmt = Json.format[StatusLogRequest]
  }

  // todo the Array[StatusLog] should be a sorted list ordered by index number
  /**
   * the server response to the client StatusLogRequest
   *
   * @param Status
   */
  case class StatusLogResponse(Status: Array[StatusLog]) extends WeblvcMsg("StatusLogResponse")

  object StatusLogResponse {
    implicit val fmt = Json.format[StatusLogResponse]
  }

}