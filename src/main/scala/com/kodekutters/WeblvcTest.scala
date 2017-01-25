package com.kodekutters

import com.kodekutters.WeblvcImplicits._
import com.kodekutters.WebLvc._
import play.api.libs.json._
import play.extras.geojson.{Feature, LatLng, Point, Polygon}

import scala.collection.mutable
import scala.collection.immutable.Seq

/**
  * ad-hock
  */
object WeblvcTest {

  import com.kodekutters.WebLvc.WebLvcSupport._

  def main(args: Array[String]): Unit = {

    testConnect()
    testConfigure()
    testPoly()
    testAggr()
    testPhys()
    testEnv()
    testRadio()
    WeaponFire()
    testFilters()
  }

  def testConnect() = {

    val js = """{"MessageKind":"Connect","ClientName":"testconnect","WebLVCVersion":1.2,"Messages":[{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity","Marking":"TankA"}]}""".stripMargin

    import com.kodekutters.WebLvc.WeblvcMsg._

    val filterList = Map[String, String]("Marking" -> "TankA")
    val filters = new AttributesMap(filterList)
    println("\nfilters: " + filters)

    val f = Json.toJson[AttributesMap](filters)
    println("\nf: " + f)
    println("\nf back: " + Json.fromJson[AttributesMap](f).asOpt)

    val subObj = new SubscribeObject("WebLVC:PhysicalEntity", None)
    val subObjJs = Json.toJson[WeblvcMsg](subObj)
    println("\n\nsubObj: " + subObj)
    val backObj = Json.fromJson[WeblvcMsg](subObjJs).asOpt
    println("\nback subObj: " + backObj + "\n")

    val test1 = new Connect("testconnect", 1.2, Option(Array(subObj)))
    println("\ntest1: " + test1)
    val test1Js = Json.toJson[WeblvcMsg](test1)
    println("\ntest1Js: " + test1Js)

    val nameResult: JsResult[WeblvcMsg] = test1Js.validate[WeblvcMsg]
    nameResult match {
      case s: JsSuccess[WeblvcMsg] => println("\nvalidate: " + s.get)
      case e: JsError => println("\nErrors: " + JsError.toJson(e).toString())
    }

    val p = Json.parse(test1Js.toString())
    println("\ntest1 parse: " + p)
    println("\nconctp: " + Json.fromJson[WeblvcMsg](p).asOpt + "\n")

    val conct = Json.fromJson[WeblvcMsg](test1Js).asOpt
    println("\nconct: " + conct + "\n")

    val conct2 = Json.fromJson[WeblvcMsg](Json.parse(js)).asOpt
    println("\nconct2: " + conct2 + "\n")
  }

  def WeaponFire(): Unit = {
    val js =
      """{
                     "MessageKind" : "Interaction",
                    "InteractionType" : "WebLVC:WeaponFire",
                    "AttackerId" : "Tank1",
                    "TargetId" : "Tank2",
                    "MunitionType" : [2,2,225,2,3,0,0],
                    "Coordinates" : {
                       "WorldLocation" : [4437182.0232, -395338.0731, 873923.4663],
                       "VelocityVector" : [57.04, 32.77, 89.263]
                    }
                 }""".stripMargin

    val obj = Json.fromJson[WeblvcMsg](Json.parse(js)).asOpt
    println("obj " + obj + "\n")
    obj match {
      case None => println("no WeaponFire")
      case Some(wef) =>
        val jsp = Json.prettyPrint(Json.toJson[WeblvcMsg](wef))
        println("WeaponFire: " + Json.prettyPrint(Json.toJson[WeblvcMsg](wef)))
        println("\nback WeaponFire: " + Json.fromJson[WeblvcMsg](Json.parse(jsp)).asOpt)
    }
    println()
  }

  def testPhys() = {

    val jsPhys =
      """{
          "MessageKind" : "AttributeUpdate",
          "ObjectType" : "WebLVC:PhysicalEntity",
          "ObjectName" : "obj-name",
          "Timestamp" : "time-thing",
          "EntityType" : [ 1, 2, 1 ],
          "EntityIdentifier" : [ 1, 2, 225, 1, 3, 0, 0 ],
          "Coordinates" : {
            "WorldLocation" : [ 1, 2, 3 ],
            "VelocityVector" : [ 4, 5, 6 ],
            "Orientation" : [ 7, 8, 9 ]
          },
          "ForceIdentifier" : 7,
          "Marking" : "F-16",
          "EngineSmokeOn" : true,
          "IsConcealed" : false,
          "DamageState" : 1,
          "some_attributesx" : "strawberriesx"
          }""".stripMargin

    val prs = Json.parse(jsPhys)
    val phys1 = Json.fromJson[WeblvcMsg](prs).asOpt
    phys1 match {
      case None => println("no phys")
      case Some(phys) => println("phys: " + Json.prettyPrint(Json.toJson[WeblvcMsg](phys)))
    }
    println()
  }

  def testAggr() = {

    val js =
      """{
             "MessageKind" : "AttributeUpdate",
             "ObjectName" : "Platoon 1",
             "ObjectType" : "WebLVC:AggregateEntity",
             "Timestamp" : "time-thing",
             "EntityIdentifier" : [1,2,3],
             "EntityType" : [1,1,225,3,2,0,0],
              "Coordinates" : {
                    "WorldLocation" : [ 1, 2, 3 ],
                    "Orientation" : [ 7, 8, 9 ]
                  },
             "Marking" : "Platoon 1",
             "Dimensions" : [10.0, 20.0, 1.0],
             "Subordinates" : ["Tank1", "Tank2", "Tank3"],
             "Formation" : 3,
             "some_attributeszzz" : "strawberrieszzz"
          }
        """.stripMargin

    val obj = Json.fromJson[WeblvcMsg](Json.parse(js)).asOpt

    val filterList = Map[String, Any]("Marking" -> Array("TankA", "TankB"))
    val filters = new AttributesMap(filterList)

    obj match {
      case None => println("no AggregateEntity")
      case Some(aggr) => println("aggr: " + Json.prettyPrint(Json.toJson[WeblvcMsg](aggr)))
    }
    println()

    val aggr = new AggregateEntity(ObjectName = "frank", Timestamp = "atime")
    println("aggr2: " + Json.prettyPrint(Json.toJson[WeblvcMsg](aggr)))
    println()
  }

  def testRadio(): Unit = {
    val js =
      """{
                     "MessageKind" : "AttributeUpdate",
                     "ObjectType" : "WebLVC:RadioTransmitter",
                     "some_attributeszzz" : "strawberrieszzz",
                     "ObjectName" : "radio-banana",
                     "EntityIdentifier" : [1, 1, 3001],
                     "RadioIndex" : 1,
                     "RadioEntityType" : [225, 2, 3, 4],
                     "TransmitState" : 1,
                     "RadioEntityType" : [1, 1, 225, 3, 4, 5],
                     "InputSource" : 2,
                     "WorldAntennaLocation" : [4437182.0232, -395338.0731, 873923.4663],
                     "RelativeAntennaLocation" : [1.0, 0.0, -3.0],
                     "AntennaPatternType" : 1,
                     "Frequency" : 44056,
                     "Power" : 50.5,
                     "ModulationType" : {
                        "SpreadSpectrum" : 1,
                        "Major": 2,
                        "Detail" : 3,
                        "System" : 4
                    },
                     "CryptoMode" : 1,
                     "CryptoSystem" : 4,
                     "CryptoKey" : 2348238752,
                     "AntennaPatternParameters" : {
                        "BeamDirection" : [-1.65, 2.234, -0.771],
                        "AzimuthBeamwidth" : 0.25,
                        "ElevationBeamwidth" : 0.78,
                        "ReferenceSystem" : 2,
                        "Ez" : 2.533,
                        "Ex" : 1.29,
                        "Phase" : 0.707
                    },
                     "FrequencyHopInUse" : true,
                     "PseudoNoiseInUse" : false,
                     "TimeHopInUse" : true
                  }
        """.stripMargin

    val obj = Json.fromJson[WeblvcMsg](Json.parse(js)).asOpt
    println("\nobj fromJson: " + obj)

    obj match {
      case None => println("no radio")
      case Some(radio) => println("\nradio toJson: " + Json.prettyPrint(Json.toJson[WeblvcMsg](radio)))
    }
    println()
  }

  def test() = {

    val test1 = new Connect("testconnect", Some(1.2))
    val test1Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test1))
    println("test1: " + test1Js)

    //---------------------------------------------------------------------------

    val poly = new Polygon[LngLatAlt](Seq(Seq(new LngLatAlt(1.2, 2.3, 12.3), new LngLatAlt(6.7, 4.5, 45.6))))
    val recon = new ServerDeadReckoning(true, 5.0, 7.0)
    val test2 = new Configure(Some(1), Some("latlong"), Some(recon), Some(Left(poly)), Some(new ObjectBounds("xxxx", 123)))
    val test2Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test2))
    println("test2: " + test2Js)

    //---------------------------------------------------------------------------
    val coord1 = new CoordinatesGeod(Array[Double](1, 2, 3), Array[Double](4, 5, 6), Array[Double](7, 8, 9))
    val coord1Js = Json.prettyPrint(Json.toJson[Coordinates](coord1))
    println("coord1: " + coord1Js)

    val coord2 = new CoordinatesECEF(4, Array[Double](1, 2, 3), Array[Double](4, 5, 6),
      Array[Double](7, 8, 9), Array[Double](10, 20, 30), Array[Double](40, 50, 60))
    val coord2Js = Json.prettyPrint(Json.toJson[Coordinates](coord2))
    println("coord2: " + coord2Js)

    val test3 = new AttributeUpdate("some-attribute-name", "objType", "zzzzzz")
    val test3Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test3))
    println("test3: " + test3Js)

    val test4 = new ObjectDeleted("some-object-name", "xxxxxxxxx")
    val test4Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test4))
    println("test4: " + test4Js)

    val filterList = Map[String, Any]("Marking" -> Array("TankA", "TankB"))
    val filters = new AttributesMap(filterList)
    val test5 = new SubscribeObject("WebLVC:PhysicalEntity", Some(filters))
    val test5Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test5))
    println("test5: " + test5Js)

    val test6 = new StatusLogRequest(Some(2), Some(10))
    val test6Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test6))
    println("test6: " + test6Js)

  }

  def testPoly() = {

    val sydney = Feature(Point(LatLng(-33.86, 151.2111)), properties = Some(Json.obj("name" -> "Sydney")))
    println("sydney: " + sydney + "\njson: " + Json.prettyPrint(Json.toJson(sydney)))

    val polyx = new Polygon(Seq(Seq(new LngLatAlt(1.2, 2.3, 12.3), new LngLatAlt(6.7, 4.5, 45.6))))
    println("polyx: " + polyx + "\npolyxjs: " + Json.prettyPrint(Json.toJson(polyx)))

    //---------------------------------------------------------------------------

    val poly = new Polygon(Seq(Seq(new LngLatAlt(1.2, 2.3, 12.3), new LngLatAlt(6.7, 4.5, 45.6))))
    val recon = new ServerDeadReckoning(true, 5.0, 7.0)
    val test2 = new Configure(1, "latlong", recon, poly, new ObjectBounds("xxxx", 123))
    val test2Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test2))
    println("test2: " + test2Js)

    //---------------------------------------------------------------------------
  }

  def testConfigure() = {
    val poly = new Polygon[LngLatAlt](Seq(Seq(new LngLatAlt(1.2, 2.3, 12.3), new LngLatAlt(6.7, 4.5, 45.6))))
    val recon = new ServerDeadReckoning(true, 5.0, 7.0)
    val test2 = new Configure(1, defaultECEFCartesian, recon, poly, new ObjectBounds("xxxx", 123))
    val test2Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test2))
    println("test2: " + test2Js)

  }

  def testFilters() = {
    val dim = Array(1, 2, 3)
    val filterList = Map[String, Any]("Marking" -> Array("TankA", "TankB"), "Dimension" -> dim)
    val filters = new AttributesMap(filterList)

    val test3 = new AttributeUpdate("some-attribute-name", "objType", "zzzzzz", filters)
    val test3Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test3))
    println("test3: " + test3Js)
    println()

    val test5 = new SubscribeObject("WebLVC:PhysicalEntity", Some(filters))
    val test5Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test5))
    println("test5: " + test5Js)

  }


  def testEnv() = {

    val js =
      """{
          "MessageKind" : "AttributeUpdate",
          "ObjectType" : "WebLVC:EnvironmentalEntity",
          "ObjectName" : "obj-name",
          "Timestamp" : "time-thing",
          "ProcessIdentifier" : [1, 2, 3],
          "Type" : [1, 4, 0, 0, 0, 0, 0],
          "ModelType" : 1,
          "EnvironmentProcessActive" : true,
          "some_attributesqqq" : "strawberriesqqq",
          "SequenceNumber" : 5,
                  "GeometryRecords" : [
                  		{
                  		"Type" : "LineString",
                  		"coordinates" : [
                  			[4437182.0232, -395338.0731, 873923.4663],
                        [4437124.1523, -395341.2841, 873922.5517]
                  		]
                    }
                  	]
          }
        """.stripMargin

    val prs = Json.parse(js)

    val env = Json.fromJson[WeblvcMsg](prs).asOpt
    println("\nenv fromJson: " + env + "\n")

    env match {
      case None => println("\nno env")
      case Some(phys) =>
        val q = Json.toJson[WeblvcMsg](phys)
        println("\nenv tojson: " + Json.prettyPrint(q))

        val nameResult: JsResult[WeblvcMsg] = q.validate[WeblvcMsg]
        nameResult match {
          case s: JsSuccess[WeblvcMsg] => println("\nvalidate: " + s.get)
          case e: JsError => println("\nErrors: " + JsError.toJson(e).toString())
        }
    }
  }

}
