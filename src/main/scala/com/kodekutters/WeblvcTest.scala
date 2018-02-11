package com.kodekutters

import java.time.LocalDateTime

import com.kodekutters.FilterSupport._
import com.kodekutters.FilterSupport.FilterType._
import com.kodekutters.WeblvcImplicits._
import com.kodekutters.WebLvcSupport._
import play.api.libs.json._
import au.id.jazzy.play.geojson._

import scala.collection.mutable
import scala.collection.immutable.Seq
import collection.JavaConverters._
import play.api.libs.json.Reads._

import scala.collection.mutable.ArrayBuffer


/**
  * ad-hock
  */
object WeblvcTest {

  def main(args: Array[String]): Unit = {

//        testConnect()
//        testConfigure()
//        testPoly()
//        testAggr()
//        testPhys()
//        testEnv()
//        testRadio()
//        WeaponFire()
//        testConnectMinMax()
//
//    testFilter()

    // testTime()

        testThis()
  }

  import com.kodekutters.{AttributeUpdateMsg, CoordinatesGeod, PhysicalEntity}

  def testThis() = {

    val jsPhys =
      """{
          "MessageKind" : "AttributeUpdate",
          "ObjectType" : "WebLVC:PhysicalEntity",
          "ObjectName" : "obj-name",
          "Timestamp" : 123456,
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
          "Book" : { "author": "Rowling", "title": "Harry Potter", "volumes": [1,2,3], "Dimensions" : [ {"min" : [1,2,3], "max" : [10,20,30]} ]  }
          }""".stripMargin

    try {
      val prs = Json.parse(jsPhys)
      val phys1 = Json.fromJson[WeblvcMsg](prs).asOpt
      println("phys1: " + phys1 + "\n")
      phys1 match {
        case None => println("no phys")
        case Some(phys) => println("phys: " + Json.prettyPrint(Json.toJson[WeblvcMsg](phys)))
      }
      println()
    }
    catch {
        case _: Throwable => println("====> got some other kind of exception")
      }

  }

  def testTime() = {

    val jsPhys =
      """{
          "MessageKind" : "AttributeUpdate",
          "ObjectType" : "WebLVC:PhysicalEntity",
          "ObjectName" : "obj-name",
          "Timestamp" : 123456,
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
    println("phys1: " + phys1)
    phys1 match {
      case None => println("no phys")
      case Some(phys) => println("phys: " + Json.prettyPrint(Json.toJson[WeblvcMsg](phys)))
    }
    println()


    def DoubleTohex(v: Double): String = {
      f"${v.toInt}%X"
    }

    val d = 12345.6
    val h = DoubleTohex(d)
    println(" double: " + d + " hex: " + h + " back d: " + Integer.parseInt(h, 16))


    val lx = 123456789L
    val hlx = f"$lx%X"
    println(" long: " + lx + " hex: " + hlx + " back lx: " + Integer.parseInt(hlx, 16))


    val gTime = LocalDateTime.now().toString
    println(" gTime: " + gTime + " " + LocalDateTime.now())

    val statusList = new ArrayBuffer[String]()
    val ndxx = statusList.size
    println("ndxx: " + ndxx)

    for (i <- 0 to 5) {
      statusList += "zz" + statusList.size
      //  val ndx = statusList.size
      //  println("ndx: "+ ndx)
    }

    statusList.foreach(println(_))

  }


  def doTest() = {

    import com.kodekutters.WeblvcImplicits._

    val theMsg = new PhysicalEntity(ObjectName = "phyzi",
      Timestamp = "today",
      Marking = "TankA",
      Coordinates = new CoordinatesGeod(Array(10.2, 20.2)))

    println("before - " + theMsg)

    val theField = theMsg.getClass.getDeclaredField("Coordinates")
    theField.setAccessible(true)
    theField.get(theMsg) match {
      case None => // no Coordinates field in this message
      // Coordinates: Option[Coordinates] = None,
      case Some(fieldVal) =>
        println("\n--------> fieldVal: " + fieldVal.asInstanceOf[Coordinates] + " <-------\n")
        val trans = new CoordinatesGeod(Array(50.2, 60.2))
        println("\n--------> trans: " + trans + " <-------\n")
        theField.set(theMsg, Option(trans))
    }


    //      val theField = theMsg.getClass.getDeclaredField("Timestamp")
    //      theField.setAccessible(true)
    //      theField.get(theMsg) match {
    //        case None => // no Timestamp field in this message
    //        case Some(x) =>
    //          x match {
    //            case Left(fieldVal) =>
    //              println("before fieldVal: " + fieldVal)
    //              theField.set(theMsg, Option(Left("tomorrow")))
    //
    //            case Right(fieldVal) => theField.set(theMsg, Option(Right(123.456)))
    //            case z => println("before z: " + z)
    //          }
    //      }

    println("after - " + theMsg)
  }

  def testFilter() = {
    val js1 =
      """{
        "MessageKind" : "SubscribeObject",
        "ObjectType" : "WebLVC:PhysicalEntity",
        "and": [
         	{
         	 "all": { "Marking" : [ "TankABC" ] }
           },
        	{
        	"any": { "Dimensions" : [ {"min" : [0,0,0], "max" : [10,10,10]} ] }
          }
        ]
        }""".stripMargin

    val js3 =
      """{
                "MessageKind" : "SubscribeObject",
                "ObjectType" : "WebLVC:PhysicalEntity",
                 "not": {"all": {
                "Marking" : [ "TankABC" ]
                }}}
                """.stripMargin

    val js2 =
      """{
        "MessageKind" : "SubscribeObject",
        "ObjectType" : "WebLVC:PhysicalEntity",
        "or": [
         	{
         	 "all": { "Marking" : [ "TankABC" ] }
           },
        	{
        	"all": { "Dimensions" : [ {"min" : [0,0,0], "max" : [10,10,10]} ] }
          }
        ]
        }""".stripMargin

    val js4 =
      """{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity",
        "all" : {
           "Coordinates" : [ {"all": { "DeadReckoningAlgorithm": [ 2, 4 ] } } ]
         	} }""".stripMargin

    val js5 =
      """{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity",
         "all": { "Marking" : [ {"min" : "TankA", "max" : "TankZ"} ] }
         }""".stripMargin

    val js6 =
      """{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity",
         "all": {
  	"Marking" : [
       	             {"min" : "TankA", "max" : "TankZ"},
             	       {"min" : "PlaneA", "max" : "PlaneD"}
  	            ] }
         }""".stripMargin

    // this does not work <-----------------
    val js7 =
      """{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity",
         "all": {
  	"Marking" : [
       	             "TankA",
             	       {"min" : "PlaneA", "max" : "PlaneD"}
  	            ] }
         }""".stripMargin

    val js8 =
      """{
        "MessageKind" : "SubscribeObject",
        "ObjectType" : "WebLVC:PhysicalEntity",
        "or": [
         	{
         	 "all": { "Marking" : [ "TankABC" ] }
           },
        	{
        	"all": { "Dimensions" : [ {"min" : [0,0,0], "max" : [10,10,10]} ] }
          }
        ]
        }""".stripMargin

    def doit(js: String, name: String) = {
      val prs = Json.parse(js)
      val obj = Json.fromJson[WeblvcMsg](prs).asOpt
      println(name + " fromJson: " + obj + "\n")
      println(name + " toJson: " + Json.toJson(obj) + "\n")
    }

    doit(js1, "js1")
    doit(js2, "js2")
    doit(js3, "js3")
    doit(js4, "js4")
    doit(js5, "js5")
    doit(js6, "js6")
    doit(js7, "js7")  // this does not work <-----------------
    doit(js8, "js8")

  }

  def testConnectMinMax() = {

    val js7 =
      """{"MessageKind":"Connect","ClientName":"testconnect","WebLVCVersion":1.2,
        "Messages":[{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity",
        "all" : {
	                "Dimensions" : [ {"min" : [0,0,0], "max" : [10,10,10]} ]
                }
        }]}""".stripMargin

    val js8 =
      """{"MessageKind":"Connect","ClientName":"testconnect","WebLVCVersion":1.2,
        "Messages":[{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity",
        "all": { "marking": ["TankA","TankB","TankC"] } }]}""".stripMargin

    val js4 =
      """{"MessageKind":"Connect","ClientName":"testconnect","WebLVCVersion":1.2,
        "Messages":[{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity",
        "all": { "marking": [{"min": 1, "max": 2}] } }]}""".stripMargin

    val js2 =
      """{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity",
        "all": { "marking": "TankA" } }""".stripMargin

    val js =
      """{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity",
        "all" : {
           "Coordinates" : [ {"all": { "DeadReckoningAlgorithm": [ 2, 4 ] } } ]
         	} }""".stripMargin

    val prs = Json.parse(js)

    val obj = Json.fromJson[WeblvcMsg](prs).asOpt
    println("\nobj fromJson: " + obj)
    println("\nobj toJson: " + Json.toJson(obj))
  }

  def testMinMax() = {

    //    val js1 = """{"min" : 0, "max" : 5}""".stripMargin
    //    val js2 = """{"min" : "a", "max" : "d"}""".stripMargin
    //    val js3 = """{"min" : 1.6, "max" : 2.4}""".stripMargin
    //    val js4 = """{"min" : [0,0,0], "max" : [10,10,10]}""".stripMargin
    //    val js5 = """{"min" : [0.1,0.2,0.3], "max" : [10.1,10.2,10.3]}""".stripMargin
    //
    //    val r1 = new MinMaxRange[Int](6, 8)
    //    println("r1: " + r1 + " tojson: "+ Json.toJson[MinMaxRange[Int]](r1) +" fromJson: " + Json.fromJson[MinMaxRange[Int]](Json.parse(js1)))
    //
    //    val r2 = new MinMaxRange[String]("rock", "hard")
    //    println("r2: " + r2 + " tojson: "+ Json.toJson[MinMaxRange[String]](r2) +" fromJson: " + Json.fromJson[MinMaxRange[String]](Json.parse(js2)))
    //
    //    val r3 = new MinMaxRange[Double](1.2, 3.4)
    //    println("r3: " + r3 + " tojson: "+ Json.toJson[MinMaxRange[Double]](r3) +" fromJson: " + Json.fromJson[MinMaxRange[Double]](Json.parse(js3)))
    //
    //    val r4 = new MinMaxRange[Array[Int]](Array(0,1,2), Array(3,4,5))
    //    println("r4: " + r4 + " tojson: "+ Json.toJson[MinMaxRange[Array[Int]]](r4) +" fromJson: " + Json.fromJson[MinMaxRange[Array[Int]]](Json.parse(js4)))

  }

  def testConnect() = {

    val js = """{"MessageKind":"Connect","ClientName":"testconnect","WebLVCVersion":1.2,"Messages":[{"MessageKind":"SubscribeObject","ObjectType":"WebLVC:PhysicalEntity","Marking":"TankA"}]}""".stripMargin

    val filterList = Map[String, JsValue]("Marking" -> JsString("TankA"))
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

    val filterList = Map[String, JsValue]("Marking" -> JsArray(Seq(JsString("TankA"), JsString("TankB"))))
    val filters = new AttributesMap(filterList)

    obj match {
      case None => println("no AggregateEntity")
      case Some(aggr) =>
        println("aggr: " + Json.toJson[WeblvcMsg](aggr))
        println("aggr pretty: " + Json.prettyPrint(Json.toJson[WeblvcMsg](aggr)))
    }
    println()

    val aggr = new AggregateEntity(ObjectName = "frank", Timestamp = "atime")
    println("aggr2: " + Json.toJson[WeblvcMsg](aggr))
    println("aggr2 pretty: " + Json.prettyPrint(Json.toJson[WeblvcMsg](aggr)))
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

    val obj = Json.fromJson[AttributeUpdateMsg](Json.parse(js)).asOpt
    println("\nobj fromJson: " + obj)

    obj match {
      case None => println("no radio")
      case Some(radio) => println("\nradio toJson: " + Json.prettyPrint(Json.toJson[AttributeUpdateMsg](radio)))
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

    //    val attrib = ("Marking", Array("TankA", "TankB"))
    //    val filter = new Filter("all", attrib)
    //    val test5 = new SubscribeObject("WebLVC:PhysicalEntity", Some(filter))
    //    val test5Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test5))
    //    println("test5: " + test5Js)

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
    val test2 = new Configure(1, "latlong", recon, poly, new ObjectBounds("xxxx", 123), None)
    val test2Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test2))
    println("test2: " + test2Js)

    //---------------------------------------------------------------------------
  }

  def testConfigure() = {
    val poly = new Polygon[LngLatAlt](Seq(Seq(new LngLatAlt(1.2, 2.3, 12.3), new LngLatAlt(6.7, 4.5, 45.6))))
    val recon = new ServerDeadReckoning(true, 5.0, 7.0)
    val test2 = new Configure(1, defaultECEFCartesian, recon, poly, new ObjectBounds("xxxx", 123), None)
    val test2Js = Json.prettyPrint(Json.toJson[WeblvcMsg](test2))
    println("test2: " + test2Js)

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
      case Some(e) =>
        println("\nenv tojson1: " + Json.prettyPrint(Json.toJson[WeblvcMsg](e)))
      //    println("\nenv tojson2: " + Json.prettyPrint(Json.toJson[AttributeUpdateMsg](e.asInstanceOf[AttributeUpdateMsg])))

      //        val nameResult: JsResult[WeblvcMsg] = q.validate[WeblvcMsg]
      //        nameResult match {
      //          case s: JsSuccess[WeblvcMsg] => println("\nvalidate: " + s.get)
      //          case e: JsError => println("\nErrors: " + JsError.toJson(e).toString())
      //        }
    }
  }

}
