package com.kodekutters

import play.api.libs.json._
import au.id.jazzy.play.geojson._

import scala.collection.immutable.Seq

/**
 * A longitude, latitude and altitude CRS for use with WGS84 ( == EPSG:4326).
 * Extension of the original code play.extras.geojson.LatLng see https://github.com/jroper/play-geojson
 *
 * @param lng The longitude in decimal degrees.
 * @param lat The latitude in decimal degrees.
 * @param alt The altitude in meters, default relative to the WGS84 ellipsoid,
 *            but can be interpreted according to the AltitudeMode of a Feature
 *
 * NOTE:
 * In GeoJSON the coordinate order is: longitude, latitude (X, Y) within coordinate arrays.
 */
case class LngLatAlt(lng: Double, lat: Double, alt: Option[Double] = None) {
  def this(lng: Double, lat: Double, alt: Double) = this(lng, lat, Option(alt))
}

object LngLatAlt {
  implicit val lngLatAltFormat: Format[LngLatAlt] = Wgs84Format.format
  implicit val lngLatAltCrs: CrsFormat[LngLatAlt] = Wgs84Format
}

/**
 * The WGS84 CRS format. Equals to EPSG:4326 CRS format.
 */
object Wgs84Format extends CrsFormat[LngLatAlt] {
  val crs = NamedCrs("urn:ogc:def:crs:OGC:1.3:CRS84")
  val format = Format[LngLatAlt](
    __.read[Seq[Double]].map {
      case Seq(lng, lat, alt) => LngLatAlt(lng, lat, Some(alt))
      case Seq(lng, lat) => LngLatAlt(lng, lat)
    },
    Writes(lla => {
      lla.alt match {
        case None => Json.arr(lla.lng, lla.lat)
        case Some(alt) => Json.arr(lla.lng, lla.lat, alt)
      }
    })
  )

  override def isDefault = true
}
