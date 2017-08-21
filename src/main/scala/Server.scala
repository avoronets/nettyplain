package com.example

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.util
import java.util.zip.{ZipEntry, ZipFile}

import dao.entity.{Location, User, Visit}
import dao.serice.StorageServiceImpl
import spray.json._

import scala.collection.JavaConversions._
/** embedded server */

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val userFormat = jsonFormat6(User)
  implicit val visitFormat = jsonFormat5(Visit)
  implicit val locationFormat = jsonFormat5(Location)
  implicit val locationsFormat = jsonFormat1(LocationsJson)
  implicit val visitsFormat = jsonFormat1(VisitsJson)
  implicit val usersFormat = jsonFormat1(UsersJson)
  implicit val visitJsonFormat = jsonFormat3(VisitJson)
  implicit val visitJsonWrapperFormat = jsonFormat1(VisitJsonWrapper)
  implicit val avgJsonFormat = jsonFormat1(AvgJson)
  implicit val visitJsonForUpdate = jsonFormat5(VisitJsonForUpdate)
  implicit val locationJsonForUpdate = jsonFormat5(LocationJsonForUpdate)
  implicit val userJsonForUpdate = jsonFormat6(UserJsonForUpdate)

}
object Server {

  import MyJsonProtocol._

  def convertZipEntry(entry: ZipEntry, zipFile: ZipFile): EntitiesJson = {

    def toJsValue(inputStream: InputStream): JsValue = {
      val bReader = new BufferedReader(new InputStreamReader(inputStream))
      val result  = bReader.lines().iterator().toStream.map(f => f.parseJson).head
      result
    }
    val inputStream = zipFile.getInputStream(entry)

    val entities = entry.getName match
    {
      case str: String if str.startsWith("users") =>
        toJsValue(inputStream).convertTo[UsersJson]
      case str: String if str.startsWith("locations") =>
        toJsValue(inputStream).convertTo[LocationsJson]
      case str: String if str.startsWith("visits") =>
        toJsValue(inputStream).convertTo[VisitsJson]
    }
    inputStream.close()
    entities
  }

  def prepareData(): List[EntitiesJson] = {
    val zipFile = new ZipFile("/tmp/data/data.zip")
    val entries: util.Enumeration[_ <: ZipEntry] = zipFile.entries().toIterator
    val result: List[Option[EntitiesJson]] = entries.map
    {entry =>
      val inputStream = zipFile.getInputStream(entry)

      val bReader = new BufferedReader(new InputStreamReader(inputStream))
      val result  = bReader.lines().iterator().toStream.map(f => f.parseJson).head
      val entities = entry.getName match
      {
        case str: String if str.startsWith("users") =>
          val usersJson = result.convertTo[UsersJson]
          usersJson.users.foreach(StorageServiceImpl.insert)
          Some(usersJson)
        case str: String if str.startsWith("locations") =>
          val locationsJson = result.convertTo[LocationsJson]
          locationsJson.locations.foreach(StorageServiceImpl.insert)
          Some(locationsJson)
        case str: String if str.startsWith("visits") =>
          val visitsJson = result.convertTo[VisitsJson]
          visitsJson.visits.foreach(StorageServiceImpl.insert)
          Some(visitsJson)
        case _ => None
      }
      inputStream.close()
      entities
    }.toList
    result.flatten
  }

  def main(args: Array[String]): Unit = {
    prepareData()
    unfiltered.netty.Server.http(80)
      .handler(Palindrome)
      .handler(Time)
      .handler(HighLoadCup)
      .run()
    dispatch.Http.default.shutdown()
  }
}

sealed trait EntitiesJson
case class UsersJson(users: Seq[User]) extends EntitiesJson
case class VisitsJson(visits: Seq[Visit]) extends EntitiesJson
case class LocationsJson(locations: Seq[Location]) extends EntitiesJson