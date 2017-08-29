package com.example

import java.time.LocalDate
import java.time.temporal.ChronoUnit

import dao.entity.{Location, User, Visit}
import dao.serice.{NotFoundException, StorageServiceImpl}
import spray.json._
import unfiltered.netty.cycle.Plan.Intent
import unfiltered.netty.{ServerErrorResponse, cycle}
import unfiltered.request.{Body, _}
import unfiltered.response.{BadRequest, NotFound, Ok, ResponseString}
import utils.DateHelper

object FromDateParam extends Params.Extract("fromDate", Params.first ~> Params.long)

object ToDateParam extends Params.Extract("toDate", Params.first ~> Params.long)

object ToDistanceParam extends Params.Extract("toDate", Params.first ~> Params.int)

object CountryParam extends Params.Extract("country", Params.first)

@io.netty.channel.ChannelHandler.Sharable
object HighLoadCup extends cycle.Plan with cycle.SynchronousExecution with ServerErrorResponse {

  import com.example.MyJsonProtocol._

  def canBeParsedToInt(str: String) = try{str.toInt; true}catch {case e: Exception => false}

  override def intent: Intent = {
    case req @ POST(Path("/users/new") & Params(params)) => try {
      val jsonBody = JsonParser(ParserInput(Body.bytes(req)))
      val user = jsonBody.convertTo[User]
      if (StorageServiceImpl.findUser(user.id).nonEmpty) BadRequest
      else {
        StorageServiceImpl.insert(user)
        Ok ~> ResponseString("""{}""")
      }
    } catch {
      case e: Exception => BadRequest ~> ResponseString(e.getMessage)
    }

    case req @ POST(Path("/locations/new")) => try {
      val jsonBody = JsonParser(ParserInput(Body.bytes(req)))
      val location = jsonBody.convertTo[Location]
      if (StorageServiceImpl.findLocation(location.id).nonEmpty) BadRequest
      else {
        StorageServiceImpl.insert(location)
        Ok ~> ResponseString("""{}""")
      }
    } catch {
      case e: Exception => BadRequest ~> ResponseString(e.getMessage)
    }

    case req @ POST(Path("/visits/new")) => try {
      val jsonBody = JsonParser(ParserInput(Body.bytes(req)))
      val visit = jsonBody.convertTo[Visit]
      if (StorageServiceImpl.findVisit(visit.id).nonEmpty) BadRequest
      else {
        StorageServiceImpl.insert(visit)
        Ok ~> ResponseString("""{}""")
      }
    } catch {
      case e: Exception => BadRequest ~> ResponseString(e.getMessage)
    }

    case req@Path(Seg("users" :: id :: Nil)) => req match {
      case GET(_) =>
        try {
          StorageServiceImpl.findUser(id.toInt).fold(NotFound ~> ResponseString("Not found"))(user =>
            Ok ~> ResponseString(user.toJson.compactPrint))
        } catch {
          case e: Exception => NotFound ~> ResponseString(e.getMessage)
        }
      case POST(_) =>
        try {
          val oldUser = StorageServiceImpl.findUser(id.toInt)
          if (oldUser.isEmpty) NotFound ~> ResponseString("Not found")
          else {
            var user = oldUser.get
            val jsonBody = JsonParser(ParserInput(Body.bytes(req)))
            val values = jsonBody.asJsObject().fields
            if (values.contains("email")) values("email") match {
                case JsString(str) => user = user.copy(email = str)
                case _ => throw new Exception("invalid email")
            }
            if (values.contains("first_name")) values("first_name") match {
                case JsString(str) => user = user.copy(first_name = str)
                case _ => throw new Exception("invalid first_name")
            }
            if (values.contains("last_name")) values("last_name") match {
                case JsString(str) => user = user.copy(last_name = str)
                case _ => throw new Exception("invalid last_name")
            }
            if (values.contains("gender")) values("gender") match {
                case str @ (JsString("f") | JsString("m")) => user = user.copy(gender = str.convertTo[String])
                case _ => throw new Exception("invalid gender")
            }
            if (values.contains("birth_date")) values("birth_date") match {
                case JsNumber(value) =>
                    LocalDate.ofEpochDay(value.toLong)
                    user = user.copy(birth_date = value.toLong)
                case _ => throw new Exception("invalid birth_date")
            }
            StorageServiceImpl.update(user)
            Ok ~> ResponseString("""{}""")
          }
        } catch {
          case e: Exception => BadRequest ~> ResponseString(e.getMessage)
        }
    }

    case req@Path(Seg("visits" :: id :: Nil)) => req match {
      case GET(_) =>
        try {
          StorageServiceImpl.findVisit(id.toInt).fold(NotFound ~> ResponseString("Not found"))(visit =>
            Ok ~> ResponseString(visit.toJson.compactPrint))
        } catch {
          case e: Exception => NotFound ~> ResponseString(e.getMessage)
        }
      case POST(_) =>
        try {
          val oldVisit = StorageServiceImpl.findVisit(id.toInt)
          if (oldVisit.isEmpty) NotFound ~> ResponseString("Not found")
          else {
            var visit = oldVisit.get
            val jsonBody = JsonParser(ParserInput(Body.bytes(req)))
            val values = jsonBody.asJsObject().fields
            if (values.contains("location")) values("location") match {
                case JsNumber(number) => visit = visit.copy(location = number.toInt)
                case _ => throw new Exception("invalid location")
            }
            if (values.contains("user")) values("user") match {
                case JsNumber(number) => visit = visit.copy(user = number.toInt)
                case _ => throw new Exception("invalid user")
            }
            if (values.contains("visited_at")) values("visited_at") match {
                case JsNumber(number) =>
                    LocalDate.ofEpochDay(number.toLong)
                    visit = visit.copy(visited_at = number.toInt)
                case _ => throw new Exception("invalid location")
            }
            if (values.contains("mark")) values("mark") match {
                case JsNumber(number)  if number >= 0 && number <=5 =>
                    visit = visit.copy(mark = number.toInt)
                case _ => throw new Exception("invalid location")
            }
            StorageServiceImpl.update(visit)
            Ok ~> ResponseString("""{}""")
          }
        } catch {
          case e: Exception => BadRequest ~> ResponseString(e.getMessage)
        }
    }

    case req@Path(Seg("locations" :: id :: Nil)) => req match {
      case GET(_) =>
        try {
          StorageServiceImpl.findLocation(id.toInt).fold(NotFound ~> ResponseString("Not found"))(location =>
            Ok ~> ResponseString(location.toJson.compactPrint))
        } catch {
          case e: Exception => NotFound ~> ResponseString(e.getMessage)
        }
      case POST(_) =>
        try {
          val oldLocation = StorageServiceImpl.findLocation(id.toInt)
          if (oldLocation.isEmpty) NotFound ~> ResponseString("Not found")
          else {
            var location = oldLocation.get
            val jsonBody = JsonParser(ParserInput(Body.bytes(req)))
            val values = jsonBody.asJsObject().fields
            if (values.contains("place")) values("place") match {
                case JsString(str) => location = location.copy(place = str)
                case _ => throw new Exception("location not valid")
            }

            if (values.contains("country")) values("country") match {
                case JsString(str) => location = location.copy(country = str)
                case _ => throw new Exception("country not valid")
            }
            if (values.contains("city")) values("city") match {
                case JsString(str) => location = location.copy(city = str)
                case _ => throw new Exception("city not valid")
            }
            if (values.contains("distance")) values("distance") match {
                case JsNumber(number) => location = location.copy(distance = number.toInt)
                case _ => throw new Exception("distance not valid")
            }
            StorageServiceImpl.update(location)
            Ok ~> ResponseString("""{}""")
          }
        } catch {
          case e: Exception => BadRequest ~> ResponseString(e.getMessage)
        }
    }

    case req@Path(Seg("users" :: id :: "visits" :: Nil)) & Params(params) => req match {
      case GET(_) =>
        if(canBeParsedToInt(id)){
          val fromDateParam = params.get("fromDate").flatMap(_.headOption)
          val toDateParam = params.get("toDate").flatMap(_.headOption)
          val countryParam = params.get("country").flatMap(_.headOption)
          val toDistanceParam = params.get("toDistance").flatMap(_.headOption)

          val seq = Seq(fromDateParam, toDateParam, countryParam, toDistanceParam).flatten

          def filter: Visit => Boolean = v => {
            val locOpt = StorageServiceImpl.findLocation(v.location)
            fromDateParam.forall(from => v.visited_at > from.toLong) &&
                toDateParam.forall(to => v.visited_at < to.toLong) &&
                countryParam.flatMap(country => locOpt.map(loc => loc.country == country)).getOrElse(true) &&
                toDistanceParam.flatMap(toDist => locOpt.map(_.distance < toDist.toInt)).getOrElse(true)
          }

//          println(params.mkString)

          try {
              if(fromDateParam.nonEmpty) validateDateParam(fromDateParam.get)
              if(toDateParam.nonEmpty) validateDateParam(toDateParam.get)
              if(toDistanceParam.nonEmpty) toDistanceParam.get.toInt

            val visits = StorageServiceImpl.getVisits(id.toInt, if (seq.isEmpty) None else Some(filter))
            Ok ~> ResponseString(VisitJsonWrapper(visits.map(VisitJsonHelper.from)).toJson.compactPrint)
          } catch {
            case NotFoundException(msg) => NotFound ~> ResponseString(msg)
            case e: Exception =>
              BadRequest ~> ResponseString(e.getMessage)
          }
        }else{
          NotFound ~> ResponseString("Not found")
        }

      case POST(_) => Ok
    }

    case req@Path(Seg("locations" :: id :: "avg" :: Nil)) & Params(params) => req match {
      case GET(_) =>
        if(canBeParsedToInt(id)){
          val fromDateParam = params.get("fromDate").flatMap(_.headOption)
          val toDateParam = params.get("toDate").flatMap(_.headOption)
          val fromAgeParam = params.get("fromAge").flatMap(_.headOption)
          val toAgeParam = params.get("toAge").flatMap(_.headOption)
          val genderParam = params.get("gender").flatMap(_.headOption)

          val seq = Seq(fromDateParam, toDateParam, fromAgeParam, toAgeParam, genderParam).flatten

          def filter: Visit => Boolean = v => {
            val userOpt = StorageServiceImpl.findUser(v.user)
                fromDateParam.forall(from => v.visited_at > from.toLong) &&
                toDateParam.forall(to => v.visited_at < to.toLong) &&
                fromAgeParam.forall(fromAge => userOpt.forall(u => DateHelper.countAge(u.birth_date) > fromAge.toLong)) &&
                toAgeParam.forall(toAge => userOpt.forall(u => DateHelper.countAge(u.birth_date) < toAge.toLong)) &&
                genderParam.forall(gender => userOpt.forall(u => u.gender == gender))
          }

          try {

            if(fromDateParam.nonEmpty) validateDateParam(fromDateParam.get)
            if(toDateParam.nonEmpty) validateDateParam(toDateParam.get)
            if(fromAgeParam.nonEmpty) fromAgeParam.get.toLong
            if(toAgeParam.nonEmpty) toAgeParam.get.toLong
            if(genderParam.nonEmpty) genderParam.filter(_ == "f").orElse(genderParam.filter(_ == "m")).get

            val avg = StorageServiceImpl.getLocationAvg(id.toInt, if (seq.isEmpty) None else Some(filter))
            avg.fold(NotFound ~> ResponseString("Not found"))(avg => Ok ~> ResponseString(AvgJson(avg).toJson.compactPrint))
          } catch {
            case e: Exception => BadRequest ~> ResponseString(e.getMessage)
          }
        } else {
          NotFound ~> ResponseString("Not found")
        }

      case POST(_) => Ok
    }
  }

    def validateDateParam(str: String) = {
        val number = str.toLong
        LocalDate.ofEpochDay(number)
    }
}

case class VisitJsonForUpdate(id: Option[Int], location: Option[Int], user: Option[Int], visited_at: Option[Long], mark: Option[Int])

case class LocationJsonForUpdate(id: Option[Int], place: Option[String], country: Option[String], city: Option[String], distance: Option[Int])

case class UserJsonForUpdate(id: Option[Int], email: Option[String], first_name: Option[String],
                             last_name: Option[String], gender: Option[String], birth_date: Option[Long])

case class VisitJson(mark: Int, visited_at: Long, place: String)

case class VisitJsonWrapper(visits: Seq[VisitJson])

case class AvgJson(avg: Double)

object VisitJsonHelper {
  def from(visit: Visit) = {
    val loc = StorageServiceImpl.findLocation(visit.location)
    VisitJson(visit.mark, visit.visited_at, loc.map(_.place).getOrElse(""))
  }
}