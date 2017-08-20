package com.example

import java.util.Date

import dao.entity.{Location, User, Visit}
import dao.serice.{NotFoundException, StorageServiceImpl}
import spray.json._
import unfiltered.netty.cycle.Plan.Intent
import unfiltered.netty.{ServerErrorResponse, cycle}
import unfiltered.request.{Body, _}
import unfiltered.response.{BadRequest, NotFound, Ok, ResponseString}

object FromDateParam extends Params.Extract("fromDate", Params.first ~> Params.long)

object ToDateParam extends Params.Extract("toDate", Params.first ~> Params.long)

object ToDistanceParam extends Params.Extract("toDate", Params.first ~> Params.int)

object CountryParam extends Params.Extract("country", Params.first)

@io.netty.channel.ChannelHandler.Sharable
object HighLoadCup extends cycle.Plan with cycle.SynchronousExecution with ServerErrorResponse {

  import com.example.MyJsonProtocol._

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
          StorageServiceImpl.findUser(id.toInt).fold(NotFound ~> ResponseString(""))(user =>
            Ok ~> ResponseString(user.toJson.compactPrint))
        } catch {
          case e: Exception => NotFound ~> ResponseString(e.getMessage)
        }
      case POST(_) =>
        try {
          val oldUser = StorageServiceImpl.findUser(id.toInt)
          if (oldUser.isEmpty) NotFound
          else {
            var user = oldUser.get
            val jsonBody = JsonParser(ParserInput(Body.bytes(req)))
            val userJsonModel = jsonBody.convertTo[UserJsonForUpdate]
            if (userJsonModel.email.nonEmpty) user = user.copy(email = userJsonModel.email.get)
            if (userJsonModel.first_name.nonEmpty) user = user.copy(first_name = userJsonModel.first_name.get)
            if (userJsonModel.last_name.nonEmpty) user = user.copy(last_name = userJsonModel.last_name.get)
            if (userJsonModel.gender.nonEmpty) user = user.copy(gender = userJsonModel.gender.get)
            if (userJsonModel.birth_date.nonEmpty) user = user.copy(birth_date = userJsonModel.birth_date.get)
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
          StorageServiceImpl.findVisit(id.toInt).fold(NotFound ~> ResponseString(""))(visit =>
            Ok ~> ResponseString(visit.toJson.compactPrint))
        } catch {
          case e: Exception => NotFound ~> ResponseString(e.getMessage)
        }
      case POST(_) =>
        try {
          val oldVisit = StorageServiceImpl.findVisit(id.toInt)
          if (oldVisit.isEmpty) NotFound
          else {
            var visit = oldVisit.get
            val jsonBody = JsonParser(ParserInput(Body.bytes(req)))
            val visitJsonModel = jsonBody.convertTo[VisitJsonForUpdate]
            if (visitJsonModel.location.nonEmpty) visit = visit.copy(location = visitJsonModel.location.get)
            if (visitJsonModel.user.nonEmpty) visit = visit.copy(user = visitJsonModel.user.get)
            if (visitJsonModel.visited_at.nonEmpty) visit = visit.copy(visited_at = visitJsonModel.visited_at.get)
            if (visitJsonModel.mark.nonEmpty) visit = visit.copy(mark = visitJsonModel.mark.get)
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
          StorageServiceImpl.findLocation(id.toInt).fold(NotFound ~> ResponseString(""))(location =>
            Ok ~> ResponseString(location.toJson.compactPrint))
        } catch {
          case e: Exception => NotFound ~> ResponseString(e.getMessage)
        }
      case POST(_) =>
        try {
          val oldLocation = StorageServiceImpl.findLocation(id.toInt)
          if (oldLocation.isEmpty) NotFound
          else {
            var location = oldLocation.get
            val jsonBody = JsonParser(ParserInput(Body.bytes(req)))
            val locationJsonModel = jsonBody.convertTo[LocationJsonForUpdate]
            if (locationJsonModel.place.nonEmpty) location = location.copy(place = locationJsonModel.place.get)
            if (locationJsonModel.country.nonEmpty) location = location.copy(country = locationJsonModel.country.get)
            if (locationJsonModel.city.nonEmpty) location = location.copy(city = locationJsonModel.city.get)
            if (locationJsonModel.distance.nonEmpty) location = location.copy(distance = locationJsonModel.distance.get)
            StorageServiceImpl.update(location)
            Ok ~> ResponseString("""{}""")
          }
        } catch {
          case e: Exception => BadRequest ~> ResponseString(e.getMessage)
        }
    }

    case req@Path(Seg("users" :: id :: "visits" :: Nil)) & Params(params) => req match {
      case GET(_) =>
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

        println(params.mkString)

        try {
          val visits = StorageServiceImpl.getVisits(id.toInt, if (seq.isEmpty) None else Some(filter))
          Ok ~> ResponseString(VisitJsonWrapper(visits.map(VisitJsonHelper.from)).toJson.compactPrint)
        } catch {
          case NotFoundException(msg) => NotFound ~> ResponseString(msg)
          case e: Exception =>
            BadRequest ~> ResponseString(e.getMessage)
        }
      case POST(_) => Ok
    }

    case req@Path(Seg("locations" :: id :: "avg" :: Nil)) & Params(params) => req match {
      case GET(_) =>
        val fromDateParam = params.get("fromDate").flatMap(_.headOption)
        val toDateParam = params.get("toDate").flatMap(_.headOption)
        val fromAgeParam = params.get("fromAge").flatMap(_.headOption)
        val toAgeParam = params.get("toAge").flatMap(_.headOption)
        val genderParam = params.get("gender").flatMap(_.headOption)

        val seq = Seq(fromDateParam, toDateParam, fromAgeParam, toAgeParam, genderParam).flatten

        def countAge(dateOfBirth: Long): Long = {
          val now = new Date().getTime
          val diff = now - dateOfBirth
          diff / (1000L * 60 * 60 * 24 * 365)
        }

        def filter: Visit => Boolean = v => {
          val userOpt = StorageServiceImpl.findUser(v.user)
          fromDateParam.forall(from => v.visited_at > from.toLong) &&
            toDateParam.forall(to => v.visited_at < to.toLong) &&
            fromAgeParam.forall(age => userOpt.forall(u => countAge(u.birth_date) > age.toLong)) &&
            toAgeParam.forall(toAge => userOpt.forall(u => countAge(u.birth_date) < toAge.toLong)) &&
            genderParam.forall(gender => userOpt.forall(u => u.gender == gender))
        }

        println(params.mkString)
        try {
          val avg = StorageServiceImpl.getLocationAvg(id.toInt, if (seq.isEmpty) None else Some(filter))
          avg.fold(NotFound ~> ResponseString(""))(avg => Ok ~> ResponseString(AvgJson(avg).toJson.compactPrint))
        } catch {
          case e: Exception => BadRequest ~> ResponseString(e.getMessage)
        }
      case POST(_) => Ok
    }
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