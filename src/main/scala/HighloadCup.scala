package com.example

import dao.entity.Visit
import dao.serice.StorageServiceImpl
import spray.json._
import unfiltered.netty.cycle.Plan.Intent
import unfiltered.netty.{ServerErrorResponse, cycle}
import unfiltered.request._
import unfiltered.response.{BadRequest, NotFound, Ok, ResponseString}

object FromDateParam extends Params.Extract("fromDate", Params.first ~> Params.long)
object ToDateParam extends Params.Extract("toDate", Params.first ~> Params.long)
object ToDistanceParam extends Params.Extract("toDate", Params.first ~> Params.int)
object CountryParam extends Params.Extract("country", Params.first)

@io.netty.channel.ChannelHandler.Sharable
object HighLoadCup extends cycle.Plan with cycle.SynchronousExecution with ServerErrorResponse {

  import com.example.MyJsonProtocol._

  override def intent: Intent = {
    case req @ Path(Seg("users" :: id :: Nil)) => req match {
      case GET(_) =>
        try{
          StorageServiceImpl.findUser(id.toInt).fold(NotFound ~> ResponseString(""))(user =>
            Ok ~>  ResponseString(user.toJson.compactPrint))
        } catch {
          case e: Exception => BadRequest ~> ResponseString(e.getMessage)
        }
      case POST(_) =>  Ok
    }

    case req @ Path(Seg("visits" :: id :: Nil)) => req match {
      case GET(_) =>
        try{
          StorageServiceImpl.findVisit(id.toInt).fold(NotFound ~> ResponseString(""))(visit =>
            Ok ~>  ResponseString(visit.toJson.compactPrint))
        } catch {
          case e: Exception => BadRequest ~> ResponseString(e.getMessage)
        }
      case POST(_) =>  Ok
    }

    case req @ Path(Seg("locations" :: id :: Nil)) => req match {
      case GET(_) =>
        try{
          StorageServiceImpl.findLocation(id.toInt).fold(NotFound ~> ResponseString(""))(location =>
            Ok ~>  ResponseString(location.toJson.compactPrint))
        } catch {
          case e: Exception => BadRequest ~> ResponseString(e.getMessage)
        }
      case POST(_) =>  Ok
    }

    case req @ Path(Seg("users" :: id :: "visits" :: Nil)) & Params(params) => req match {
      case GET(_) =>
        val fromDateParam = params.get("fromDate").flatMap(_.headOption)
        val toDateParam = params.get("toDate").flatMap(_.headOption)
        val countryParam = params.get("country").flatMap(_.headOption)
        val toDistanceParam = params.get("toDistance").flatMap(_.headOption)

        val seq = Seq(fromDateParam, toDateParam, countryParam, toDistanceParam).flatten

        lazy val filter: Visit => Boolean =  v => {
          val locOpt = StorageServiceImpl.findLocation(v.location)
          fromDateParam.forall(from => v.visited_at > from.toLong) &&
            toDateParam.forall(to => v.visited_at > to.toLong) &&
            countryParam.flatMap(country => locOpt.map(loc => loc.country == country)).getOrElse(true) &&
            toDistanceParam.flatMap(toDist => locOpt.map(_.distance < toDist.toInt)).getOrElse(true)
        }

        println(params.mkString)

        try{
          val visits = StorageServiceImpl.getVisits(id.toInt, if(seq.isEmpty) None else Some(filter))
          if(visits.isEmpty) NotFound ~> ResponseString("")
          else Ok ~> ResponseString(VisitJsonWrapper(visits.map(VisitJsonHelper.from)).toJson.compactPrint)
        } catch {
          case e: Exception =>
            BadRequest ~> ResponseString(e.getMessage)
        }
      case POST(_) =>  Ok
    }

    case req @ Path(Seg("locations" :: id :: "avg" :: Nil)) => req match {
      case GET(_) =>
        try{
          val avg = StorageServiceImpl.getLocationAvg(id.toInt, None)
          avg.fold(NotFound ~> ResponseString(""))(avg => Ok ~> ResponseString(AvgJson(avg).toJson.compactPrint))
        } catch {
          case e: Exception => BadRequest ~> ResponseString(e.getMessage)
        }
      case POST(_) =>  Ok
    }

    case POST(Path(Seg("users" :: id :: Nil))) => Ok

    case POST(Path(Seg("locations" :: id :: Nil))) => Ok

    case POST(Path(Seg("visits" :: id :: Nil))) => Ok

    case POST(Path("/users/new")) => Ok

    case POST(Path("/locations/new")) => Ok

    case POST(Path("/visits/new")) => Ok
  }
}


case class VisitJson(mark: Int, visited_at: Long, place: String)
case class VisitJsonWrapper(visits: Seq[VisitJson])
case class AvgJson(avg: Double)

object VisitJsonHelper{
  def from(visit: Visit) = {
    val loc = StorageServiceImpl.findLocation(visit.location)
    VisitJson(visit.mark, visit.visited_at, loc.map(_.place).getOrElse(""))
  }
}