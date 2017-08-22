package dao.serice

import dao.entity.{Location, User, Visit}

import scala.collection.mutable

trait StorageService {

  def findUser(id: Int): Option[User]
  def findVisit(id: Int): Option[Visit]
  def findLocation(id: Int): Option[Location]
  def getVisits(userId: Int, filterOpt: Option[Visit => Boolean]): Seq[Visit]
  def getLocationAvg(locationId: Int, filterOpt: Option[Visit => Boolean]): Option[Double]

  def insert(user: User): Unit
  def insert(visit: Visit): Unit
  def insert(location: Location): Unit

  def update(user: User): Unit
  def update(visit: Visit): Unit
  def update(location: Location): Unit

}

object StorageServiceImpl extends StorageService
{
  type VisitId = Int
  type UserId = Int
  type LocationId = Int

  private val users: mutable.Map[Int, User] = mutable.Map.empty
  private val visits: mutable.Map[Int, Visit] = mutable.Map.empty
  private val locations: mutable.Map[Int, Location] = mutable.Map.empty

  override def findUser(id: Int): Option[User] = users.get(id)

  override def findVisit(id: Int): Option[Visit] = visits.get(id)

  override def findLocation(id: Int): Option[Location] = locations.get(id)

  override def getVisits(userId: Int, filterOpt: Option[Visit => Boolean]): Seq[Visit] = {
    if(findUser(userId).isEmpty) throw NotFoundException("user not found by user id")
    val allVisits = visits.filter(_._2.user == userId).values.toList
    if(filterOpt.isEmpty) allVisits.sortBy(_.visited_at)
    else allVisits.filter(filterOpt.get).sortBy(_.visited_at)
  }

  override def getLocationAvg(locationId: Int, filterOpt: Option[Visit => Boolean]): Option[Double] = {
    locations.get(locationId).map
    {_ =>
      val visitsByLocation = visits.filter(_._2.location == locationId).values
      if(visitsByLocation.isEmpty) 0.00
      else{
        if(filterOpt.isEmpty) Math.round(visitsByLocation.map(_.mark).sum.toDouble / visitsByLocation.size * 100000d) / 100000d
        else {
          val filtered = visitsByLocation.filter(filterOpt.get)
          Math.round(filtered.map(_.mark).sum.toDouble / filtered.size * 100000d) / 100000d
        }
      }
    }
  }

  override def insert(user: User): Unit = users += user.id -> user

  override def insert(visit: Visit): Unit = visits += visit.id -> visit

  override def insert(location: Location): Unit = locations += location.id -> location

  override def update(user: User): Unit = users.update(user.id, user)

  override def update(visit: Visit): Unit = visits.update(visit.id, visit)

  override def update(location: Location): Unit = locations.update(location.id, location)
}

case class NotFoundException(msg: String) extends Exception {
  override def getMessage = msg
}