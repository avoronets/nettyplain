import unfiltered.netty.cycle.Plan.Intent
import unfiltered.netty.{ServerErrorResponse, cycle}
import unfiltered.request.{GET, POST, Path, Seg}
import unfiltered.response.Ok

object HighLoadCup extends cycle.Plan
with cycle.SynchronousExecution with ServerErrorResponse {

  override def intent: Intent = {
    case GET(Path(Seg("users" :: id :: Nil))) => Ok

    case GET(Path(Seg("users" :: id :: "visits" :: Nil))) => Ok

    case GET(Path(Seg("locations" :: id :: "avg" :: Nil))) => Ok

    case POST(Path(Seg("users" :: id :: Nil))) => Ok

    case POST(Path(Seg("locations" :: id :: Nil))) => Ok

    case POST(Path(Seg("visits" :: id :: Nil))) => Ok

    case POST(Path("/users/new")) => Ok

    case POST(Path("/locations/new")) => Ok

    case POST(Path("/visits/new")) => Ok
  }
}
