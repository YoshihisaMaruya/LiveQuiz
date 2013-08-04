package models

import akka.actor._
import scala.concurrent.duration._
import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import akka.util.Timeout
import akka.pattern.ask
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import scala.collection.mutable.ListBuffer

object Robot {

  def apply(chatRoom: ActorRef) {

    // Create an Iteratee that logs all messages to the console.
    val loggerIteratee = Iteratee.foreach[JsValue](event => Logger("robot").info(event.toString))

    implicit val timeout = Timeout(1 second)
    // Make the robot join the room
    chatRoom ? (Join("Robot")) map {
      case Connected(robotChannel) =>
        // Apply this Enumerator on the logger.
        robotChannel |>> loggerIteratee
    }

    // Make the robot talk every 30 seconds
    Akka.system.scheduler.schedule(
      30 seconds,
      30 seconds,
      chatRoom,
      Talk("Robot", "I'm still alive"))
  }
}

object GameRoomMonitor {
  import scala.collection.mutable.{ Set => MSet, Map => MMap }
  implicit val timeout = Timeout(1 second)

  private val rooms: MMap[String, String] = MMap.empty

  def create(roomname: String) = {
    val actor = Akka.system.actorOf(Props[GameRoomActor], roomname)
    rooms += (roomname -> actor.path.toString)
    println("@ create actor : " + actor.path + ", executed actors " + rooms)
    Robot(actor)

    (actor ? Join(roomname)).map {

      case Connected(enumerator) =>

        // Create an Iteratee to consume the feed
        val iteratee = Iteratee.foreach[JsValue] { event =>
          actor ! Talk(roomname, (event \ "text").as[String])
        }.mapDone { _ =>
          actor ! Quit(roomname)
        }

        (iteratee, enumerator)

      case CannotConnect(error) =>

        // Connection error

        // A finished Iteratee sending EOF
        val iteratee = Done[JsValue, Unit]((), Input.EOF)

        // Send an error and close the socket
        val enumerator = Enumerator[JsValue](JsObject(Seq("error" -> JsString(error)))).andThen(Enumerator.enumInput(Input.EOF))

        (iteratee, enumerator)
    }
  }

  /**
   * check
   */
  def isExsist(roomname: String) = rooms.keys.exists(r => r == roomname)

  /**
   * delete game room
   */
  def delete(roomname: String) = {
    val actor = Akka.system.actorFor(rooms.get(roomname).get) // TODO : Optionのチェック
    println("@ delete actor : " + actor.path + " , executed actor : " + rooms)
    actor ! Kill // kill actor
    rooms.remove(roomname)
  }
}

object GameController{
  
}

class GameRoomActor extends Actor {
  var members = Set.empty[String]
  val (chatEnumerator, chatChannel) = Concurrent.broadcast[JsValue]

  def receive = {

    case Join(roomname) => {

      if (members.contains(roomname)) {
        sender ! CannotConnect("This roomname is already used")
      } else {
        members = members + roomname
        sender ! Connected(chatEnumerator)
        self ! NotifyJoin(roomname)
      }
    }

    case NotifyJoin(roomname) => {
      notifyAll("join", roomname, "has entered the room")
    }

    case Talk(roomname, text) => {
      notifyAll("talk", roomname, text)
    }

    case Quit(roomname) => {
      members = members - roomname
      notifyAll("quit", roomname, "has left the room")
    }

  }

  def notifyAll(kind: String, user: String, text: String) {
    val msg = JsObject(
      Seq(
        "kind" -> JsString(kind),
        "user" -> JsString(user),
        "message" -> JsString(text),
        "members" -> JsArray(
          members.toList.map(JsString))))
    chatChannel.push(msg)
  }

}

/****
 * these are connection signals
 */
case class Join(roomname: String)
case class Quit(roomname: String)
case class Talk(roomname: String, text: String)
case class NotifyJoin(roomname: String)

case class Connected(enumerator: Enumerator[JsValue])
case class CannotConnect(msg: String)

/****
* these are control signals
*/