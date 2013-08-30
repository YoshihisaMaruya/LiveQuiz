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
import scala.concurrent.Future
import scala.collection.mutable.LinkedList
import scala.concurrent.Await


/*
object Robot {

  def apply(chatRoom : ActorRef) {
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
}*/

object GameRoomMonitor {
  import scala.collection.mutable.{ Set => MSet, Map => MMap }
  implicit val timeout = Timeout(100 second)
  val future_timeout = 100 second

  private val rooms : MMap[String, String] = MMap.empty //(name => path) actor pathを保持する為に必要

  val teems = Set("1","2")
  val roles = Set("left","right")
  
  /**
   * actorを作成
   */
  def create(roomname : String) : Option[ActorRef] = {
    if (this.isExsistRoom(roomname)) { //actorが存在しているなら
      None
    } else { //いないなら
      try {
        val actor = Akka.system.actorOf(Props(new GameRoomActor(roomname,teems,roles)), roomname) ///アクターを作成
        rooms += (roomname -> actor.path.toString) ///pathを追加
        println("@ create actor : " + actor.path + ", executed actors " + rooms)
        Some(actor)
      } catch { ///exeptionのとき
        case e => {
          e.printStackTrace
          None
        }
      }
    }
  }

  /**
   * actorとコネクションを張る
   */
  def connection(roomname : String) : Option[Future[(Iteratee[JsValue, Unit], Enumerator[JsValue])]] = {
      def _connection(a : ActorRef) = {
        (a ? Creating).map {
          case Created(enumerator) =>
            val iteratee = Iteratee.foreach[JsValue] { event => ///コネクションが続いてるなら、信号を送信
             
            }.mapDone { _ => ///ないならメッセージをログに
              println("room " + roomname + " closed")
               delete(roomname)
               a ! Kill
            }
            (iteratee, enumerator)
        }
      }

    //actorが
    this.get(roomname).map { a => //あるとき
      println("@connection => " + a.path)
      Some(_connection(a))
    }.getOrElse { //無いとき
      this.create(roomname).map { a => ///acctorを作成
        Some(_connection(a))
      }.getOrElse {
        None
      }
    }
  }

  /**
   * actorが存在しているか否か
   */
  def isExsistRoom(roomname : String) : Boolean = {
    rooms.get(roomname).map { a =>
      try {
        Akka.system.actorFor(a) //ちゃんとactor取れるか
        true
      } catch {
        case e => {
          rooms.remove(roomname)
          e.printStackTrace
          false
        }
      }
    }.getOrElse {
      false
    }
  }

  /**
   * actorを取得
   */
  def get(roomname : String) : Option[ActorRef] = {
    rooms.get(roomname).map { a =>
      try {
        Some(Akka.system.actorFor(a))
      } catch {
        case e => {
          rooms.remove(roomname)
          e.printStackTrace
          None
        }
      }
    }.getOrElse {
      None
    }
  }

  /**
   * actorを消去
   */
  def delete(roomname : String) = {
    val actor = get(roomname)
    actor.map { a =>
      println("@ delete actor : " + a.path + " , executed actor : " + rooms)
      a ! Kill // kill actor
      rooms.remove(roomname)
    }.getOrElse(println("actor not found"))
  }
  
  /**
   * actorに参加
   */
  def join(roomname : String,team : String, username : String) : Option[String]= {
    this.get(roomname).map{ a =>
       Await.result(a ? Joininig(team,username),future_timeout) match{
         case Joined(role) => {
           Some(role)
         }
         case CannotJoined => {
           println("@join => actorを取得出来ませんでした")
           None
         }
       }
    }.getOrElse(None)
  }
  
  /**
   * actorから去る
   */
  def leave(roomname : String,team : String, username : String, role : String) = {
     this.get(roomname).map{ a =>
       Await.result(a ? Leaving(team,username,role), future_timeout) match{
         case Leaved => Some
         case _ => None
       }
    }
  }
  
  /**
   * actorにユーザが存在しているか否か
   */
  def isExistUser(roomname : String,team : String, username : String) = {
    this.get(roomname).map{ a =>
      Await.result(a ? IsExist(team,username), future_timeout) match{
         case YesExist => true
         case _ => false
       }
    }.getOrElse(false)
  }
  
  /**
   * 信号をフォワード
   */
  def forwardSignal(roomname : String,team : String, username : String, role : String) = {
    this.get(roomname).map{ a =>
      Await.result(a ? IsExist(team,username), future_timeout) match{
         case YesExist => {
            a ! Signal(team,username,role)
            true
         }
         case _ => false
       }
    }.getOrElse(false)
  }
}

/**
 * gameroomアクター
 */
class GameRoomActor(val roomname : String, teams : Set [String], roles : Set[String]) extends Actor {
  import context._
  import scala.collection.mutable.{ Queue => MQueue , Map => MMap}
  
  var members : MMap[(String,String) , String]= MMap.empty   //(group,username) => role
  val (enumerator, channel) = Concurrent.broadcast[JsValue]

  override def postStop() {
	  println("@actor : " + roomname + " close")
 } 
  val vacants = teams.flatMap(g => Map(g -> {
     var q : MQueue[String]= MQueue.empty
     roles.foreach(q.enqueue(_))
     q
   }
   )).toMap //空き行列を作成
  
  
  def receive  = { //ルーム作成
    case Creating => {
      println("@actor : " + roomname + " open," + " path is "+ self.path)
      sender ! Created(enumerator)
      become(waiting)
    }
  }

  def waiting : Receive = { //待機状態
    case Joininig(team,username) => { //ユーザが参加したい
      println("@actor wait joining : users => " + members.mkString(","))
      if (members.contains((team,username))) { //名前被り
        println("@active =>  " + team + " : " +  username + " has already exsited")
        sender ! CannotJoined
      } else { 
        vacants.get(team).map{ rs => 
          if(rs.isEmpty){ //役割が存在しない
            println("@active =>  " + team + " : " +  username + " さんは役割なし")
            sender ! CannotJoined
          } else{
            val r = rs.dequeue //役割
        	members += ( ((team,username),r)  )
        	channel.push(Json.toJson(Map( //参加した事を伝える
        			   "status" -> "enter",
        			   "username" -> username,
        			   "team" -> team,
        			   "role" -> r
        	   ))) 
        	if(members.size == (teams.size * roles.size)) //メンバーが揃った
        	{
        	   println("become active")
        	   channel.push(Json.toJson(Map("status" -> "active"))) //ゲームの状態をactiveに
        	   become(starting)
        	}
        	sender ! Joined(r)
          }
        }.getOrElse{ //グープが存在しない
          sender ! CannotJoined
        }
      }
    }
    //共通メッセージ
    case Leaving(group,username,role) => leave(group, username, role)
    case IsExist(group,username) => isExist(group, username)
  }
  
  def starting : Receive = { //スタート状態
    case Joininig(group,username) => { //ユーザが参加したい
      sender ! CannotJoined //スタート状態なので駄目
    }
    case Signal(team : String, username : String, role : String) => { //コントローラーからのシグナル
      println("signal")
      channel.push(Json.toJson(Map(
          "status" -> "start",
          "username" -> username,
          "team" -> team,
          "role" -> role
       ))) 
    }
     //共通メッセージ
    case Leaving(group,username,role) => {
      leave(group, username, role)
      become(waiting) //wait状態になる
    }
    case IsExist(group,username) => isExist(group, username)
  }
  
  /**
   * 存在確認
   */
  def isExist(group : String, username : String) = {
    if (members.contains((group,username))) { //存在してる
        sender ! YesExist
        true
      } else {
        sender ! NoExist
        false
      }
  }
  
  /**
   * 退出
   */
  def leave(group : String,username : String, role : String) = {
    vacants.get(group).map{ rs => 
        	rs.enqueue(role)
            members -= ((group,username))
        }
      sender ! Leaved
 }
}

/**
 * *
 * these are actor's message
 */
//ルーム作成
case class Creating(roomname : String)
case class Created(enumerator : Enumerator[JsValue])

//ユーザ参加
case class Joininig(team : String, username : String)
case class Joined(role : String)
case object CannotJoined

//ユーザ退出
case class Leaving(team : String, username : String, role : String)
case object Leaved

case class IsExist(team : String, username : String)
case object YesExist
case object NoExist

//コントローラーからのシグナル
case class Signal(team : String, username : String, role : String)