package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.iteratee._
import models._
import akka.actor._
import scala.concurrent.duration._
import play.api.data.Form
import play.api.data.Forms._

object GameView extends Controller {

  val ROOMNAME = "roomname"

  /**
   * login checker
   */
  private[this] def isLogin(success: String => Result, failed: => Result)(implicit request: Request[AnyContent]) = {
    request.cookies.get(ROOMNAME).map { u =>
      success(u.value)
    }.getOrElse {
      failed
    }
  }

  /**
   * Just display the home page.
   */
  def index = Action { implicit request =>
    isLogin({
      u => Redirect(routes.Game.room())
    }, {
      Ok(views.html.game.index())
    })
  }

  /**
   * do login
   */
  def login = Action { implicit request =>
    case class LoginForm(roomname: String, password: String)
    val form = Form(mapping("roomname" -> text, "password" -> text)(LoginForm.apply)(LoginForm.unapply))
    val d = form.bindFromRequest.get
    if (!GameRoomMonitor.isExsist(d.roomname)) { //roomnameが被らないか確認 
      Redirect(routes.Game.room()).withCookies(Cookie(ROOMNAME, d.roomname, Option(24000))) //cookieに値を保存
    } else {
      Redirect(routes.Game.index).flashing("error" -> "The room name has already existed")
    }
  }

  /**
   * do logout
   */
  def logout = Action { implicit request =>
    isLogin({ u =>
      {
        GameRoomMonitor.delete(u)
        Redirect(routes.Game.index).withCookies(Cookie(ROOMNAME, "hoge", Option(-1)))
      }
    }, {
      Redirect(routes.Game.index).flashing("error" -> "Please choose a valid username.")
    })
  }

  /**
   * Display the chat room page.
   */
  def room = Action { implicit request =>
    isLogin({
      u => Ok(views.html.game.room(u))
    },
      {
        Redirect(routes.Game.index).flashing("error" -> "Please choose a valid username.")
      })
  }

  /**
   * Handles the chat websocket.
   */
  def signal =
    WebSocket.async[JsValue] { request =>
      request.cookies.get(ROOMNAME) match {
        case Some(u) => GameRoomMonitor.create(u.value)
        case None => null //取れない場合はアプリを落とす(一時対処、実運用時は用対応)
      }
    }
}

/**
 * This controller for game controller
 */
object GameController extends Controller {
  object LoginForm{
      case class LoginForm(roomname: String, password: String)
  }
  private object Session {
    val USERNAME = "username"
    val ROOMNAME = "roomname"
    val TEAM = "team"
      
    val form = Form(mapping("roomname" -> text, "password" -> text)(LoginForm.apply)(LoginForm.unapply))
    val d = form.bindFromRequest.get
    def bind()

    def isLogin(success: String => Result, failed: => Result)(implicit request: Request[AnyContent]) = {
      request.cookies.get(ROOMNAME).map { u =>
        success(u.value)
      }.getOrElse {
        failed
      }
    }
  }

  /**
   * Just display the home page.
   */
  def index = Action { implicit request =>
    isLogin({
      u => Redirect(routes.Game.room())
    }, {
      Ok(views.html.game.index())
    })
  }

  /**
   * do login
   */
  def login = Action { implicit request =>
    case class LoginForm(roomname: String, password: String)
    val form = Form(mapping("roomname" -> text, "password" -> text)(LoginForm.apply)(LoginForm.unapply))
    val d = form.bindFromRequest.get
    if (!GameRoomMonitor.isExsist(d.roomname)) { //roomnameが被らないか確認 
      Redirect(routes.Game.room()).withCookies(Cookie(ROOMNAME, d.roomname, Option(24000))) //cookieに値を保存
    } else {
      Redirect(routes.Game.index).flashing("error" -> "The room name has already existed")
    }
  }

}