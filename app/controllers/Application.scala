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
import play.mvc.Results.Todo
import views.html.defaultpages.todo
import models.form.GameViewForm

object GameView extends Controller {
  import models.form.{GameViewForm => Form}
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
      u => Redirect(routes.GameView.room())
    }, {
      val form = Form.login.form
      Ok(views.html.gameview.login(form))
    })
  }

  /**
   * do login
   */
  def dologin = Action { implicit request =>
    val b = Form.login.bind
    println(b)
    if (!GameRoomMonitor.isExsist(b.roomname)) { //roomnameが被らないか確認 
      Redirect(routes.GameView.room()).withCookies(Cookie(ROOMNAME, b.roomname, Option(24000))) //cookieに値を保存
    } else {
      Redirect(routes.GameView.index).flashing("error" -> "The room name has already existed")
    }
  }

  /**
   * do logout
   */
  def dologout = Action { implicit request =>
    isLogin({ u =>
      {
        GameRoomMonitor.delete(u)
        Redirect(routes.GameView.index).withCookies(Cookie(ROOMNAME, "hoge", Option(-1)))
      }
    }, {
      Redirect(routes.GameView.index).flashing("error" -> "Please choose a valid username.")
    })
  }

  /**
   * Display the chat room page.
   */
  def room = Action { implicit request =>
    isLogin({
      u => Ok(views.html.gameview.room(u))
    },
      {
        Redirect(routes.GameView.index).flashing("error" -> "Please choose a valid username.")
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
  import models.form.{GameControllerForm => Form}
 
  /**
   * Login状態をチェックする
   */
  private object LoginChecker {
    val USERNAME = "game_controller_username"
    val KEY = "game_controller_key"
    val GROUP = "game_controller_group"
      
    def is(success: String => Result, failed: => Result)(implicit request: Request[AnyContent]) = {
      request.cookies.get(USERNAME).map { u =>
        success(u.value)
      }.getOrElse {
        failed
      }
    }
  }

  /**
   * Just display the home page.
   */
  def login = Action { implicit request =>
    LoginChecker.is({
      u => Ok(u)
    }, {
      val form = Form.login.form
      Ok(views.html.gamecontroller.login(form))
    })
  }

  /**
   * do login
   */
  def dologin = TODO

}