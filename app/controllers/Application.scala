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

object GameView extends Controller {
  import models.form.{ GameViewForm => Form }

  /**
   * Login状態をチェックする
   */
  private object User {
    val ROOMNAME = "game_view_roomname"
    val KEY = "game_view_key"
    val MAXPEOPLE = "game_view_maxpeople"
    val TIMEOUT = Option(24000)
    val NULLTIME = Option(-1)

    def is(success : String => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      request.cookies.get(ROOMNAME).map { r => //メンドクサイからroomnameだけみる。実運用時はこれじゃだめ。。。
        println("@view connect : roomname => " + r.value)
        success(r.value)
      }.getOrElse {
        failed
      }
    }

    def login(success : => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      val b = Form.login.bind
      if (!GameRoomMonitor.isExsist(b.roomname)) { //roomnameが被らないか確認 
        println("@view login : roomname => " + b.roomname + ", key => " + b.key + ", maxpeople => " + b.maxpeople)
        success.withCookies(
            Cookie(ROOMNAME, b.roomname, TIMEOUT),
            Cookie(KEY, b.key, TIMEOUT),
            Cookie(MAXPEOPLE, b.maxpeople, TIMEOUT)
            ) //cookieに値を保存
      } else {
        failed
      }
    }

    def logout(success : => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      is({ r =>
        { 
          println("@view logout : roomname => " + r)
          GameRoomMonitor.delete(r)
          success.withCookies(
              Cookie(ROOMNAME, "hoge", NULLTIME),
              Cookie(KEY, "hoge", NULLTIME),
              Cookie(MAXPEOPLE, "hoge", NULLTIME)
              )
        }
      }, {
        failed
      })
    }

    
    def values(implicit request : Request[AnyContent])  = {
      
    }
    
    def roomname(implicit request : Request[AnyContent]) =
      {
        request.cookies.get(ROOMNAME).map { u =>
          Some(u.value)
        }.getOrElse {
          None
        }
      }

    def roomname(implicit request : RequestHeader) = {
      request.cookies.get(ROOMNAME).map { u =>
        Some(u.value)
      }.getOrElse {
        None
      }
    }
  }

  /**
   * Just display the home page.
   */
  def login = Action { implicit request =>
    User.is({
      u => Redirect(routes.GameView.main())
    }, {
      val form = Form.login.form
      Ok(views.html.gameview.login(form))
    })
  }

  /**
   * do login
   */
  def dologin = Action { implicit request =>
    User.login(Redirect(routes.GameView.main()), Redirect(routes.GameView.login).flashing("error" -> "The room name has already existed"))
  }

  /**
   * do logout
   */
  def dologout = Action { implicit request =>
    User.logout({
      Redirect(routes.GameView.login)
    }, {
      Redirect(routes.GameView.login).flashing("error" -> "ろぐいんしてません")
    })
  }

  /**
   * Display the chat room page.
   */
  def main = Action { implicit request =>
    User.is({
        u => Ok(views.html.gameview.main(u))
    },
      {
        Redirect(routes.GameView.login).flashing("error" -> "Please choose a valid username.")
      })
  }

  /**
   * Handles the chat websocket.
   */
  def signal =
    WebSocket.async[JsValue] { implicit request =>
      {
        User.roomname(request) match {
          case Some(u) => GameRoomMonitor.create(u)
          case None    => null //取れない場合はアプリを落とす(一時対処、実運用時は用対応)
        }
      }
    }
}

/**
 * This controller for game controller
 */
object GameController extends Controller {
  import models.form.{ GameControllerForm => Form }

  /**
   * Login状態をチェックする
   */
  private object User {
    val ROOMNAME = "game_controller_roomname"
    val USERNAME = "game_controller_username"
    val KEY = "game_controller_key"
    val TEAM = "game_controller_team"
    val TIMEOUT = Option(24000)
    val NULLTIME = Option(-1)

    def is(success : (String,String,String) => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      
      (request.cookies.get(ROOMNAME),request.cookies.get(USERNAME),request.cookies.get(TEAM)) match{
        case (Some(r),Some(u),Some(t)) => {
          println("@controller connect : roomname => " + r.value + ", username => " + u.value + ", team => " + t.value)
          success(r.value,u.value,t.value) 
        }
        case (_,_,_) => failed
      }
    }
    
    def login(success : => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
       val b = Form.login.bind
       println("@controller login : roomname => " + b.roomname + ", key => " + b.key + ", username => " + b.username + ", team => " + b.team)
        success.withCookies(
            Cookie(ROOMNAME, b.roomname, TIMEOUT),
            Cookie(USERNAME, b.username, TIMEOUT),
            Cookie(KEY, b.key, TIMEOUT),
            Cookie(TEAM, b.team, TIMEOUT)
        ) //cookieに値を保存
     

       // failed
      
    }
    
    def logout(success : => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      is({ (r,u,t) =>
        {
          println("@controller logout : roomname => " + r + ", username => " + u + ", team => " + t)
          success.withCookies(
              Cookie(ROOMNAME, "hoge", NULLTIME),
              Cookie(USERNAME, "hoge", NULLTIME),
              Cookie(KEY, "hoge", NULLTIME),
              Cookie(TEAM, "hoge", NULLTIME)
            ) //cookieの値を消去
        }
      }, {
        failed
      })
    }
  }

  /**
   * Just display the home page.
   */
  def login = Action { implicit request =>
    User.is({
      (r,u,t) =>  Redirect(routes.GameController.main())
    }, {
      val form = Form.login.form
      Ok(views.html.gamecontroller.login(form))
    })
  }

  /**
   * do login
   */
  def dologin = Action { implicit request =>
    User.login(Redirect(routes.GameController.main()), Redirect(routes.GameController.login).flashing("error" -> "The room name has already existed"))
  }
  
  def dologout = Action { implicit request =>
  	User.logout(Redirect(routes.GameController.login()), Redirect(routes.GameController.login).flashing("error" -> "ろぐいんしてへんよ"))
  }
  
  def main = Action { implicit request =>
  	User.is({
        (r,u,t) => Ok(views.html.gamecontroller.main(r,u,t))
  	}, {
  	    Redirect(routes.GameController.login).flashing("error" -> "なんかまちがってるよ")
  	}
  	)
  	
  }
  
  def signal = Action { implicit request =>
  	Ok("まだ")
  }

}