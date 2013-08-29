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
import views.html.defaultpages.badRequest


/**
 * ゲームのコントローラー(mvcのcじゃないよ)
 * 機能 : ログイン, ajaxでの信号送信
 */
object GameController extends Controller {
  import models.form.{ GameControllerForm => Form }

  /**
   * ユーザセッション周り(実際にはすべてcookieで対応)
   */
  private object User {
    val ROOMNAME = "game_controller_roomname"
    val USERNAME = "game_controller_username"
    val KEY = "game_controller_key"
    val TEAM = "game_controller_team"
    val ROLE = "game_controller_role"
    val TIMEOUT = Option(24000)
    val NULLTIME = Option(-1)

    /**
     * ログインしているかどうかと、ルームが存在しているかどうか
     * 面倒なので、セキュリティは無視
     */
    def is(success : (String, String, String,String) => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      val c = request.cookies
      (c.get(ROOMNAME), c.get(TEAM), c.get(USERNAME),c.get(ROLE)) match {
        case (Some(room), Some(team), Some(username),Some(role)) => {
          if(GameRoomMonitor.isExistUser(room.value,team.value,username.value)) //
          {
             println("@controller connect : roomname => " + room.value + ", username => " + username.value + ", team => " + team.value + ", role => " + role.value)
             success(room.value, team.value, username.value,role.value)
          } else{
            failed
          }
        }
        case (_, _, _, _) => failed
      }
    }
    
    def values(implicit request : Request[AnyContent]) = {
      (request.cookies.get(ROOMNAME), request.cookies.get(USERNAME), request.cookies.get(TEAM)) match {
        case (Some(r), Some(u), Some(t)) => {
        	Some(r.value,u.value,t.value)
        }
        case (_,_,_) => None
      }
    }

    /**
     * ログインを行う
     * actorに参加
     */
    def doLogin(success : => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      val b = Form.login.bind  
      println("@controller login : roomname => " + b.roomname + ", key => " + b.key + ", username => " + b.username + ", team => " + b.team)
      GameRoomMonitor.join(b.roomname, b.team, b.username).map{ role =>
             println("@controller login : roomname => " + b.roomname + ", key => " + b.key + ", username => " + b.username + ", team => " + b.team + ", role => " + role)
        	 success.withCookies(
        			 Cookie(ROOMNAME, b.roomname, TIMEOUT),
        			 Cookie(USERNAME, b.username, TIMEOUT),
        			 Cookie(KEY, b.key, TIMEOUT),
        			 Cookie(ROLE, role, TIMEOUT),
        			 Cookie(TEAM, b.team, TIMEOUT)) //cookieに値を保存
      }.getOrElse(failed)
    }

    /**
     * ログアウト
     * actorから去る
     */
    def doLogout(success : => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      is({ (roomname, team, username,role) =>
        {
          GameRoomMonitor.leave(roomname, team, username, role)
          println("@controller logout : roomname => " + roomname + ", username => " + username + ", team => " + team + ", role => " + role)
          success.withCookies(
            Cookie(ROOMNAME, "hoge", NULLTIME),
            Cookie(USERNAME, "hoge", NULLTIME),
            Cookie(KEY, "hoge", NULLTIME),
            Cookie(ROLE, "hoge", NULLTIME),
            Cookie(TEAM, "hoge", NULLTIME)) //cookieの値を消去
        }
      }, {
        failed
      })
    }
  }

  /**
   * ログインページ
   */
  def login = Action { implicit request =>
    User.is({
      (roomname, team, username,role) => Redirect(routes.GameController.index())
    }, {
      val form = Form.login.form
      Ok(views.html.gamecontroller.login(form))
    })
  }

  /**
   * do login
   */
  def dologin = Action { implicit request =>
    User.doLogin(
        Redirect(routes.GameController.index()),
        Redirect(routes.GameController.login).flashing("error" -> "ログイン出来ませんでした。")
        )
  }

  def dologout = Action { implicit request =>
    User.doLogout(
        Redirect(routes.GameController.login()), 
        Redirect(routes.GameController.login).flashing("error" -> "ろぐいんしてへんよ")
      )
  }

  def index = Action { implicit request =>
    User.is({
      (roomname, team, username, role) => Ok(views.html.gamecontroller.index(roomname,team,username,role))
    }, {
      Redirect(routes.GameController.login)
    })
  }

  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(Routes.javascriptRouter("jsRoutes")(routes.javascript.GameController.ajax)).as("text/javascript")
  }

  def ajax() = Action { implicit request =>
    request.body.asFormUrlEncoded.map{ b =>
   	(b.get("roomname"),b.get("team"),b.get("username"),b.get("role")) match { //キャッシュから取得出来るけど、とりあえず
   		case (Some(roomname),Some(team),Some(username),Some(role)) => {
   		  User.is({
   			  (rm, r, u, re) => {
   			    GameRoomMonitor.forwardSignal(roomname.head, team.head, username.head, role.head)
   			    Ok("Success")
   			  }
   		  }, {
   			  BadRequest("aa")
   		  })
   		}
   		case (_,_,_,_) => BadRequest("aa")
     }
    }.getOrElse{
      BadRequest("aa")
    }
  }
  
  def signal = Action { implicit request =>
    Ok("まだ")
  }

}