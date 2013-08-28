package controllers;

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

/**
 * ゲーム用画面(mvcのコントローラー)
 * 機能としては1.ゲームルーム作成, 2.websocket通信
 */
object GameView extends Controller {
  import models.form.{ GameViewForm => Form }

  /**
   * ユーザセッション周り(実際にはすべてcookieで対応)
   */
  private object User {
    val ROOMNAME = "game_view_roomname"
    val KEY = "game_view_key"
    val MAXPEOPLE = "game_view_maxpeople"
    val TIMEOUT = Option(24000) //出来るだけ長くするが、いつかは消える。もしクッキーが消えた場合、どのタイミングでゲームアクターを停止させるか...? => バッチ処理
    val NULLTIME = Option(-1)
    
    /**
     * ログインしているかどうかと、ルームが存在しているかどうか
     * 面倒なので、セキュリティは無視してcookieにroomnameが保持されているかだけ見る　
     */
    def is(implicit request : Request[AnyContent]): Boolean = {
      request.cookies.get(ROOMNAME).map { r => {
          if(GameRoomMonitor.isExsistRoom(r.value)){
        	  println("@view connect : roomname => " + r.value)
        	  true
          } else{
             false
          }
      	}
      }.getOrElse {
        false
      }
    }

    /**
     *   ログインしているかどうかと、ルームが存在しているかどうか
     *   ログインしているならsuccesを、していないならfailedを実行
     *  ・面倒なので、セキュリティは無視してcookieにroomnameが保持されているかだけ見る(セッション無視、使ってない)
     */
    def is(success : String => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      request.cookies.get(ROOMNAME).map { r => 
        if(GameRoomMonitor.isExsistRoom(r.value)){
        	  println("@view connect : roomname => " + r.value)
        	  success(r.value)
          } else{
             failed
          }
      }.getOrElse {
        failed
      }
    }

    /**
     *  ログインを行う
     * ・セッションは使わないでクッキーのみ。セッションの代わりにゲームアクターを使う
     */
    def doLogin(success : => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      val b = Form.login.bind //フォームをバインド
      //actorを作成
      GameRoomMonitor.create(b.roomname).map{ a => 
        println("@view login : roomname => " + b.roomname + ", key => " + b.key + ", maxpeople => " + b.maxpeople)
        //ルーム名、キー、最大人数をクッキーに保存(セッションは用いないため)
        success.withCookies(
          Cookie(ROOMNAME, b.roomname, TIMEOUT),
          Cookie(KEY, b.key, TIMEOUT),
          Cookie(MAXPEOPLE, b.maxpeople, TIMEOUT)) //cookieに値を保存
      }getOrElse{
        failed
      }
    }

    /**
     * ログアウトを行う　
     */
    def doLogout(success : => Result, failed : => Result)(implicit request : Request[AnyContent]) = {
      is({ r =>
        {
          println("@view logout : roomname => " + r)
          GameRoomMonitor.delete(r)
          //ヌル値を設定
          success.withCookies(
            Cookie(ROOMNAME, "hoge", NULLTIME),
            Cookie(KEY, "hoge", NULLTIME),
            Cookie(MAXPEOPLE, "hoge", NULLTIME))
        }
      }, {
        failed
      })
    }

    /**
     * cookieからルーム名を取得
     */
    def roomname(implicit request : Request[AnyContent]) =
      {
        request.cookies.get(ROOMNAME).map { u =>
          Some(u.value)
        }.getOrElse {
          None
        }
      }

     /**
     * cookieからルーム名を取得
     */
    def roomname(implicit request : RequestHeader) = {
      request.cookies.get(ROOMNAME).map { u =>
        Some(u.value)
      }.getOrElse {
        None
      }
    }
  }

  /**
   * ログインページ
   * ログインしてたらメインページにリダイレクト、してない場合ログイン画面を表示
   */
  def login(any : String = "") = Action { implicit request =>
    User.is({
      u => Redirect(routes.GameView.index())
    }, {
      val form = Form.login.form
      Ok(views.html.gameview.login(form))
    })
  }

  /**
   * ログインを行う
   * 成功したらメインページ
   */
  def dologin = Action { implicit request =>
    User.doLogin(
        Redirect(routes.GameView.index()), 
        Redirect(routes.GameView.login).flashing("error" -> "ルームの名前が被ってるよ")
        )
  }

  /**
   * ログアウトを行う
   * 成功したらログインページ
   */
  def dologout = Action { implicit request =>
    User.doLogout({
      Redirect(routes.GameView.login)
    }, {
      Redirect(routes.GameView.login).flashing("error" -> "ろぐいんしてません")
    })
  }

  /**
   * メインページ
   * ・
   */
  def index = Action { implicit request =>
    User.is({
      u => Ok(views.html.gameview.index(u))
    },
      {
        Redirect(routes.GameView.login)
      })
  }

  /**
   * ウェブソケットのハンドリング
   */
  def signal =
    WebSocket.async[JsValue] { implicit request =>
      {
        User.roomname(request) match { //クッキーからroomnaneを取得
          case Some(r) => {
            GameRoomMonitor.connection(r).map{ c => ///取れたらconnectionを取得
            	c
            }.getOrElse{ ///何らかの理由でアクターが取れなかった場合(一時対処、実運用時は用対応)
              null
            }
          }
          case None    => null //取れない場合はアプリを落とす(一時対処、実運用時は用対応)
        }
      }
    }
}
