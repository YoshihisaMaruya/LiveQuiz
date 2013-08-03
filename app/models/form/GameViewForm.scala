package models.form

import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.Request
import play.api.mvc.AnyContent

object GameViewForm {
 object login{
    case class LoginForm(roomname: String, key: String, maxpeople : String)
    val form = Form(mapping("roomname" -> text, "key" -> text, "maxpeople" -> text)(LoginForm.apply)(LoginForm.unapply))
  
    def apply = form
    
    def bind(implicit request: Request[AnyContent]) = form.bindFromRequest.get
  }
}