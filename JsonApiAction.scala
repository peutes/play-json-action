package action

import javax.inject.Inject
import play.api.libs.json._
import play.api.mvc.Results.{ BadRequest, InternalServerError }
import play.api.mvc._
import scalaz.Scalaz._
import scalaz._

import scala.concurrent.{ ExecutionContext, Future }

private[action] case class JsonApiRequest[A](
  jsValue:      JsValue,
  request:      Request[A],
  jwtAuth:      Option[JwtAuth],
) extends WrappedRequest[A](request)

class JsonApiAction @Inject() (
  val parser: BodyParsers.Default,
)(implicit val executionContext: ExecutionContext) extends ActionBuilder[JsonApiRequest, AnyContent] with ActionRefiner[Request, JsonApiRequest] {

  def refine[A](request: Request[A]): Future[Either[Result, JsonApiRequest[A]]] = {
    for {
      jsValue <- parseJson(request)
    } yield {
      JsonApiRequest(jsValue = jsValue, request = request, jwtAuth = None)
    }
  }.toEither

  def asyncByOptionT[Json <: JsonOfApi](
    callback: (JsonApiRequest[AnyContent], Json, ExecutionContext) => OptionT[Future, Result]
  )(implicit read: Reads[Json], writes: OWrites[Json]): Action[AnyContent] =
    async { implicit request: JsonApiRequest[AnyContent] =>

      val value: JsValue = (request.jsValue \ JsonKey.data).getOrElse(request.jsValue)
      Json.fromJson(value) match {
        case JsSuccess(json: Json, _) => {
          callback(request, json, executionContext)
            .run
            .map(_.getOrElse(throw new Exception("failed to get result.")))
            .recoverWith {
              case e: Throwable =>
                logger.error(e.getMessage)
                Future(InternalServerError(failureJson("unknown error")))
            }
        }
        case _ =>
          Future(BadRequest(failureJson("invalid request")))
      }
    }

  private def parseJson[A](request: Request[A])(implicit ec: ExecutionContext): EitherT[Future, Result, JsValue] = EitherT(Future(
    request.body.asInstanceOf[AnyContent].asJson \/> JsonResults.badRequestErrorJson
  ))

  private def failureJson(description: String): JsObject = Json.obj("status" -> "failed", "description" -> description)
}
