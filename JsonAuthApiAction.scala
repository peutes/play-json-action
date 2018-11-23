package action

import domain.exception.UnknownException
import javax.inject.Inject
import play.api.libs.json._
import play.api.mvc._
import scalaz.Scalaz._
import scalaz._

import scala.concurrent.{ ExecutionContext, Future }

private[action] case class JsonApiRequest[A](
  jsValue:      JsValue,
  request:      Request[A],
  jwtAuth:      Option[JwtAuth],
) extends WrappedRequest[A](request)

class JsonAuthApiAction @Inject() (
  val parser: BodyParsers.Default,
)(implicit val executionContext: ExecutionContext) extends ActionBuilder[JsonApiRequest, AnyContent] with ActionRefiner[Request, JsonApiRequest] with  {

  def refine[A](request: Request[A]): Future[Either[Result, JsonApiRequest[A]]] = {
    for {
      jsValue <- parseJson(request)
      jwtAuth <- validateToken(jsValue)
    } yield {
      JsonApiRequest(jsValue = jsValue, request = request, jwtAuth = jwtAuth.some)
    }
  }.toEither

  def asyncByOptionT[Json <: AuthJsonOfApi](
    callback: (JsonApiRequest[AnyContent], Json, ExecutionContext) => OptionT[Future, Result]
  )(implicit read: Reads[Json], writes: OWrites[Json]): Action[AnyContent] =
    async { implicit request: JsonApiRequest[AnyContent] =>

      val value: JsValue = (request.jsValue \ JsonKey.data).getOrElse(request.jsValue)
      val result: EitherT[Future, Result, Result] = Json.fromJson(value) match {
        case JsSuccess(json: Json, _) => for {
          _ <- validateKey(json.expectedJwtAuthKey)
          result <- EitherT(
            callback(request, json.asInstanceOf[Json], executionContext)
              .run
              .map(r => \/-(r.getOrElse(throw new UnknownException("failed to get result."))))
              .recoverWith {
                case e: Throwable =>
                  Future(-\/(JsonResults.internalServerErrorJson))
              }
          )
        } yield result

        case _ =>
          EitherT[Future, Result, Result](Future(-\/(JsonResults.badRequestErrorJson)))
      }
      result.fold(r => r, r => r)
    }

  private def validateKey[A](expectedJwtAuthKey: String)(implicit request: JsonApiRequest[A]): EitherT[Future, Result, JwtAuth]

  private def validateToken(jsValue: JsValue): EitherT[Future, Result, JwtAuth]

  private def parseJson[A](request: Request[A])(implicit ec: ExecutionContext): EitherT[Future, Result, JsValue] = EitherT(Future(
    request.body.asInstanceOf[AnyContent].asJson \/> JsonResults.badRequestErrorJson
  ))

}
