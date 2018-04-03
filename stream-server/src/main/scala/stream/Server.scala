package stream

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.PredefinedToEntityMarshallers
import akka.http.scaladsl.model.{HttpEntity, MediaTypes}
import akka.http.scaladsl.server.Directives._
import akka.stream.{ActorMaterializer, Materializer}
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._


import scala.concurrent.ExecutionContext

object Server extends App with PredefinedToEntityMarshallers {
  implicit val system: ActorSystem = ActorSystem("stream-server")
  implicit val materialize: Materializer = ActorMaterializer()
  implicit val ec: ExecutionContext = system.dispatcher

  val s3ClientBuilder = AmazonS3ClientBuilder.standard()
  s3ClientBuilder.setRegion("ams3")
  s3ClientBuilder.setEndpointConfiguration(new EndpointConfiguration("https://ams3.digitaloceanspaces.com", ""))
  val s3Client = s3ClientBuilder.build()

  val getObject: Storage.GetObject = Storage.get(s3Client)

  val route =
    cors() {
      path("get" / Remaining) { location =>
        get {
          complete(HttpEntity(MediaTypes.`audio/ogg`, getObject(Storage.Location(location))))
        }
      }
    }


  Http().bindAndHandle(route, "0.0.0.0", 8080)
}
