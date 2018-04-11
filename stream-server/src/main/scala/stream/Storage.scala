package stream

import java.io.{BufferedInputStream, ByteArrayOutputStream}

import akka.util.ByteString
import com.amazonaws.services.s3.AmazonS3
import org.joda.time.DateTime

object Storage {
  case class Location(value: String) extends AnyVal

  type GetObject = (Location) => (ByteString, String)

  def get(s3Client: AmazonS3): GetObject = location => {
    println(s"get $location")
    val startTime = DateTime.now()

    val obj = s3Client.getObject("music-stream", location.value)

    val is = new BufferedInputStream(obj.getObjectContent)
    val os = new ByteArrayOutputStream()

    val buf = Array.ofDim[Byte](8192)

    try {
      Stream.continually {
        val numRead = is.read(buf, 0, 8192)
        if (numRead > 0) {
          os.write(buf, 0, numRead)
          true
        } else {
          false
        }
      }.takeWhile(_ == true).force
    } finally {
      is.close()
      os.close()
    }

    val res = ByteString(os.toByteArray)

    val endTime = DateTime.now()
    println(s"get took ${endTime.getMillis - startTime.getMillis} ms")

    (res, obj.getObjectMetadata.getContentType)
  }
}
