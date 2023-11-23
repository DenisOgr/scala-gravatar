package scalaGravatar

case class Gravatar(
                     isSecure: Option[Boolean] = None,
                     forceDefault: Option[Boolean] = None,
                     default: Option[DefaultImage] = None,
                     size: Option[Int] = None,
                     rating: Option[Rating] = None) {
  private val DEFAULT_SIZE: Int = 100
  private val DEFAULT_IS_SECURE: Boolean = true
  private val DEFAULT_FORCE_DEFAULT: Boolean = true
  private val DEFAULT_IMAGE_DEFAULT: DefaultImage = BlankImage()
  private val DEFAULT_RATING: Rating = G()

  require(size.getOrElse(DEFAULT_SIZE) > 0 && size.getOrElse(DEFAULT_SIZE) < 200, "The size should be positive and no more then 200.")

  def withSecure(isSecure: Boolean): Gravatar = copy(isSecure = Some(isSecure))

  def withForceDefault(forceDefault: Boolean): Gravatar = copy(forceDefault = Some(forceDefault))

  def withDefault(default: DefaultImage): Gravatar = copy(default = Some(default))

  def withSize(size: Int): Gravatar = copy(size = Some(size))

  def withRating(rating: Rating): Gravatar = copy(rating = Some(rating))


  def initBuilder: URIBuilder = {
    val (protocol, host, port) = if (isSecure.getOrElse(DEFAULT_IS_SECURE)) {
      ("https", "gravatar.com", 443)
    } else {
      ("http", "gravatar.com", 80)
    }

    var builder = URIBuilder.empty
      .withProtocol(protocol)
      .withHost(host)
      .withPort(port)
      .withPath("avatar")

    if (forceDefault.getOrElse(DEFAULT_FORCE_DEFAULT)) { // force default
      builder = builder.appendQueryParams("f=y")
    }

    builder = builder.appendQueryParams(s"r=${rating.getOrElse(DEFAULT_RATING).name}") // rating
    builder = builder.appendQueryParams(s"d=${default.getOrElse(DEFAULT_IMAGE_DEFAULT).name}") // default image
    builder = builder.appendQueryParams(s"s=${size.getOrElse(DEFAULT_SIZE)}") //  size
    builder
  }

  def getAvatar(email: String): String = {
    initBuilder.appendPath(MD5Util.md5Hex(email)).build.toString
  }

}

sealed abstract class DefaultImage(_name: String) {
  def name: String = _name
}

case class MPImage() extends DefaultImage("mp")

case class IdenticonImage() extends DefaultImage("identicon")

case class MonsteridImage() extends DefaultImage("monsterid")

case class WavatarImage() extends DefaultImage("wavatar")

case class RetroImage() extends DefaultImage("retro")

case class RobohashImage() extends DefaultImage("robohash")

case class BlankImage() extends DefaultImage("blank")


object DefaultImage {
  def apply(name: String): DefaultImage = {
    name match {
      case "mp" => MPImage()
      case "identicon" => IdenticonImage()
      case "monsterid" => MonsteridImage()
      case "wavatar" => WavatarImage()
      case "retro" => RetroImage()
      case "robohash" => RobohashImage()
      case "identicon" => IdenticonImage()
      case _ => BlankImage()
    }
  }
}

sealed abstract class Rating(_name: String) {
  def name: String = _name
}

case class G() extends Rating("g")

case class PG() extends Rating("pg")

case class R() extends Rating("r")

case class X() extends Rating("x")

class InvalidRatingError extends Error("Invalid rating")

object Rating {
  def apply(name: String): Rating = {
    name match {
      case "g" => G()
      case "pg" => PG()
      case "r" => R()
      case "x" => X()
      case _ => throw new InvalidRatingError()
    }
  }
}
