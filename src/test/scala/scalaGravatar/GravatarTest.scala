package scalaGravatar
import org.scalatest.flatspec.AnyFlatSpec

import java.net.{URI, URL}

class GravatarTest extends AnyFlatSpec{
  behavior of "Gravatar"

  def fixture =
    new {
      val gravatar: Gravatar = Gravatar(Some(true), Some(true), Some(MPImage()), Some(123), Some(G()))
    }

  it should "build proper and new Gravatar object after changing isSecure" in {
    val f = fixture
    val baseGravatar = f.gravatar
    val actualGravatar = baseGravatar.withSecure(false)
    assert(actualGravatar != baseGravatar)
    assert(!actualGravatar.isSecure.getOrElse(false))
  }

  it should "build proper and new Gravatar object after changing flag forceDefault" in {
    val baseGravatar = fixture.gravatar
    val actualGravatar = baseGravatar.withForceDefault(false)
    assert(baseGravatar != actualGravatar)
    assert(!actualGravatar.forceDefault.getOrElse(false))
  }

  it should "build proper and new Gravatar object after changing default" in {
    val baseGravatar = fixture.gravatar
    val actualGravatar = baseGravatar.withDefault(RetroImage())
    assert(baseGravatar != actualGravatar)
    assert(actualGravatar.default.getOrElse(BlankImage()) == RetroImage())
  }

  it should "build the proper and new Gravatar object after changing the size" in {
    val baseGravatar = fixture.gravatar
    val actualGravatar = baseGravatar.withSize(10)
    assert(baseGravatar != actualGravatar)
    assert(actualGravatar.size.getOrElse(-1) == 10)

    assertThrows[IllegalArgumentException](baseGravatar.withSize(201))
    assertThrows[IllegalArgumentException](baseGravatar.withSize(0))
  }

  it should "build the proper and new Gravatar object after changing the rating" in {
    val baseGravatar = fixture.gravatar
    val actualGravatar = baseGravatar.withRating(X())
    assert(baseGravatar != actualGravatar)
    assert(actualGravatar.rating.getOrElse(G()) == X())
  }

  it should "build the proper URIBuilder object" in {
    val baseGravatar = Gravatar(Some(true), Some(true), Some(MPImage()), Some(199), Some(X()))
    val actualUriBuilder: URIBuilder = baseGravatar.initBuilder
    assert(actualUriBuilder.protocol.contains("https"))
    assert(actualUriBuilder.host.contains("gravatar.com"))
    val path  = actualUriBuilder.path.getOrElse(Nil)
    assert(path.nonEmpty)
    assert(path.head.encode == "avatar")

    val queryParams = actualUriBuilder.queryParams.getOrElse(Map.empty)

    // check forceDefault
    assert(queryParams.contains("f"))
    assert(queryParams.getOrElse("f", Nil).nonEmpty)
    assert(queryParams.getOrElse("f", Nil).head == "y")

    // check default
    assert(queryParams.contains("d"))
    assert(queryParams.getOrElse("d", Nil).nonEmpty)
    assert(queryParams.getOrElse("d", Nil).head == MPImage().name)

    //check rating
    assert(queryParams.contains("r"))
    assert(queryParams.getOrElse("r", Nil).nonEmpty)
    assert(queryParams.getOrElse("r", Nil).head == X().name)

    //check size
    assert(queryParams.contains("s"))
    assert(queryParams.getOrElse("s", Nil).nonEmpty)
    assert(queryParams.getOrElse("s", Nil).head == "199")
  }

  it should "build proper avatar url" in {
    val gravatar = Gravatar(Some(true), Some(false), Some(MonsteridImage()), Some(198), Some(X()))
    val email: String = "some@some.com"
    val actualUrl = gravatar.getAvatar(email)
    actualUrl.contains(MD5Util.md5Hex(email))
    new URL(actualUrl).toURI
  }

  // test download image
}

class DefaultImageTest extends AnyFlatSpec {
  behavior of "DefaultImage object"

  it should "build proper default image object" in {
    assert(DefaultImage("invalid") == BlankImage())
    assert(DefaultImage("mp") == MPImage())
    assert(DefaultImage("identicon") == IdenticonImage())
    assert(DefaultImage("monsterid") == MonsteridImage())
    assert(DefaultImage("wavatar") == WavatarImage())
    assert(DefaultImage("retro") == RetroImage())
    assert(DefaultImage("robohash") == RobohashImage())
    assert(DefaultImage("identicon") == IdenticonImage())
  }
}

class RatingTest extends AnyFlatSpec {
  behavior of "Rating"

  it should "creates proper rating" in {
    assert(Rating("g") == G())
    assert(Rating("pg") == PG())
    assert(Rating("r") == R())
    assert(Rating("x") == X())
    assertThrows[InvalidRatingError](Rating("invalid") == X())
  }
}
