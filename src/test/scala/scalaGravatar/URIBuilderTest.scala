package scalaGravatar

import org.scalatest.flatspec.AnyFlatSpec

import java.net.URI

class URIBuilderTest extends AnyFlatSpec {
  /**
   * URIBuilder should change {protocol|host|port|path|queryParams} and return new instance in
   */
  behavior of "URIBuilder"

  def fixtute = {
    new {
      val builder: URIBuilder = URIBuilder.fromRaw(Some("http"), Some("0.0.0.0"), Some(8000), Some(List(Segment("some"))), Some(Map("k" -> List("v1"))))
    }
  }

  it should "change protocol and return new instance" in {
    val f = fixtute
    val actualBuilder = f.builder.withProtocol("https")
    assert(f.builder != actualBuilder)
    assert(f.builder.protocol != actualBuilder.protocol)
    assert(actualBuilder.protocol.contains("https"))
  }

  it should "change host and return new instance" in {
    val f = fixtute
    val actualBuilder = f.builder.withHost("127.0.0.1")
    assert(f.builder != actualBuilder)
    assert(f.builder.host != actualBuilder.host)
    assert(actualBuilder.host.contains("127.0.0.1"))
  }

  it should "change port and return new instance" in {
    val f = fixtute
    val actualBuilder = f.builder.withPort(1001)
    assert(f.builder != actualBuilder)
    assert(f.builder.port != actualBuilder.port)
    assert(actualBuilder.port.contains(1001))
  }

  it should "change path and return new instance" in {
    val f = fixtute
    val actualBuilder = f.builder.withPath("/another/new/path/")
    assert(f.builder != actualBuilder)
    assert(f.builder.path != actualBuilder.path)
    assert(f.builder.isLastSlash != actualBuilder.isLastSlash)
    assert(actualBuilder.path.getOrElse(Nil).length == 3)
    assert(actualBuilder.path.getOrElse(Nil).head == Segment("another"))
    assert(actualBuilder.path.getOrElse(Nil)(1) == Segment("new"))
    assert(actualBuilder.path.getOrElse(Nil)(2) == Segment("path"))

    assert(actualBuilder.isLastSlash)
  }

  it should "change queryParams and return new instance" in {
    val f = fixtute
    val actualBuilder = f.builder.withQueryParams("?kt=1&kt=2&kt2=3")
    val expected = Some(Map("kt" -> List("1", "2"), "kt2" -> List("3")))
    assert(f.builder != actualBuilder)
    assert(f.builder.queryParams != actualBuilder.queryParams)
    assert(expected == actualBuilder.queryParams)
  }

  it should "change segments in path and return new instance" in {
    val f = fixtute
    val actualBuilder = f.builder.segments("newSegment1", "newSegment2", "newSegment3")
    assert(actualBuilder != f.builder)
    assert(actualBuilder.path.getOrElse(Nil).length == 3)
    assert(actualBuilder.path.getOrElse(Nil).head == Segment.decode("newSegment1"))
    assert(actualBuilder.path.getOrElse(Nil).tail.head == Segment.decode("newSegment2"))
    assert(actualBuilder.path.getOrElse(Nil).last == Segment.decode("newSegment3"))
  }

  it should "append segments to path and return new instance" in {
    val f = fixtute
    val actualBuilder = f.builder.appendPath("newSegment1/newSegment2/newSegment3/")
    val actualSegments = actualBuilder.path.getOrElse(List.empty).takeRight(3)
    assert(actualSegments.head == Segment.decode("newSegment1"))
    assert(actualSegments.tail.head == Segment.decode("newSegment2"))
    assert(actualSegments.last == Segment.decode("newSegment3"))
  }

  it should "append queryParams and return new instance" in {
    val f = fixtute
    val actualBuilder = f.builder.
      withQueryParams("?k1=v1&k1=v2&k3=v3")
      .appendQueryParams("?k1=v3&k4=v4")

    assert(actualBuilder.queryParams.getOrElse(Map.empty) == Map("k1" -> List("v1", "v2", "v3"), "k3" -> List("v3"), "k4" -> List("v4")))
  }

  it should "clear path and return new instance" in {
    val f = fixtute
    val actualBuilder = f.builder.emptyPath
    assert(actualBuilder.path.getOrElse(Nil).isEmpty)
    assert(f.builder != actualBuilder)
  }

  it should "clear queryParams and return new instance" in {
    val f = fixtute
    val actualBuilder = f.builder.emptyQueryParams
    assert(actualBuilder.queryParams.getOrElse(Map.empty).isEmpty)
    assert(actualBuilder != f.builder)
  }


  it should "build proper URI" in {
    val builder: URIBuilder =
      URIBuilder.fromRaw(Some("http"), Some("0.0.0.0"), Some(8000), Some(List(Segment("some"))), Some(Map("k" -> List("v1"))))

    val actualURI: URI = builder.build
    assert(actualURI.getScheme == builder.protocol.getOrElse(""))
    assert(actualURI.getHost == builder.host.getOrElse(""))
    assert(actualURI.getPort == builder.port.getOrElse(-1))
    assert(actualURI.getPath == URIBuilder.encodePath(builder.path.getOrElse(List.empty)))
    assert(URIBuilder.encodeQueryParams(builder.queryParams.getOrElse(Map.empty)) == actualURI.getQuery)
  }

  it should "convert to proper string" in {
    val builder: URIBuilder =
      URIBuilder.fromRaw(Some("http"), Some("0.0.0.0"), Some(8000), Some(List(Segment("some"))), Some(Map("k" -> List("v1"))))

    assert(builder.toString == "http://0.0.0.0:8000/some?k=v1")
  }

  "companion object" should "create proper URIBuilder instance from URI" in {
    val actualBuilder = URIBuilder(URI.create("http://0.0.0.0:8000/some?k=v1"))
    assert(actualBuilder.protocol.getOrElse("") == "http")
    assert(actualBuilder.host.getOrElse("") == "0.0.0.0")
    assert(actualBuilder.port.getOrElse(-1) == 8000)
    assert(URIBuilder.encodePath(actualBuilder.path.getOrElse(List.empty)) == "/some")
    assert(URIBuilder.encodeQueryParams(actualBuilder.queryParams.getOrElse(Map.empty)) == "k=v1")
  }

  "URIBuilder companion object" should "encode path" in {
    val testPath: String = "/1/2/3"
    val expectedResult: List[Segment] = List(Segment("1"), Segment("2"), Segment("3"))
    assert(URIBuilder.encodePath(expectedResult) == testPath)
  }

  "URIBuilder companion object" should "decode path" in {
    val testPath: String = "1/2/3"
    val expectedResult: List[Segment] = List(Segment("1"), Segment("2"), Segment("3"))
    assert((expectedResult, false) == URIBuilder.decodePath(testPath))
  }
  "URIBuilder companion object" should "encode query params" in {
    val testQueryParams = Map("k1" -> List("1", "2"), "k2" -> List("3"), "k3" -> List("1"))
    val expectedResult = "k1=1&k1=2&k2=3&k3=1"
    assert(expectedResult == URIBuilder.encodeQueryParams(testQueryParams))
  }

  "URIBuilder companion object" should "decode query params" in {
    val expectedQueryParams = Map("k1" -> List("1", "2"), "k2" -> List("3"), "k3" -> List("1"))
    assert(expectedQueryParams == URIBuilder.decodeQueryParams("?k1=1&k1=2&k2=3&k3=1"))
  }
}
