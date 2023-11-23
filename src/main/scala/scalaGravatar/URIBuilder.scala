package scalaGravatar
import java.net.{URI, URLDecoder, URLEncoder}

case class URIBuilder private (
                                        protocol: Option[String],
                                        host: Option[String],
                                        port: Option[Int],
                                        path: Option[List[Segment]],
                                        queryParams: Option[Map[String, List[String]]],
                                        isLastSlash: Boolean = false
                                      ) {

  def withProtocol(protocol: String): URIBuilder = copy(protocol=Some(protocol))
  def withHost(host: String): URIBuilder = copy(host=Some(host))
  def withPort(port: Int): URIBuilder = copy(port=Some(port))

  /**
   * Encode and replace path
   * @param path: String
   * @return
   */
  def withPath(path: String): URIBuilder = {
    val (newPath, isLastSlash) = URIBuilder.decodePath(path)
    copy(path=Some(newPath), isLastSlash = isLastSlash)
  }

  def appendPath(path: String): URIBuilder = {
    val (newPath, isLastSlash) = URIBuilder.decodePath(path)
    copy(isLastSlash = isLastSlash, path = Some(this.path.getOrElse(Nil) ::: newPath))
  }

  def appendQueryParams(params: String): URIBuilder = {
    copy(queryParams = Some(queryParams.getOrElse(Map.empty) ++ URIBuilder.decodeQueryParams(params).map({
      case (k,v) => k -> (queryParams.getOrElse(Map.empty).getOrElse(k, Nil) ::: v)
    })))
  }
  def withQueryParams(params: String): URIBuilder = {
    copy(queryParams = Some(URIBuilder.decodeQueryParams(params)))
  }
  def segments(segments: String*): URIBuilder = {
    copy(path = Option(segments.map(Segment.decode).toList))
  }

  def emptyPath: URIBuilder = copy(path = Option(Nil))

  def emptyQueryParams: URIBuilder = copy(queryParams=Option(Map.empty))

  def build : URI = {
    new URI(
      protocol.getOrElse("https"),
      null,
      host.getOrElse(""),
      port.getOrElse(-1),
      URIBuilder.encodePath(path.getOrElse(Nil), isLastSlash),
      URIBuilder.encodeQueryParams(queryParams.getOrElse(Map.empty)),
      null
    )
  }

  override def toString: String = build.toString
}
/**
 * This object builds Builder for Java URI objects.
 * The default method apply accept the Java URI object and create builder based on URI.
 * Methods:
 * - apply - described below
 * - decodePath(path: String): (List[Segment], Boolean) - convert string to the list of segments.
 * Also return boolean info about last slash. (Apply FP snipped of code.)
 *
 *
 */
object URIBuilder {

  val QueryParamPattern = """(?i)(\w+)=(.*)?""".r
  /**
   * URI.host -> URIBuilder.host
   * URI.port -> URIBuilder.port
   * URI.protocol -> URIBuilder.isSSL
   * URI.path: String -> URIBuilder.path: List[Segment] //method
   * URI.query (String) -> URIBuilder.queryParams: Map[String, List[String] //method with regex
   *
   * @return
   */
  def apply(uri: URI): URIBuilder = {
    val (path, isLastSlash) = decodePath(uri.getPath)
    new URIBuilder(
      Some(uri.getScheme),
      Some(uri.getHost),
      Some(uri.getPort),
      Some(path),
      Some(decodeQueryParams(uri.getQuery)),
      isLastSlash
    )
  }

  def fromRaw(
               protocol: Option[String],
               host: Option[String],
               port: Option[Int],
               path: Option[List[Segment]],
               queryParams: Option[Map[String, List[String]]],
               isLastSlash: Boolean = false
             ): URIBuilder = {
    new URIBuilder(
      protocol,
      host,
      port,
      path,
      queryParams,
      isLastSlash
    )
  }
  def empty: URIBuilder = new URIBuilder(None, None, None, Option(Nil),Option(Map.empty))

  /**
   * Convert (decode) String path into list of the Segments.
   * Return with info about latest slash.
   * Steps: Wrap into Option->filter(trim and check if string empty)->map(split into substrings): Option[List[String]
   * ->map(convert to list of segments)->map(convert from list of the segments to tuple(segments, is last slash))
   *
   * @param path:String
   * @return
   */
  def decodePath(path: String): (List[Segment], Boolean) = {
      Option(path)
        .filterNot(_.trim.isEmpty)
        .map({
          case s if s.startsWith("/") => s.substring(1)
          case s => s
        })
        .map(_.split("/").toList)
        .map(_.map(Segment.decode))
        .map((_, path.endsWith("/")))
        .getOrElse((Nil,false))
  }

  /**
   * Convert list of the Segments to the string.
   */
  def encodePath(path: List[Segment], isLastSlash: Boolean = false): String = {
    path.map(_.encode).mkString("/", "/", if(isLastSlash) "/" else "")
  }

  /**
   * Map(("k1"->List("v1", "v2"), ("k2" -> List("v3")))) = "?k1=v1&k1=v2&k2=v3"
   */
  def encodeQueryParams(params: Map[String, List[String]]): String = {
    params.map({case (k,v)=>v.map(vc=>s"$k=$vc").mkString("&")}).mkString("&")
  }

  /**
   *  "?k1=v1&k1=v2&k2=v3" => Map(("k1"->List("v1", "v2"), ("k2" -> List("v3"))))
   *  remove "?" left trim("?") => string
   *  split("&")  => list of string List("k1=v1","k1=v2","k2=v3")
   *  map and match with regex =>  (k -> v)
   *  foldLeft: start with empty Map, func: overwrite k with map.getOrElse(nil) :+ v => Map
   */
  def decodeQueryParams(params: String): Map[String, List[String]] = {
    Option(params).map({
      case s if s.startsWith("?") => params.substring(1)
      case _ => params
    }).get.split("&")
      .map {
        case QueryParamPattern(k, "") => (k, "")
        case QueryParamPattern(k, v) => (k,v)
      }.foldLeft(Map[String, List[String]]())({
      case (acc, (k,v)) if v != "" =>  acc + (k -> (acc.getOrElse(k, Nil) :+ v))
      case (acc, _) => acc
    })

  }
}

case class Segment(segment: String) {
  /**
   * Encode using UTF-8 segment string to string
   * @return
   */
  def encode: String = URLEncoder.encode(segment, "UTF-8")
}

object Segment {
  /**
   * Convert string to Segment.
   * @return
   */
  def decode(seg: String): Segment = Segment(URLDecoder.decode(seg, "UTF-8"))
}
