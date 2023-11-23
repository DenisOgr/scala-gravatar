package scalaGravatar

import org.scalatest.flatspec.AnyFlatSpec

class MD5UtilTest extends AnyFlatSpec {
  behavior of "MD5Util"

  it should "convert email into correct MD5 hash" in {
    assert(MD5Util.md5Hex("some@some.com") == "21dd5f1417efd1dad68f1e223c7b1c1b")
    assert(MD5Util.md5Hex("some@some.com") != "invalid")
  }
}
