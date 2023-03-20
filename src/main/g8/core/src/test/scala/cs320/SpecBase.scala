package cs320

import org.scalatest.flatspec.AnyFlatSpec

trait SpecBase extends AnyFlatSpec {

  def normalize(s: String): String =
    s.replaceAll("Spec.", "")
      .replaceAll("Implementation.", "")
      .replaceAll("Predef.", "")
      .replaceAll("`package`.", "")
      .replaceAll(".apply", "")
      .replaceAll("\\\\n", " ")
      .replaceAll("\\s+", " ")
      .replaceAll("\\\\'", "'")
}
