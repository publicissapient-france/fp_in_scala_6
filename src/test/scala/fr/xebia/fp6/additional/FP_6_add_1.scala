package fr.xebia.fp6.additional

import java.io.File

import algebra.PartialOrder
import org.scalatest.{FunSpec, OptionValues, ShouldMatchers}

object example1 {

  implicit object FilePartialOrder extends PartialOrder[File] {

    private val samePath = 0
    private val superFolder = 1
    private val subFile = -1

    private val nonComparable = Double.NaN

    override def partialCompare(x: File, y: File): Double = {
      val xPath = x.toPath
      val yPath = y.toPath

      import scala.collection.convert.wrapAsScala._

      if (xPath.getParent == yPath.getParent) {
        samePath
      } else {
        if (xPath.size < yPath.size && yPath.startsWith(xPath)) {
          subFile
        } else if (xPath.size > yPath.size && xPath.startsWith(yPath)) {
          superFolder
        }

        else
          nonComparable
      }
    }
  }

}

class FilePartialOrderSpec extends FunSpec with ShouldMatchers with OptionValues {

  import cats.syntax.partialOrder._
  
  import example1._

  val tmp = new File("/tmp")

  val file1 = new File("/tmp/file1")

  val file2 = new File("/tmp/file2")

  val file3 = new File("/another_tmp/file3")

  describe("The partial order for files") {

    it("should be able to compare files with the same parent path") {
      file1.tryCompare(file2).value shouldBe 0
      file2.tryCompare(file1).value shouldBe 0
      file1.tryCompare(file1).value shouldBe 0
    }

    it("should be able to compare a file and its parent folder") {
      file1 < tmp shouldBe false
      file1 > tmp shouldBe true

      tmp < file1 shouldBe true
      tmp > file1 shouldBe false
    }

    it("should not be able to compare files is different parent paths") {
      file3 < file1 shouldBe false
      file3 > file1 shouldBe false
    }

  }

}