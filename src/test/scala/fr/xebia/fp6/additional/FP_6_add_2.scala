package fr.xebia.fp6.additional

import java.io.File

import algebra.Order
import org.scalatest.{FunSpec, OptionValues, ShouldMatchers}

object example2 {

  implicit object FileOrder extends Order[File] {

    private val superFolder = 1
    private val subFile = -1

    override def compare(x: File, y: File): Int = {
      val xPath = x.toPath
      val yPath = y.toPath

      import scala.collection.convert.wrapAsScala._

      if (xPath.getParent == yPath.getParent) {
        xPath.getFileName.compareTo(yPath.getFileName)
      } else {
        if (xPath.size < yPath.size) {
          subFile
        } else if (xPath.size > yPath.size) {
          superFolder
        } else {
          xPath.compareTo(yPath)
        }
      }

    }
  }

}

class FileOrderSpec extends FunSpec with ShouldMatchers with OptionValues {

  import example2._
  import cats.syntax.order._

  val root = new File("/")

  val tmp = new File("/tmp")

  val file1 = new File("/tmp/file1")

  val file2 = new File("/tmp/file2")

  val file3 = new File("/another_tmp/file3")

  describe("The total order for files") {

    it("should be able to compare any two files within the same folder") {
      file1 < file2 shouldBe true
      file2 > file1 shouldBe true
    }

    it("should be able to compare a file and its parent folder") {
      file1 > tmp shouldBe true

      tmp < file1 shouldBe true
    }

    it("should be able to compare with the upper common folder of two files in different folder") {
      file3 < file1 shouldBe true

      tmp < file3 shouldBe true
    }

    it("should be able to sort a list of file") {
      List(file2, file3, root, file1, tmp).sorted(Order.ordering(FileOrder)) shouldBe List(root, tmp, file3, file1, file2)

    }
  }
}