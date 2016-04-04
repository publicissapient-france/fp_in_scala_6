package fr.xebia.fp6

import java.io.File
import java.nio.file.Files

import algebra.Semigroup
import cats.{NonEmptyReducible, Reducible}
import org.scalatest.{FunSpec, ShouldMatchers}

object example1 {

	implicit object FileSemiGroup extends Semigroup[File] {

		// SEEME 1_Semigroup_1
		override def combine(x: File, y: File): File = {
			val destination = Files.createTempFile(x.getName, ".merged")

			val allBytes: Array[Byte] = Files.readAllBytes(x.toPath) ++ Files.readAllBytes(y.toPath)

			Files.write(destination, allBytes)

			destination.toFile
		}
	}

	implicit val horribleHack: Reducible[List] = new NonEmptyReducible[List, List]()(cats.std.list.listInstance) {
		override def split[A](fa: List[A]): (A, List[A]) = {
			(fa.head, fa.tail)
		}
	}

}

class SemiGroupSpec extends FunSpec with ShouldMatchers {

	import cats.syntax.semigroup._
	import fr.xebia.fp6.example1._

	describe("A semigroup for files") {

		val file1 = Files.createTempFile("exo3-3", ".1")
		Files.write(file1, Array(1.toByte))
		info(file1.toString)

		val file2 = Files.createTempFile("exo3-3", ".2")
		Files.write(file2, Array(2.toByte))
		info(file2.toString)

		val file3 = Files.createTempFile("exo3-3", ".3")
		Files.write(file3, Array(3.toByte))
		info(file3.toString)


		it("should merge two files to a new temporary file") {

			// SEEME 1_Semigroup_2
			val mergedFile: File = ???

			Files.readAllBytes(mergedFile.toPath) shouldBe Array(1.toByte, 2.toByte)

		}

		it("should be equivalent to merge multiple files from left or right") {

			val files = List(file1, file2, file3).map(_.toFile)

			// SEEME 1_Semigroup_3
			val mergedFileFromLeft: File = ???

			val mergedFileFromRight: File = ???

			Files.readAllBytes(mergedFileFromLeft.toPath) shouldBe Array(1.toByte, 2.toByte, 3.toByte)
			Files.readAllBytes(mergedFileFromLeft.toPath) shouldBe Files.readAllBytes(mergedFileFromRight.toPath)
		}

		it("should reduce a non empty list of files") {
			val files = List(file1, file2, file3).map(_.toFile)

			// SEEME 1_Semigroup_4
			val mergedFile: File = ???

			Files.readAllBytes(mergedFile.toPath) shouldBe Array(1.toByte, 2.toByte, 3.toByte)
		}

		it("does not reduce an empty list of file") {
			a[NoSuchElementException] shouldBe thrownBy {
				// SEEME 1_Semigroup_5
				???
			}
		}
	}
}
