package fr.xebia.fp6

import java.io.File
import java.nio.file.{Paths, Files}

import cats.{Eval, Foldable, Monoid}
import org.scalatest.{FunSpec, ShouldMatchers}

object example2 {

	implicit object FileMonoid extends Monoid[File] {

		override def combine(x: File, y: File): File = {
			if (x.isDirectory) {
				if (y.isDirectory) {
					println(s"x and y are directories,returning $x")
					x
				}
				else {
					println(s"copying ${y.toPath} to ${Paths.get(x.toString, y.getName)}")
					Files.copy(y.toPath, Paths.get(x.toString, y.getName)).toFile
				}
			} else {
				if (y.isDirectory) {
					println(s"copying ${x.toPath} to ${Paths.get(y.toString, x.getName)}")
					Files.copy(x.toPath, Paths.get(y.toString, x.getName)).toFile

				} else {
					val destination = Files.createTempFile(empty.toPath, x.getName, ".merged")

					println(s"merging $destination")

					val allBytes: Array[Byte] = Files.readAllBytes(x.toPath) ++ Files.readAllBytes(y.toPath)

					Files.write(destination, allBytes)

					destination.toFile

				}

			}

		}

		lazy val empty: File = Files.createTempDirectory("exo3_4").toFile
	}

}

class MonoidSpec extends FunSpec with ShouldMatchers {

	import cats.std.list._
	import cats.syntax.semigroup._
	import cats.syntax.order._
	import fr.xebia.fp6.example2.FileMonoid
	import additional.example2.FileOrder

	describe("A monoid for files") {

		it("should merge two files to a new temporary file") {
			val file1 = Files.createTempFile("exo3-4", ".1")
			Files.write(file1, Array(1.toByte))

			val file2 = Files.createTempFile("exo3-4", ".2")
			Files.write(file2, Array(2.toByte))

			val file3 = Files.createTempFile("exo3-4", ".3")
			Files.write(file3, Array(3.toByte))

			val mergedFile = file1.toFile combine file2.toFile

			Files.readAllBytes(mergedFile.toPath) shouldBe Array(1.toByte, 2.toByte)
		}

		it("should be equivalent to merge multiple files from left or right") {
			val file1 = Files.createTempFile("exo3-4", ".1")
			Files.write(file1, Array(1.toByte))

			val file2 = Files.createTempFile("exo3-4", ".2")
			Files.write(file2, Array(2.toByte))

			val file3 = Files.createTempFile("exo3-4", ".3")
			Files.write(file3, Array(3.toByte))

			val files = List(file1, file2, file3).map(_.toFile)

			val mergedFileFromLeft = Foldable[List].foldLeft(files, FileMonoid.empty)(FileMonoid.combine)

			val mergedFileFromRight = Foldable[List].foldRight(files, Eval.now(FileMonoid.empty)) {
				case (file, lazyAcc) => lazyAcc.map(file |+| _)
			}.value


			Files.readAllBytes(mergedFileFromLeft.toPath) shouldBe Array(1.toByte, 2.toByte, 3.toByte)
			Files.readAllBytes(mergedFileFromLeft.toPath) shouldBe Files.readAllBytes(mergedFileFromRight.toPath)

			FileMonoid.empty < mergedFileFromLeft shouldBe true
			FileMonoid.empty < mergedFileFromRight shouldBe true
		}

		it("should reduce a non empty list of files") {
			val file1 = Files.createTempFile("exo3-4", ".1")
			Files.write(file1, Array(1.toByte))

			val file2 = Files.createTempFile("exo3-4", ".2")
			Files.write(file2, Array(2.toByte))

			val file3 = Files.createTempFile("exo3-4", ".3")
			Files.write(file3, Array(3.toByte))

			val files = List(file1, file2, file3).map(_.toFile)

			val mergedFile = Foldable[List].fold(files)

			Files.readAllBytes(mergedFile.toPath) shouldBe Array(1.toByte, 2.toByte, 3.toByte)

			FileMonoid.empty < mergedFile shouldBe true
		}

		it("can reduce an empty list of file") {
			val emptyFold = Foldable[List].fold(List.empty[File])

			emptyFold shouldBe a('directory)
		}
	}
}
