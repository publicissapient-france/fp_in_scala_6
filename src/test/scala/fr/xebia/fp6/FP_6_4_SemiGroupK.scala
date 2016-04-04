package fr.xebia.fp6

import cats.{Foldable, MonoidK, SemigroupK}
import org.scalatest.{ShouldMatchers, FunSpec}

class FP_6_4_SemiGroupK extends FunSpec with ShouldMatchers {

	object example4 {

		implicit object SetMultiplicativeSemigroup extends SemigroupK[Set] {

			override def combine[A](x: Set[A], y: Set[A]): Set[A] = x intersect y

		}

		implicit object SetAdditiveMonoid extends MonoidK[Set] {

			override def empty[A]: Set[A] = Set.empty

			override def combine[A](x: Set[A], y: Set[A]): Set[A] = x union y
		}

	}

	describe("A multiplicative semigroup for Set[_]") {

		import cats.syntax.semigroupk._

		import example4.SetMultiplicativeSemigroup

		it("should combine 2 Sets as intersections") {

			val left = Set(1, 2, 3)

			val right = Set(2, 4, 6)

			left <+> right shouldBe Set(2)

		}
	}

	describe("An additive monoid for Set[_]") {

		import cats.syntax.semigroupk._
		import cats.std.list._

		import example4.SetAdditiveMonoid

		it("should combine 2 Sets as unions") {

			val left = Set(1, 2, 3)

			val right = Set(2, 4, 6)

			left <+> right shouldBe Set(1, 2, 3, 4, 6)

		}

		it("could be used to fold a List of Sets") {
			val left = Set(1, 2, 3)

			val right = Set(2, 4, 6)

			val list = List(left, right)

			Foldable[List].foldK(list) shouldBe Set(1, 2, 3, 4, 6)
			
			Foldable[List].fold(list)(implicitly[MonoidK[Set]].algebra) shouldBe Set(1, 2, 3, 4, 6)

		}
	}
}
