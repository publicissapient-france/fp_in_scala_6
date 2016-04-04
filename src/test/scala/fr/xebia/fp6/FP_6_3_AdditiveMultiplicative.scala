package fr.xebia.fp6

import algebra.ring.{MultiplicativeMonoid, AdditiveMonoid}
import cats.std.boolean._
import cats.std.list.listInstance
import cats.{Foldable, Monoid}
import org.scalatest.{FunSpec, ShouldMatchers}

class FP_6_3_AdditiveMultiplicative extends FunSpec with ShouldMatchers {

	describe("The boolean additive monoid") {

		// SEEME 3_Additive_1
		implicit val monoid: Monoid[Boolean] = ???

		it("should fold an empty list of boolean to false") {

			Foldable[List].fold(List.empty[Boolean]) shouldBe false

		}

		it("should fold a list of booleans to true when there is at least one true") {

			Foldable[List].fold(List(false, true, false)) shouldBe true

		}

		it("should fold a list of booleans to false when there is only false values") {

			Foldable[List].fold(List(false, false)) shouldBe false

		}
	}

	describe("The boolean multiplicative monoid") {

		// SEEME 3_Additive_2
		implicit val monoid: Monoid[Boolean] = ???

		it("should fold an empty list of boolean to true") {

			Foldable[List].fold(List.empty[Boolean]) shouldBe true

		}

		it("should fold a list of booleans to false when there is at least one false") {

			Foldable[List].fold(List(false, true, true)) shouldBe false

		}

		it("should fold a list of booleans to true when there is only true values") {

			Foldable[List].fold(List(true, true)) shouldBe true

		}
	}


	object example3 {

		import cats.std.int.intGroup

		// SEEME 3_Additive_3
		class GenericCounter[T: Monoid] {

			import cats.syntax.semigroup._

			// SEEME 3_Additive_4
			private lazy val M = implicitly[Monoid[T]]

			//Mutable for presentation mode ;-)
			// SEEME 3_Additive_5
			private var counter: T = M.empty

			// SEEME 3_Additive_6
			def incrementedWith(newValue: T): Unit = {
				this.counter = counter |+| newValue
			}

			def current: T = counter

		}

		sealed trait HttpResult

		case object `200 OK` extends HttpResult

		case object `500 Internal Server Error` extends HttpResult


	}

	describe("A generic counter") {
		import example3._
		import cats.std.int._

		it("should start with empty value from the monoid") {
			// SEEME 3_Additive_7
			new GenericCounter[Int].current shouldBe 0

			new GenericCounter[Boolean]()(Monoid.additive).current shouldBe false
			new GenericCounter[Boolean]()(Monoid.multiplicative).current shouldBe true

		}

		describe("for additive result monoid") {

			// SEEME 3_Additive_8
			implicit val additiveHttpResult = new AdditiveMonoid[HttpResult] {

				override def zero: HttpResult = `500 Internal Server Error`

				override def plus(x: HttpResult, y: HttpResult): HttpResult = (x, y) match {
					case (`500 Internal Server Error`, `500 Internal Server Error`) => `500 Internal Server Error`
					case (_, _) => `200 OK`
				}
			}


			implicit val monoid: Monoid[HttpResult] = Monoid.additive[HttpResult]

			it("should start with `500 Internal Server Error`") {
				// SEEME 3_Additive_8
				new GenericCounter[HttpResult].current shouldBe `500 Internal Server Error`


			}

			it("be `500 Internal Server Error` when only `500 Internal Server Error` values are added") {
				// SEEME 3_Additive_9
				val counter = new GenericCounter[HttpResult]

				counter incrementedWith `500 Internal Server Error`
				counter incrementedWith `500 Internal Server Error`

				counter.current shouldBe `500 Internal Server Error`

			}

			it("be `200 OK` when with at least one `200 OK` value") {
				// SEEME 3_Additive_10
				val counter = new GenericCounter[HttpResult]

				counter incrementedWith `500 Internal Server Error`
				counter incrementedWith `200 OK`
				counter incrementedWith `500 Internal Server Error`

				counter.current shouldBe `200 OK`

			}
		}

		describe("for multiplicative result monoid") {

			// SEEME 3_Additive_11
			implicit val multiplicativeHttpResult = new MultiplicativeMonoid[HttpResult] {

				override def one: HttpResult = `200 OK`

				override def times(x: HttpResult, y: HttpResult): HttpResult = (x, y) match {
					case (`200 OK`, `200 OK`) => `200 OK`
					case (_, _) => `500 Internal Server Error`
				}
			}

			implicit val monoid: Monoid[HttpResult] = Monoid.multiplicative[HttpResult]

			it("should start with `200 OK`") {

				// SEEME 3_Additive_12
				new GenericCounter[HttpResult].current shouldBe `200 OK`


			}

			it("be `500 Internal Server Error` when at least one `500 Internal Server Error` is added") {

				// SEEME 3_Additive_13
				val counter = new GenericCounter[HttpResult]

				counter incrementedWith `200 OK`
				counter incrementedWith `500 Internal Server Error`
				counter incrementedWith `200 OK`

				counter.current shouldBe `500 Internal Server Error`

			}

			it("be `200 OK` when only `200 OK` values are added") {

				// SEEME 3_Additive_14
				val counter = new GenericCounter[HttpResult]

				counter incrementedWith `200 OK`
				counter incrementedWith `200 OK`
				counter incrementedWith `200 OK`

				counter.current shouldBe `200 OK`

			}
		}
	}
}