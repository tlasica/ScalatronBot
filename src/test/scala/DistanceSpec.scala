import org.scalatest.{WordSpec, Matchers}


/**
 * Created by tomek on 17.01.15.
 */
class DistanceSpec extends WordSpec with Matchers {

  "calculateDistance()" should {
    "calculate correct distance on empty ground" in {
      val ground = Array.fill(3,3)(true)
      val dist = Distance.calculateDistanceArray(ground, 0, 0)
      dist(0)(1) shouldBe 1
      dist(1)(0) shouldBe 1
      dist(1)(1) shouldBe 1
      dist(0)(2) shouldBe 2
      dist(1)(2) shouldBe 2
      dist(2)(2) shouldBe 2
    }

    /**
     * 0010
     * 0110
     * 0100
     * 0000
     *
     */
    "calculate correct distance with some walls" in {
      val ground = Array.fill(4,4)(true)
      ground(0)(2) = false
      ground(1)(1) = false
      ground(1)(2) = false
      ground(2)(1) = false
      val dist = Distance.calculateDistanceArray(ground, 0, 0)
      dist(0)(1) shouldBe 1
      dist(1)(0) shouldBe 1
      dist(1)(1) shouldBe Int.MaxValue
      dist(0)(2) shouldBe Int.MaxValue
      dist(0)(3) shouldBe 6
      dist(3)(1) shouldBe 3
    }

    "calculate correct on large ground" in {
      val ground = Array.fill(99,99)(true)
      val dist = Distance.calculateDistanceArray(ground, 0, 0)
      dist(8)(8) shouldBe 8
    }

    "calculate from middle of the square" in  {
      val ground = Array.fill(11,11)(true)
      val dist = Distance.calculateDistanceArray(ground, 5, 5)
      ground(2)(4) = false
      ground(3)(3) = false
      ground(4)(3) = false
      ground(5)(3) = false
      ground(6)(3) = false
      dist(1)(9) shouldBe 4
    }

  }


}
