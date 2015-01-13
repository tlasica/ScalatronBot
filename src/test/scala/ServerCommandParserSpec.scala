import org.scalatest.{Matchers, WordSpec}

/**
 * Created by tomek on 13.01.15.
 */
class ServerCommandParserSpec extends WordSpec with Matchers {

  "Server Command Parser" should {
    "tokenize correctly simple Goodbay command with one parameter" in {
      val cmd = ServerCommandParser.tokenize("Goodbay(energy=234)")
      cmd.optcode shouldBe "Goodbay"
    }
    "tokenize correctly Welcome with multiple parameters" in {
      val cmd = ServerCommandParser.tokenize("Welcome(name=Agent,apocalypse=100,round=0,maxslaves=5)")
      cmd.optcode shouldBe "Welcome"
      cmd.params("name") shouldBe "Agent"
      cmd.params("maxslaves") shouldBe "5"
    }
  }
}
