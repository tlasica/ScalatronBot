import org.scalatest.{Matchers, WordSpec}

/**
 * Created by tomek on 13.01.15.
 */
class ServerCommandParserSpec extends WordSpec with Matchers {

  "Server Command Parser" should {
    "tokenize Goodbay command with one parameter" in {
      val cmd = ServerCommandParser.tokenize("Goodbay(energy=234)")
      cmd.optcode shouldBe "Goodbay"
    }
    "tokenize Welcome with multiple parameters" in {
      val cmd = ServerCommandParser.tokenize("Welcome(name=Agent,apocalypse=100,round=0,maxslaves=5)")
      cmd.optcode shouldBe "Welcome"
      cmd.params("name") shouldBe "Agent"
      cmd.params("maxslaves") shouldBe "5"
    }
    "parse Welcome command" in {
      val cmd = ServerCommandParser.parse("Welcome(name=Agent,apocalypse=100,round=0,maxslaves=5)")
      cmd shouldBe a [Welcome]
      cmd match {
        case x:Welcome =>
            x.apocalypse shouldBe 100
            x.maxSlaves shouldBe 5
            x.name shouldBe "Agent"
        case _ => fail("not a Welcome command object")
      }
    }
    "parse real input" in {
      val cmd = ServerCommandParser.parse("Welcome(name=BasiUlek,apocalypse=5000,round=14)")
    }
  }
}

