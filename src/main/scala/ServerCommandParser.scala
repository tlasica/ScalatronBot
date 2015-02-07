/**
 * Created by tomek on 12.01.15.
 */
object ServerCommandParser {

  case class ParsedCommand(optcode: String, params:Map[String, String]) {
    def getInt(key: String) = params.getOrElse(key, "0").toInt
    def getString(key: String) = params(key)
    def getCoord(key: String): Coord = {
      val xy = params(key).split(":")
      Coord(xy(1).toInt, xy(2).toInt)
    }
  }

  def parse(input: String): ServerCommand = {
    val parsedCmd = tokenize(input)
    parsedCmd.optcode match {
      case "Welcome" => parseWelcome(parsedCmd)
      case "React" => parseReact(parsedCmd)
      case "Goodbye" => parseGoodbye(parsedCmd)
    }
  }

  def tokenize(input: String): ParsedCommand = {
    val tokens = input.split("\\(|\\)|\\,")
    val optcode = tokens(0)
    val params = tokens.toList drop 1
    val paramPairs = params map ((x: String) => x.split("="))
    val paramsMap = paramPairs groupBy(_(0)) mapValues (_(0)(1))
    ParsedCommand(optcode, paramsMap)
  }

  private[this] def parseWelcome(cmd: ParsedCommand): ServerCommand = {
    WelcomeCmd(
      name = cmd.getString("name"),
      apocalypse = cmd.getInt("apocalypse"),
      round = cmd.getInt("round"),
      maxSlaves = cmd.getInt("maxslaves") ) // TODO: do poprawienia na Option
  }

  private[this] def parseReact(cmd: ParsedCommand): ServerCommand = {
    ReactCmd( cmd.params )
  }

  private[this] def parseGoodbye(cmd: ParsedCommand): ServerCommand = {
    GoodbyeCmd(energy = cmd.getInt("energy"))
  }
}
