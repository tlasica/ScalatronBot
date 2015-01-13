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
    val paramPairs = tokens.drop(1) map ((x: String) => x.split("="))
    val paramsMap = paramPairs groupBy(_(0)) mapValues (_(0)(1))
    ParsedCommand(optcode, paramsMap)
  }

  private def parseWelcome(cmd: ParsedCommand): ServerCommand = {
    Welcome(
      name = cmd.getString("name"),
      apocalypse = cmd.getInt("apocalypse"),
      round = cmd.getInt("round"),
      maxSlaves = cmd.getInt("maxslaves") )
  }

  private def parseReact(cmd: ParsedCommand): ServerCommand = {
    React(
      generation = cmd.getInt("generation"),
      name = cmd.getString("name"),
      time = cmd.getInt("time"),
      view = cmd.getString("view"),
      energy = cmd.getInt("energy"),
      masterPos = cmd.getCoord("master"),
      collision = cmd.getCoord("collision"),
      slavesAlive = cmd.getInt("slaves"),
      state = cmd.params
    )
  }

  private def parseGoodbye(cmd: ParsedCommand): ServerCommand = {
    Goodbye(energy = cmd.getInt("energy"))
  }
}
