/**
 * Created by tomek on 12.01.15.
 */
object ServerCommandParser {

  case class ParsedCommand(optcode: String, params:Map[String, String])

  def parse(input: String): ServerCommand = {
    val parsedCmd = tokenize(input)
    parsedCmd.optcode match {
      case "Welcome" => parseWelcome(parsedCmd)
      case "React" => parseReact(parsedCmd)
      case "Goodbye" => parseGoodbye(parsedCmd)
    }
  }

  def tokenize(input: String): ParsedCommand = {
    val tokens = input.split("(,)")
    val optcode = tokens(0)
    val paramPairs = tokens.drop(1) map ((x: String) => x.split("="))
    val paramsMap = paramPairs groupBy(_(0)) mapValues (_(0)(1))
    ParsedCommand(optcode, paramsMap)
  }

  private def parseWelcome(command: ParsedCommand): ServerCommand = ???

  private def parseReact(command: ParsedCommand): ServerCommand = ???

  private def parseGoodbye(command: ParsedCommand): ServerCommand = ???
}
