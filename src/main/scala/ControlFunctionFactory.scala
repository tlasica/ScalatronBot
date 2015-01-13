/**
 * Created by tomek on 11.01.15.
 */
class ControlFunctionFactory {
  def create = (input:String) => {
    try {
      val cmd = ServerCommandParser.parse(input)
    }
    catch {
      case _ => println(input)

    }

    //val cmd = ServerCommandParser.parse(input)
//    val resp = cmd match {
//      //case w: Welcome => "Status(text=Welcome!)"
//      //rcase r: React => s"Status(text=$r.time)"
//      case g: Goodbye => "Status(text=bye)"
//      case _ => "DUPA!"
//    }
    "Status(text=dupa)"
  }


}
