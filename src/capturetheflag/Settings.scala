package capturetheflag

class UsageException(message: String) extends Exception(message)

case class Settings(
  help: Boolean = false,
  timeUnit: Int = 1000,
  timeout: Int = 30,
  bombTime: Int = 5,
  cleaningTime: Int = 5,
  foodTime: Int = 5,
  flagTime: Int = 5,
  bombs: Int = 10,
  foods: Int = 20,
  width: Int = 12,
  height: Int = 16,
  forceSize: Boolean = false,
  playersPerTeam: Int = 3,
  flagsPerTeam: Int = 3
) {
  def parse(args: List[String]): Settings = {
    if (args.isEmpty) this
    else {
      val option = args.head
      option match {
        case "--help" => copy(help = true).parse(args.tail)
        case "--forcesize" => copy(forceSize = true).parse(args.tail)
        case _ => {
          if (args.tail.isEmpty)
            throw new UsageException("Argument required for option " + option)

          val arg = try {
            args.tail.head.toInt
          } catch {
            case error: NumberFormatException =>
              throw new UsageException("Option " + option + " requires an int")
          }

          val tail = args.tail.tail

          val succ = option match {
            case "-u" | "--timeunit" => copy(timeUnit = arg)
            case "-t" | "--timeout" => copy(timeout = arg)
            case "-m" | "--bomtime" => copy(bombTime = arg)
            case "-c" | "--cleaningtime" => copy(cleaningTime = arg)
            case "-n" | "--foodtime" => copy(foodTime = arg)
            case "-k" | "--flagtime" => copy(flagTime = arg)
            case "-b" | "--bombs" => copy(bombs = arg)
            case "-f" | "--foods" => copy(foods = arg)
            case "-w" | "--width" => copy(width = arg)
            case "-h" | "--height" => copy(height = arg)
            case "-p" | "--playersperteam" => copy(playersPerTeam = arg)
            case "-g" | "--flagsperteam" => copy(flagsPerTeam = arg)
            case _ => throw new UsageException("Unknown option: " + option)
          }

          succ.parse(tail)
        }
      }
    }
  }
}
