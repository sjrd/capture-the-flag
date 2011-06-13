package capturetheflag

object Main {
  def main(args: Array[String]) {
    try {
      val settings = Settings().parse(args.toList)

      if (settings.help) {
        printUsage()
      } else {
        val master = new Master(settings)
        master.start()
        new Observer(master).observe()
      }
    } catch {
      case error: UsageException =>
        println(error.getMessage)
    }
  }

  def printUsage() {
    println("Usage:")
    println("  capturetheflag.Main [OPTIONS]")
  }
}
