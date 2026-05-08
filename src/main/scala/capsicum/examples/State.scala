package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

def basicState(): Int = {
  val handler = new MutableStateHandler[Int, Int](10)
  
  def prog(using StateCapability[Int, Int]): Int = {
    val handler = summon[StateCapability[Int, Int]]
    handler.get { s1 =>
      handler.put(s1 + 5, { _ =>
        handler.get { s2 =>
          s2
        }
      })
    }
  }
  
  handler.run(prog)
}

def trackedState(): Unit = {
  class FileSystem

  class Logger(fs: FileSystem^) {
    def log(s: String): Unit = println(s"Pretending to ${s}")
  }

  val fs: FileSystem^ = new FileSystem
  val logger: Logger^{fs} = new Logger(fs)
  val handler: StateCapability[Logger^{fs}, Unit] = new MutableStateHandler(logger)

  def progWithScopedCapture(using state: StateCapability[Logger^{fs}, Unit]): Unit = {
    state.get{ (logger: Logger^{fs}) =>
      logger.log("Hi from scoped captured state")
      
      val newLogger: Logger^{fs} = new Logger(fs)
      state.put(newLogger, { _ =>
        println("Logger updated!")
      })
    }
  }

  def progWithPolymorphicCapture[C^](using state: StateCapability[Logger^{C}, Unit]): Unit = {
    state.get{ (logger: Logger^{C}) =>
      logger.log("Hi from polymorphic state")
      
      // Using a new tracked FS wouldn't be ok, we don't know what C^ includes
      val untrackedFs: FileSystem = new FileSystem
      val newLogger: Logger^{} = new Logger(untrackedFs)
      state.put(newLogger, { _ =>
        println("Logger updated!")
      })
    }
  }

  handler.run(progWithScopedCapture)
  handler.run(progWithPolymorphicCapture)
}
