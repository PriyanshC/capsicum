package capsicum.examples

import capsicum.core._
import capsicum.effects._
import language.experimental.captureChecking

def basicMutableState(): Int = {
  val mutableHandler = new MutableStateHandler[Int, Int](10)
  
  def prog(using state: StateCapability[Int, Int]): Int = {
    state.get { s1 =>
      state.put(s1 + 5) { _ =>
        state.get { s2 =>
          s2
        }
      }
    }
  }
  
  mutableHandler.run(prog)
}

// def basicPureState(): Int = {
//   val pureHandler = new PureStateCapability[Int, Int]
  
//   def prog(using state: PureStateCapability[Int, Int]): Int ->{state} (Int, Int) = {
//     state.get { s1 =>
//       state.put(s1 + 5) { _ =>
//         state.get { s2 =>
//           (currentState: Int) => (currentState, s2)
//         }
//       }
//     }
//   }

//   val stateFn: Int -> (Int, Int) = pureHandler.run(prog)
//   val (finalState, result) = stateFn(10)
//   result
// }

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
      state.put(newLogger) { _ =>
        println("Logger updated!")
      }
    }
  }

  def progWithPolymorphicCapture[C^](using state: StateCapability[Logger^{C}, Unit]): Unit = {
    state.get{ (logger: Logger^{C}) =>
      logger.log("Hi from polymorphic state")
      
      // Using a new tracked FS wouldn't be ok, we don't know what C^ includes
      val untrackedFs: FileSystem = new FileSystem
      val newLogger: Logger^{} = new Logger(untrackedFs)
      state.put(newLogger) { _ =>
        println("Logger updated!")
      }
    }
  }

  handler.run(progWithScopedCapture)
  handler.run(progWithPolymorphicCapture)
}


def compareMutablePureBacktracking(): Unit = {
  type F = Int -> Int
  case class Choose[V](choices: Seq[V]) extends Effect[V]
  class ThreadedAmbCapability extends Capability[Choose, F, F] {
    final def choose[V](choices: Seq[V]): (resume : V => F) => F^{resume} = perform(Choose(choices))
    override def perform[V](eff: Choose[V])(resume: V => F): F^{resume} = {
      (initialState: Int) => {
        eff.choices.foldLeft((initialState)) { case (currentState, choice) =>
          resume(choice)(currentState)
        }
      }
    }
  }

  inline def progPure(using amb: ThreadedAmbCapability, state: PureStateCapability[Int, Id]): Int = {
    val fn = amb.perform(Choose(Seq("Heads", "Tails"))) { flip =>
      state.update(_ + 1) { _ =>
        if (flip == "Heads") {
          amb.perform(Choose(Seq("Heads", "Tails"))) { _ =>
            state.update(_ + 1) { _ =>
              state.get(_ => currentState => currentState)
            }
          }
        } else {
          state.get(_ => currentState => currentState)
        }
      }
    }
    fn(0)
  }

  val state = new PureStateCapability[Int, Id]
  val amb = new ThreadedAmbCapability

  println(state.run(amb.run(progPure)))
}

