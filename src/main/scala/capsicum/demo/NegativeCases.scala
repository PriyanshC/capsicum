package capsicum.demo

import scala.language.experimental.captureChecking
import capsicum._


lazy object ContinuationLeakDemo {
    sealed trait ProduceOp[A] extends Effect { type V = A }

    object ProduceOp {
        case class GetValue[A]() extends ProduceOp[A]
    }

    trait ProducerCapability[A, R] extends Capability[ProduceOp[A], R, R] {
        def produce(resume: A -> R): R = perform(ProduceOp.GetValue(), resume)
    }

    class GoodHandler[R] extends ProducerCapability[Unit -> Unit, R] {
      override def perform(op: ProduceOp[Unit -> Unit], resume: op.V => R): R = op match
        case ProduceOp.GetValue() => {
            println("Good handler invoked!")
            resume(_ => ())
        }
    }

    class UnsafeHandler[R] extends ProducerCapability[Unit -> Unit, R] {
      override def perform(op: ProduceOp[Unit -> Unit], resume: op.V => R): R = op match
        case ProduceOp.GetValue() => {
            // Note: Removing {resume} yields the same error as below, but now earlier
            val leakingInner: Unit ->{resume} Unit = { (_: Unit) =>
                resume(_ => ())
                println("Unsafe handler invoked!")
            }

            // ERROR: Capability `resume` cannot flow into capture set {}
            // resume(leakingInner)

            ???
        }
      }

    def program(n: Int): ProducerCapability[Unit -> Unit, Unit] ?-> Unit = {
        val r = summon[ProducerCapability[Unit -> Unit, Unit]].perform(ProduceOp.GetValue(), { (f: Unit -> Unit) => 
            (1 to n).foreach(_ => f)
        })
    }

    val result1 = program(3)(using new GoodHandler[Unit])
}

  // ERROR: (only if resume is defined as capturing, is this right?)
  // val badHandler: FnProducer = Handler({_ => { resume => 
  //   println("Unsafe handler invoked!")
  //   resume(() => {
  //     resume(() => ())
  //   })
  // }})

  // ERROR: Resume captures resume
  // val badHandler: FnProducer = new Handler {
  //   override def handle(x: Unit, resume: (() -> Unit) => Unit): Unit = {
  //     println("Unsafe handler invoked!")
  //     resume(() => {
  //       resume(() => ())
  //     })
  //   }
  // }
//   val result: Unit = run(goodHandler)(program(5))
// }

// lazy object SmuggledHandlerDemo {
//   type IntProducer = Handler[Unit, Int, Unit, Unit]

//   var handlerStorage: Option[IntProducer^] = None

//   val goodHandler: IntProducer^ = Handler({_ => { resume => resume(5)}})

//   val naughtyProgram: IntProducer ?-> Unit = {
//     val h: IntProducer^ = summon[IntProducer^]
//     // handlerStorage = Some(h) // ERROR: Leak the handler itself
//   }

//   val result = run(goodHandler)(naughtyProgram)
//   // val smuggledHandler: IntProducer = handlerStorage.get // Use the handler
// }

// lazy object SmuggledHandlerFnDemo {
//   type IntProducer = Handler[Unit, Int, Unit, Unit]

//   var fnStorage: Option[() => Unit] = None

//   val goodHandler: IntProducer = Handler({_ => { resume => resume(5)}})

//   val naughtyProgram: IntProducer ?-> Unit = {
//     val h = summon[IntProducer]
//     // fnStorage = Some(() => h.handle((), _ => ())) // ERROR: Leak the handle function
//   }

//   val result = run(goodHandler)(naughtyProgram)
//   // val smuggledFn = fnStorage.get
//   // smuggledFn()
// }

