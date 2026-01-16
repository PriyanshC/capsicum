package example

// trait Producer[T] {
//   def produce(): T
// }

// trait Handler[H]

// object CapsicumMain {
//   def produceAndCallN(n: Int)(using funProducer: Handler[Producer[() => Unit]]): Unit = {
//     val f = funProducer.produce()
//     (1 to n).foreach(_ => f())
//   }
  
//   val unsafeHandler: Handler[Producer[() => Unit]] = { resume =>
//     println("Handler called!")
//     resume(resume) // Leak!!
//   }
// }
