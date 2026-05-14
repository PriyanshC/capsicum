package example

import turbolift._
import turbolift.Extensions._
import language.experimental.captureChecking

sealed trait FnProducerSignature extends Signature {
    def produce(): (() => Unit) !! ThisEffect
}

trait FnProducerEffect extends Effect[FnProducerSignature] with FnProducerSignature:
    this: FnProducerEffect =>
    final override def produce() = perform(_.produce())

extension (px: FnProducerEffect)
    def noop = 
        new px.impl.Stateless[Identity, Option, Any] with px.impl.Sequential with FnProducerSignature {
            override def onReturn(a: Unknown) = !!.pure(Some(a))
            // override def produce() = !!.pure(() => ())
            override def produce() = {println("No-op function called!"); !!.pure(this.produce)} // Leak?
        }.toHandler

case object MyFnProducer extends FnProducerEffect
type MyFnProducer = MyFnProducer.type

object TurboLiftDemo extends App {
    val program: turbolift.Computation[() => Unit, example.MyFnProducer.type] = MyFnProducer.produce()
        
    val cont: Option[() => Unit] = program.handleWith(MyFnProducer.noop).run

    println(cont)
    val a = cont.map(_())
}
