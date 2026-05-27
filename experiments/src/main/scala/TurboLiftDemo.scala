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
    def noop: () => Handler[[A] =>> Identity[A], Option, px.type, Any]^{caps.fresh} = 
        () => new px.impl.Stateless[Identity, Option, Any] with px.impl.Sequential with FnProducerSignature with caps.SharedCapability {
            override def onReturn(a: Unknown) = !!.pure(Some(a))
            // override def produce() = !!.pure(() => ())
            override def produce() = {println("No-op function called!"); !!.pure(this.produce)} // Leak?
        }.toHandler

case object MyFnProducer extends FnProducerEffect
type MyFnProducer = MyFnProducer.type

object TurboLiftDemo extends App {

    val cont: () => Unit = {
        val h: Handler[[A] =>> Identity[A], Option, MyFnProducer.type, Any]^ = MyFnProducer.noop()
        val program: turbolift.Computation[() => Unit, example.MyFnProducer.type] = MyFnProducer.produce()
        program.handleWith(h).run.get
    }

    println("Exec done!")
    println(cont)
    val a = cont()
}
