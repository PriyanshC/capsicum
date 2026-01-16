package example

import turbolift._
import turbolift.Extensions._
// import language.experimental.captureChecking

sealed trait TLProducerSignature extends Signature {
    def produce(): (() => Unit) !! ThisEffect
}

trait TLProducerEffect extends Effect[TLProducerSignature] with TLProducerSignature:
    final override def produce() = perform(_.produce())

extension (px: TLProducerEffect)
    def noop = 
        new px.impl.Stateless[Identity, Option, Any] with px.impl.Sequential with TLProducerSignature {
            override def onReturn(a: Unknown) = !!.pure(Some(a))
            // override def produce() = !!.pure(() => ())
            override def produce() = {println("No-op function called!"); !!.pure(this.produce)} // Leak?
        }.toHandler

case object MyTLProducer extends TLProducerEffect
type MyTLProducer = MyTLProducer.type

object TurboLiftDemo extends App {
    val program =
        for
            f <- MyTLProducer.produce()
        yield (f)
        
    val cont = program.handleWith(MyTLProducer.noop).run

    println(cont)
    val a = cont.map(_())
}
