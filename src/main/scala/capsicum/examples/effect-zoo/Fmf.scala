// package capsicum.examples

// import scala.language.experimental.captureChecking

// import capsicum.core._
// import capsicum.effects._

// object Fmf {
//   def theSeq: Seq[Int] = Seq(1, 5)

//   def round1 = {
//     SafeChainedStream.fromSeq(Fmf.theSeq)
//       .filter(_ % 2 == 0)
//       .map(_ + 1)
//       .fold(0)(_ + _)
//   }
// }

