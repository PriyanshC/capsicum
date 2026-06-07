// package capsicum.examples


// import capsicum.core._
// import capsicum.effects._
// import language.experimental.captureChecking

// import java.util.zip.CRC32
// object Exports {
//   def chunkSize1 = 2048 - 16
//   def chunkSize2 = 512 - 16
//   def chunkSize3 = 128 - 16
//   def chunkSize4 = 64 - 16
//   def byteCount = 20000
//   val bytes = scala.util.Random(13370042).nextBytes(byteCount)

//   val theSeq1: IndexedSeq[Array[Byte]] = bytes.grouped(chunkSize1).map(_.toArray).toIndexedSeq
//   val theSeq2: IndexedSeq[Array[Byte]] = bytes.grouped(chunkSize2).map(_.toArray).toIndexedSeq
//   val theSeq3: IndexedSeq[Array[Byte]] = bytes.grouped(chunkSize3).map(_.toArray).toIndexedSeq
//   val theSeq4: IndexedSeq[Array[Byte]] = bytes.grouped(chunkSize4).map(_.toArray).toIndexedSeq
//   val theSeq5: IndexedSeq[Byte] = bytes.toIndexedSeq

//   def asciiToLower(b: Byte): Byte = if 65 <= b && b <= 90 then (b | 0x20).toByte else b
//   def makeAccum() = new CRC32
//   extension (acc: CRC32)
//     def ++ (bytes: Array[Byte]): CRC32 = { acc.update(bytes); acc }
//     def :+ (byte: Byte): CRC32 = { acc.update(byte); acc }
// }

// object Crc {
//   import Exports._

//   def consume1(stream: SafeChunkedChainedStream[Byte]): Long =
//     stream
//       .filter(_ < 128)
//       .map(asciiToLower)
//       .foldChunks(makeAccum())((acc, chunk) => acc ++ chunk.elements)
//       .getValue()

//   def consume2(stream: SafeChainedStream[Byte]): Long =
//     stream
//       .filter(_ < 128)
//       .map(asciiToLower)
//       .fold(makeAccum())(_ :+ _)
//       .getValue

//   def prechunked(elementss: IndexedSeq[Array[Byte]]): SafeChunkedChainedStream[Byte] =
//     SafeChunkedChainedStream.fromPrechunked(elementss)

//   def unchunked(elements: IndexedSeq[Byte]): SafeChainedStream[Byte] =
//     SafeChainedStream.fromSeq(elements)


//   def round1 = consume1(prechunked(theSeq1))
//   def round2 = consume1(prechunked(theSeq2))
//   def round3 = consume1(prechunked(theSeq3))
//   def round4 = consume1(prechunked(theSeq4))
//   def round5 = consume2(unchunked(theSeq5))
// }

