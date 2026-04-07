// package example

// import scala.language.experimental.captureChecking
// import java.io.FileOutputStream
// import java.io.OutputStream


// class MyFileOutputStream(name: String) extends FileOutputStream(name) {
//     override def close(): Unit = {
//         val x: FileOutputStream^{this} = this
//         super.close()
//     }
// }

// object Filesys extends App {
//     def processFile[T](name: String)(processor: OutputStream^ => T): T = {
//         val out = new MyFileOutputStream(name)
//         val result = processor(out)
//         out.close()
//         result
//     }

//     // def nastee: Unit = {
//     //     val nasty = processFile("log.txt"): 
//     //         out => (i: Int) => out.write(i)
//     //     nasty(1)
//     // }

//     // nastee
// }


// object Main extends App {
//   def echo(using c: ConsoleEffects): Unit = {
//     val str = c.getLine()
//     if (!str.isEmpty()) {
//       c.putStrLn(str)
//       echo(using c)
//     }
//   }

  


// }


// trait ConsoleEffects extends Effek {
//   def getLine(): String
//   def putStrLn(str: String): Unit
// }