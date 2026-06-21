package capsicum.examples.presentation

import scala.language.experimental.captureChecking

import capsicum.core._
import capsicum.effects.{ConsoleCapability, StdConsoleHandler}

import com.sun.star.bridge.XUnoUrlResolver
import com.sun.star.comp.helper.Bootstrap
import com.sun.star.frame.XComponentLoader
import com.sun.star.lang.XComponent
import com.sun.star.presentation.{XPresentation2, XPresentationSupplier, XSlideShowController}
import com.sun.star.uno.{UnoRuntime, XComponentContext}
import java.io.File

// Effect and capability template definitions

sealed trait PresentationEff[V] extends Effect[V]
type Presentation = [V] =>> PresentationEff[V]

case object Next extends PresentationEff[Unit]
case object Prev extends PresentationEff[Unit]

trait PresentationCapability[R] extends Capability[Presentation, R, R] {
  final inline def nextSlide(inline resume: Unit => R): R = 
    perform(Next)(resume)
    
  final inline def prevSlide(inline resume: Unit => R): R = 
    perform(Prev)(resume)
}

// Live instance

class LibreOfficePresentationHandler[R](path: String, host: String = "localhost", port: Int = 8100) extends PresentationCapability[R] with OneShotKeepResult[PresentationEff, R] {

  private var document: XComponent = scala.compiletime.uninitialized
  private var presentation: XPresentation2 = scala.compiletime.uninitialized

  def loadPresentation(presentationFile: File): Unit = {
    val localContext = Bootstrap.createInitialComponentContext(null)
    val localServiceManager = localContext.getServiceManager
    
    val urlResolver = query(
      localServiceManager.createInstanceWithContext("com.sun.star.bridge.UnoUrlResolver", localContext),
      classOf[XUnoUrlResolver]
    )
    
    val unoUrl = s"uno:socket,host=$host,port=$port;urp;StarOffice.ComponentContext"
    val remoteContext = query(urlResolver.resolve(unoUrl), classOf[XComponentContext])
    val remoteServiceManager = remoteContext.getServiceManager

    val desktop = remoteServiceManager.createInstanceWithContext("com.sun.star.frame.Desktop", remoteContext)
    val componentLoader = query(desktop, classOf[XComponentLoader])
    
    val fileUrl = presentationFile.toURI.toString
    document = componentLoader.loadComponentFromURL(fileUrl, "_blank", 0, Array())
    
    if (document == null) throw new RuntimeException("No document")

    val supplier = query(document, classOf[XPresentationSupplier])
    presentation = query(supplier.getPresentation(), classOf[XPresentation2])
  }

  loadPresentation(File(path))
  presentation.start()

  private def query[T](obj: Any, clazz: Class[T]): T = {
    UnoRuntime.queryInterface(clazz, obj)
  }

  private def getLiveController: XSlideShowController = {
    if (presentation == null || !presentation.isRunning) {
      throw new IllegalStateException("Slideshow not actively running")
    }
    presentation.getController()
  }

  // Capsicum API
  override protected def handleEff[V](eff: PresentationEff[V]): V = eff match {
    case Next => getLiveController.gotoNextEffect()
    case Prev => getLiveController.gotoPreviousEffect()
  }
}

// Main

def deliverThesis(using pres: PresentationCapability[Unit], console: ConsoleCapability[Unit]): Unit = {
  def loop(slide: Int): Unit = {
    console.print(s"Slide $slide\n> ") { _ =>
      console.readLine { cmd => cmd.toLowerCase() match
        case "exit" | "quit" => ()
        case "back" | "prev" | "b" => pres.prevSlide(_ => loop(slide - 1))
        case _ => pres.nextSlide(_ => loop(slide + 1))
      }
    }
  }
  loop(1)
}

@main def runPresentationController(args: String*): Unit = {
  val path = args.headOption.getOrElse("/home/pc/Downloads/MEng Presentation.pptx")
  val console = new StdConsoleHandler[Unit]
  val handler = new LibreOfficePresentationHandler[Unit](path)
  run(console, handler)(deliverThesis)
}

