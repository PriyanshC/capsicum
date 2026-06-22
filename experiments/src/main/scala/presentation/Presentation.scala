package capsicum.examples.presentation

import scala.language.experimental.captureChecking

import capsicum.core._
import capsicum.effects.{ConsoleCapability, StdConsoleHandler}

import com.sun.star.beans.XPropertySet
import com.sun.star.bridge.XUnoUrlResolver
import com.sun.star.comp.helper.Bootstrap
import com.sun.star.drawing.{XDrawPage, XShape, XShapes}
import com.sun.star.frame.XComponentLoader
import com.sun.star.lang.XComponent
import com.sun.star.text.XText
import com.sun.star.uno.{UnoRuntime, XComponentContext}
import com.sun.star.presentation.{XPresentation2, XPresentationPage, XPresentationSupplier, XSlideShowController}

import java.io.File
import scala.concurrent.duration.{Duration, DurationLong}
import scala.util.boundary

// soffice "--accept=socket,host=localhost,port=8100;urp;"

// Effect and capability template definitions

sealed trait PresentationEff[V] extends Effect[V]
case object Next extends PresentationEff[Unit]
case object Prev extends PresentationEff[Unit]
case object SlideInfo extends PresentationEff[(Int, String)]

trait PresentationCapability[R] extends Capability[PresentationEff, R, R] {
  final inline def nextSlide(inline resume: Unit => R): R = perform(Next)(resume)
  final inline def prevSlide(inline resume: Unit => R): R = perform(Prev)(resume)
  final inline def slideInfo(inline resume: ((Int, String)) => R): R = perform(SlideInfo)(resume)
}

sealed trait TimerEff[V] extends Effect[V]
case object Start extends TimerEff[Unit]
case object Current extends TimerEff[Duration]



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

  def getCurrentSlideNotes: String = {
    val drawPage: XDrawPage = getLiveController.getCurrentSlide()
    if (drawPage == null) return ""

    val presPage = query(drawPage, classOf[XPresentationPage])
    if (presPage == null) return ""
    
    val notesPage: XDrawPage = presPage.getNotesPage()
    if (notesPage == null) return ""
    
    val shapes = query(notesPage, classOf[XShapes])
    val notesBuilder = new StringBuilder()

    for (i <- 0 until shapes.getCount) {
      val shape = query(shapes.getByIndex(i), classOf[XShape])
      if (shape != null) {
        val shapeType = shape.getShapeType
        
        // Fallback check: It's explicitly a NotesShape OR an OutlineTextShape inside a notes page
        if (shapeType == "com.sun.star.presentation.NotesShape" || 
            shapeType == "com.sun.star.presentation.OutlinerShape") {
          
          val textObj = query(shape, classOf[XText])
          if (textObj != null && textObj.getString.trim.nonEmpty) {
            notesBuilder.append(textObj.getString).append("\n")
          }
        }
      }
    }

    if (notesBuilder.isEmpty) {
      for (i <- 0 until shapes.getCount) {
        val shape = query(shapes.getByIndex(i), classOf[XShape])
        if (shape != null) {
          val shapeType = shape.getShapeType
          if (!shapeType.contains("Header") && !shapeType.contains("Footer") && !shapeType.contains("DateTime")) {
            val textObj = query(shape, classOf[XText])
            if (textObj != null && textObj.getString.trim.nonEmpty) {
              notesBuilder.append(textObj.getString).append("\n")
            }
          }
        }
      }
    }

    notesBuilder.toString().trim
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
    case SlideInfo => (getLiveController.getCurrentSlideIndex(), getCurrentSlideNotes)
  }
}


class TimerCapability[R] extends Capability[TimerEff, R, R] with OneShotKeepResult[TimerEff, R] {
  private var startMillis: Option[Long] = None
  inline def start(inline resume: Unit => R) = perform(Start)(resume)
  inline def current(inline resume: Duration => R) = perform(Current)(resume)
  override protected def handleEff[V](eff: TimerEff[V]): V = eff match
    case Start => startMillis = Some(System.currentTimeMillis())
    case Current => startMillis.map((System.currentTimeMillis() - _)).getOrElse(0L).millis
}

// Main

def formatDuration(t: Duration): String = f"${t.toMinutes}%02d:${t.toSeconds % 60}%02d"

def deliverThesis(using pres: PresentationCapability[Unit], console: ConsoleCapability[Unit], timer: TimerCapability[Unit]): Unit = {
  def loop: Unit = {
    timer.current { t =>
      pres.slideInfo { (slide, notes) =>
        console.print(f"Slide $slide (${formatDuration(t)})\n$notes\n> ") { _ =>
          console.readLine { cmd => cmd.toLowerCase() match
            case "exit" | "quit"       => ()
            case "start"               => timer.start(_ => loop)
            case "back" | "prev" | "b" => pres.prevSlide(_ => loop)
            case "time" | "t"          => timer.current(tt => console.print(s"${{formatDuration(tt)}}\n")(_ => loop))
            case _                     => pres.nextSlide(_ => loop)
          }
        }
      }
    }
  }
  loop
}

@main def runPresentationController(args: String*): Unit = {
  val path = args.headOption.getOrElse("/home/pc/Downloads/MEng Presentation.pptx")
  val console = new StdConsoleHandler[Unit]
  val handler = new LibreOfficePresentationHandler[Unit](path)
  val timer = new TimerCapability[Unit]
  run(console, handler, timer)(deliverThesis)
}

