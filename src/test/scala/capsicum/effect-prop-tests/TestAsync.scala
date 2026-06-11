package capsicum.test.effects

import capsicum.core._
import capsicum.effects._
import org.scalacheck.{Arbitrary, Cogen, Properties, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}
import scala.reflect.ClassTag
import scala.concurrent.ExecutionContext

abstract class AsyncLaws[A: Arbitrary : Cogen : ClassTag, B: Arbitrary : ClassTag](
  newAsync: [R] => () => AsyncCapability[R]
)(implicit ctagA: ClassTag[A], ctagB: ClassTag[B]) 
  extends Properties(s"AsyncLaws[${ctagA.runtimeClass.getSimpleName}, ${ctagB.runtimeClass.getSimpleName}]") {

  property("Fork-Join") = forAll { (value: A, f: A => B) =>
    val async = newAsync[Boolean]()
    val pureResult = f(value)
    
    async.fork(() => f(value)) { fiber =>
      async.join(fiber) { asyncResult =>
        asyncResult == f(value)
      }
    }
  }

  property("Join-Join is idempotent") = forAll { (value: A) =>
    val async = newAsync[Boolean]()
    async.fork(() => value) { fiber =>
      async.join(fiber) { res1 =>
        async.join(fiber) { res2 =>
          res1 == res2
        }
      }
    }
  }

  property("Fork Commutativity") = forAll { (valA: A, valB: B) =>
    val async1 = newAsync[(A, B)]()
    val result1 = async1.fork(() => valA) { fibA =>
      async1.fork(() => valB) { fibB =>
        async1.join(fibA) { resA =>
          async1.join(fibB) { resB =>
            (resA, resB)
          }
        }
      }
    }

    val async2 = newAsync[(A, B)]()
    val result2 = async2.fork(() => valB) { fibB =>
      async2.fork(() => valA) { fibA =>
        async2.join(fibA) { resA =>
          async2.join(fibB) { resB =>
            (resA, resB)
          }
        }
      }
    }

    result1 == result2
  }

  property("Join Commutativity") = forAll { (valA: A, valB: B) =>
    // Join A, then Join B
    val async1 = newAsync[(A, B)]()
    val result1 = async1.fork(() => valA) { fibA =>
      async1.fork(() => valB) { fibB =>
        async1.join(fibA) { resA =>
          async1.join(fibB) { resB =>
            (resA, resB)
          }
        }
      }
    }

    // Join B, then Join A
    val async2 = newAsync[(A, B)]()
    val result2 = async2.fork(() => valA) { fibA =>
      async2.fork(() => valB) { fibB =>
        async2.join(fibB) { resB =>
          async2.join(fibA) { resA =>
            (resA, resB)
          }
        }
      }
    }

    result1 == result2
  }
}


abstract class VirtualAsyncSpec[A : Arbitrary : Cogen : ClassTag, B : Arbitrary : ClassTag] extends AsyncLaws[A, B](
  [R] => () => {
    given ExecutionContext = ExecutionContext.global
    new VirtualAsyncHandler[R]
  }
)

object IntStringVirtualAsync extends VirtualAsyncSpec[Int, String]
object DoubleBooleanVirtualAsync extends VirtualAsyncSpec[Double, Boolean]
