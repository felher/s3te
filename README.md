
# Welcome to s3te, a scala 3 tree explorer

<kbd>
  
![firefox](https://github.com/user-attachments/assets/ae4f492a-c466-4242-84ee-08c3d6c94a65)

</kbd>

## Table of Contents

- [How to use](#how-to-use)
- [Motivation and Features](#motivation-and-features)

## How to use

Replace the `X` with the scala 3 minor version you are using, then add this to your `build.sbt` (notice the **single** percent sign):

```scala
libraryDependencies += "org.felher" % "s3te-compile_3.X" % "0.0.1"
```

If you are using scala 3.5.2, `X` should be `5`.

If the `X` you use is lower than your scala minor version, it should still work, but you might not see some stuff that was added to scala in the meantime.

If the `X` you use is higher than your scala minor version, it shouldn't work at all.

Then create a macro in a new file like this:
```scala
import scala.quoted.*

object MyMacro:
  inline def myMacro(inline a: Any): Any = ${ myMacroImpl('a) }

  def myMacroImpl(a: Expr[Any])(using Quotes): Expr[Any] =
    import quotes.reflect.*

    org.felher.s3te.S3te.renderTreeHTML("/tmp/tree.html", a.asTerm)

    '{ null }
```

and then call your macro in another with the tree you want to explore:
```scala
object MyMain:
  def main(): Unit =
    MyMacro.myMacro:
        val a = 3
        val b = 4
        a + b
```

Then compile your code in SBT. You should now be able to browse the HTML file in your browser using `file:///tmp/tree.html` as the URL.

## Motivation and Features

You can whatch this 5 to 6 minute video to see what s3te can do for you:

https://github.com/user-attachments/assets/943744db-5a84-48d9-8661-822881dcc91b
