# Welcome to s3te, a Scala 3 tree explorer

<kbd>
  
![firefox](https://github.com/user-attachments/assets/ae4f492a-c466-4242-84ee-08c3d6c94a65)

</kbd>

## Table of Contents

- [How to use](#how-to-use)
- [Motivation and Features](#motivation-and-features)
- [Binary and source compatibility guarantees](#binary-and-source-compatibility-guarantees)
- [Contributions and issues](#contributions-and-issues)

## How to use

### Short version, I know what I'm doing

Replace the `X` with the Scala 3 minor version you are using, then add this to
your `build.sbt` (notice the **single** percent sign):

```scala
libraryDependencies += "org.felher" % "s3te-compile_3.X" % "0.0.2" % Compile
```

and then call `org.felher.s3te.S3te.renderTreeHTML("/tmp/out.html", expr.asTerm)`
in your macro to generate a tree of the expression `expr` in the file
`/tmp/out.html`.

### Long version, how do I inspect some code I have?

#### 1. Update your build.sbt
Replace the `X` with the Scala 3 minor version you are using, then add this to
your `build.sbt` (notice the **single** percent sign):

```scala
libraryDependencies += "org.felher" % "s3te-compile_3.X" % "0.0.2" % Compile
```

If you are using Scala 3.5.2, `X` should be `5`.

> [!TIP]
> You should be able to user an older versions of this library to inspect code
> of a newer Scala version, though some new stuff added to Scala in the
> meantime might not show up. So if s3te is not yet published for your
> cutting-edge Scala version, try the minor version of Scala release before
> that.
> 
> The reverse is not true, though. You can't use a newer version of this
> library with an older version of Scala, which means, currently, s3te is only
> supported on Scala 3.3+.

#### 2. Create a macro file
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

#### 3. Call the macro
and then call your macro in another file with the tree you want to explore:
```scala
object MyMain:
  def main(): Unit =
    MyMacro.myMacro:
        // your code here
        val a = 3
        val b = 4
        a + b
```

#### 4. Compile code and view tree

Then compile your code in SBT. You should now be able to browse the HTML file
in your browser using `file:///tmp/tree.html` as the URL.

## Motivation and Features

Here is a quick list of the features of s3te:

- hover over code to highlight associated tree (and vice versa)
- click on code to scroll tree into view (and vice versa)
- click multiple times to select successively larger spans of code or trees
- drill into part of the tree by clicking the "tree" icon right of the name of a node
- collapse/expand parts of the tree by clicking the arrow icon left of the name of a node
- hover over the name of a node on the tree to show a small description (short scaladocs)
- click on the info icon right of the name of a node to show a detailed description of the node, including:
  - larger description (large scaldocs)
  - overview of the type hierarchy of the node in question
  - links to all the scala docs (methods and modules) for any node in the hierarchy, including the node in question
  - view the symbol, as well as all the flags of the symbol, with a description of each flag
  - view the type of the node (TypeRepr from .tpe) if it has an associated type, including cycle detection for self-referential types
  - see generated code to copy into your macro that extracts the node in question from the root of the tree you've given to the explorer

You can also watch this 5 to 6 minute video for a more visual explanation:

https://github.com/user-attachments/assets/943744db-5a84-48d9-8661-822881dcc91b

## Binary and source compatibility guarantees

Currently, basically none. I mean, I won't break api just for the fun of it,
but the purpose of this library is mainly to debug and explore Scala code. I
don't expect anyone to keep this library around. If you would like some
guarantees, please open an issue to discuss it.

## Contributions and issues

If you are missing some feature or think something is wrong, please open an
issue. I still consider this library beta status! Of course, pull requests are
also very welcome.

## Acknowledgements

Big thanks to the Scala 3 team for making this possible. The docs are mostly
copied from the Scala 3 docs and the implementation is heavily inspired by the
Scala 3 code base as well.
