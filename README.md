# Running

Using the scala REPL, you can run the following example:

```scala
import com.oomagnitude._

val model = train(10000)
inferWord("the", model)
```

## Run the REPL using Gradle

Build the cla project:

`./gradlew clean build test`

Then, run scalaConsole:

`./gradlew -q scalaConsole`

## Run the REPL using IntelliJ

Add a new run configuration of type 'Scala Console' and make sure it uses the classpath of this project. When using the
IntelliJ REPL, you will need to type cmd+enter in order to send a command to the REPL.
