import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.core.FileAppender

import static ch.qos.logback.classic.Level.INFO

appender("FILE", FileAppender) {
  file = "target/logs/application.log"
  encoder(PatternLayoutEncoder) {
    pattern = "%date - [%level] - from %logger in %thread %n%message%n%xException%n"
  }
}

logger("application", INFO, ["FILE"])
root(INFO, ["FILE"])

