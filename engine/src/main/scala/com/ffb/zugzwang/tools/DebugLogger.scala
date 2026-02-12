package com.ffb.zugzwang.tools

import java.io.{FileWriter, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object DebugLogger:

  private val logFile   = new PrintWriter(new FileWriter("/tmp/chess-engine-debug.log", true))
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")

  def log(message: String): Unit =
    val timestamp = LocalDateTime.now().format(formatter)
    logFile.println(s"[$timestamp] $message")
    logFile.flush()

  def close(): Unit =
    logFile.close()
