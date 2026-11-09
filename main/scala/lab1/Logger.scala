package lab1

import java.io.{FileWriter, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

//Технический журнал. Пишет события в файл, дописывая в конец
object Logger:

  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
  private var writer: Option[PrintWriter] = None


  //Открывает файл. Если родительских папок нет — создаёт их
  def init(logFile: String): Unit =
    try
      val f = new java.io.File(logFile)
      Option(f.getParentFile).foreach(_.mkdirs())
      writer = Some(new PrintWriter(new FileWriter(f, true)))
      info("Приложение запущено")
    catch
      case e: Exception =>
        println(s"Не удалось открыть журнал: ${e.getMessage}")


  def info(msg: String): Unit  = log("INFO", msg)
  def warn(msg: String): Unit  = log("WARN", msg)
  def error(msg: String): Unit = log("ERROR", msg)


  //Формирует строку "[дата время] сообщение" и пишет в файл
  private def log(level: String, msg: String): Unit =
    val ts = LocalDateTime.now().format(formatter)
    val line = s"[$ts] $level $msg"
    writer.foreach { w =>
      w.println(line)
      w.flush()
    }


  def close(): Unit =
    writer.foreach { w =>
      info("Приложение завершено.")
      w.close()
    }
    writer = None