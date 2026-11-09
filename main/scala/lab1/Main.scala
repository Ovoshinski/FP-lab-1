package lab1

import java.time.format.DateTimeFormatter

object Main:

  private val dateFmt = DateTimeFormatter.ofPattern("d MMMM yyyy, HH:mm")

  def main(args: Array[String]): Unit =
    System.setOut(new java.io.PrintStream(System.out, true, "UTF-8"))
    System.setErr(new java.io.PrintStream(System.err, true, "UTF-8"))
    System.setProperty("file.encoding", "UTF-8")

    val configPath = if args.nonEmpty then args(0) else "config.json"
    val config = ConfigLoader.load(configPath)

    val errors = ConfigLoader.validate(config)
    if errors.nonEmpty then
      println("Ошибки конфигурации:")
      errors.foreach(e => println(s"  - $e"))
      println("Используется конфигурация по умолчанию.")
      println()
    val effectiveConfig = if errors.nonEmpty then AppConfig.default else config

    Logger.init(effectiveConfig.logFile)
    val engine  = new RuleEngine(effectiveConfig)
    val session = new SessionManager(effectiveConfig, os.pwd.toString)

    os.makeDir.all(os.Path(effectiveConfig.reportDirectory, os.pwd))
    os.makeDir.all(os.Path(effectiveConfig.sessionDirectory, os.pwd))

    //Проверка на наличие незавершенной сессии
    session.loadSessionFromDisk() match
      case Some(s) if s.status == "active" =>
        println("Обнаружена незавершённая сессия:")
        println(s"  Название: ${s.name}")
        println(s"  Начата:   ${s.startTime.format(dateFmt)}")
        println()
        println("Что сделать с этой сессией?")
        println("  [1] Продолжить (вернуться в неё)")
        println("  [2] Завершить и сохранить отчёт")
        println("  [3] Выключить программу (данные сохранятся)")
        println()

        var choice = ""
        while choice != "1" && choice != "2" && choice != "3" do
          print("Ваш выбор (1, 2 или 3): ")
          val input = scala.io.StdIn.readLine()
          if input == null then choice = "3"
          else
            val trimmed = input.trim
            if trimmed == "1" || trimmed == "2" || trimmed == "3" then choice = trimmed
            else println("Неверный выбор. Введите 1, 2 или 3.")

        choice match
          case "1" =>
            session.resumeSession(s)
            println("\nСессия восстановлена.\n")
          case "2" =>
            session.resumeSession(s)
            session.endSession() match
              case Right(finished) =>
                val md = ReportGenerator.generate(finished, effectiveConfig, engine)
                val outDir = os.Path(effectiveConfig.reportDirectory, os.pwd)
                os.makeDir.all(outDir)
                val safeName = finished.name.replaceAll("[^a-zA-Zа-яА-Я0-9_-]", "-").toLowerCase
                val file = outDir / s"$safeName.md"
                os.write.over(file, md)
                println("\nСессия завершена.")
                println(s"Отчёт сохранён: ${file.relativeTo(os.pwd)}\n")
              case Left(_) => ()
          case "3" =>
            println("До свидания!")
            Logger.close()
            return
          case _ => ()
      case _ => ()

    //Заголовок
    println("====================================================")
    println("Лабораторная работа - 1: ADT и функциональный стиль")
    println("Вариант 9")
    println("====================================================")
    println()
    
    println(Instructions.getText(Nil))

    //Главный цикл
    var running = true
    while running do
      print("report-shell> ")
      val line = scala.io.StdIn.readLine()

      if line == null then running = false
      else
        val trimmed = line.trim
        if trimmed.nonEmpty then

          //1. Парсим строку в типизированную команду
          val parsed: Either[AppError, Command] = CommandParser.parse(trimmed)

          //2. Обрабатываем команду. Используем flatMap (монаду) для связывания Парсера и Хендлера
          val result: Either[AppError, CommandResult] = parsed.flatMap { cmd =>
            if cmd == Exit then
              running = false
              Right(SilentSuccess)
            else
              CommandHandler.handle(cmd, session, effectiveConfig, engine)
          }

          //3. Выводим результат (взаимодействие с UI)
          result match
            case Right(TextOutput(text)) => println(text); println()
            case Right(ExecResult(stdout, stderr, code)) =>
              if stdout.nonEmpty then print(stdout)
              if stderr.nonEmpty then System.err.print(stderr)
              if code != 0 then println(s"\nКоманда завершилась с кодом $code.")
              println()
            case Right(SilentSuccess) => ()
            case Left(err) => println(formatError(err)); println()

    Logger.close()
    println("До новых запусков программы!")
  
  private def formatError(err: AppError): String = err match
    case DirectoryNotFound(p) => s"Ошибка: Каталог не найден - $p"
    case SessionAlreadyActive => "Ошибка: Сессия уже активна. Завершите её командой ends."
    case SessionNotActive => "Ошибка: Нет активной сессии."
    case UnknownCommand(c) => s"Ошибка: Неизвестная команда - $c"
    case InvalidArguments(c, m) => s"Ошибка аргументов для '$c': $m"
    case ExecutionFailed(m) => s"Ошибка выполнения: $m"
    case ParseError(m) => s"Ошибка разбора: $m"