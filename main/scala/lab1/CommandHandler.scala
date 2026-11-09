package lab1

import java.time.LocalDateTime

//Обработчик бизнес-логики. НЕ общается с пользователем напрямую (никаких println).
object CommandHandler:

  //Возвращаем Either: либо ошибка, либо готовый результат для вывода (CommandResult)
  def handle(
              cmd: Command, session: SessionManager, config: AppConfig, engine: RuleEngine
            ): Either[AppError, CommandResult] =

    cmd match
      case Pwd =>
        Right(TextOutput(session.cwd))

      case Cd(rawPath) =>
        val targetPath = if rawPath == "~" then sys.env.getOrElse("USERPROFILE", rawPath) else rawPath
        val cwdBefore = session.cwd

        //Используем map (функтор) для преобразования результата
        session.changeDirectory(targetPath).map { newCwd =>
          session.recordCommand(
            text = s"cd $rawPath", commandName = "cd", args = List(rawPath),
            cwdBefore = cwdBefore, cwdAfter = newCwd,
            startTime = LocalDateTime.now(), endTime = LocalDateTime.now(),
            exitCode = 0, stdout = "", stderr = ""
          )
          SilentSuccess // Говорим Main'у, что всё ок, выводить текст не нужно
        }

      case StartSession(name) =>
        //flatMap (монада) позволяет сделать цепочку: если сессия стартовала -> вернуть текст
        session.startSession(name).map(_ => TextOutput("Сессия с записью протокола начата."))

      case SetRoot(path) =>
        session.setRoot(path).map { _ =>
          val rootDir = session.getSession.map(_.rootDirectory).getOrElse("")
          TextOutput(s"Корневой каталог установлен:\n$rootDir")
        }

      case MakeStep(title) =>
        session.newStep(title).map(_ => TextOutput("Новый шаг создан."))

      case MakeNote(text) =>
        session.addNote(text).map(_ => TextOutput("Комментарий добавлен."))

      case SkipNext =>
        session.setSkipFlag()
        Right(TextOutput("Следующая команда будет исключена из отчёта."))

      case Status =>
        session.getSession match
          case None => Left(SessionNotActive)
          case Some(s) =>
            val stepTitle = s.steps.lift(session.getCurrentStepIndex).map(_.title).getOrElse("-")
            val included = s.steps.flatMap(_.commands).count(_.included)
            val text = s"""Сессия: ${s.name}
                          |Корневой каталог: ${s.rootDirectory}
                          |Текущий каталог: ${s.currentDirectory}
                          |Текущий шаг: $stepTitle
                          |Записано команд: $included
                          |Исключено команд: ${s.excludedCount}
                          |Время начала: ${s.startTime}""".stripMargin
            Right(TextOutput(text))

      case Preview =>
        session.getSession match
          case None => Left(SessionNotActive)
          case Some(s) =>
            val md = ReportGenerator.generate(s, config, engine)
            val outDir = os.Path(config.reportDirectory, os.pwd)
            os.makeDir.all(outDir)
            val file = outDir / s"draft-${s.name}.md"
            os.write.over(file, md)
            Right(TextOutput(s"Предварительный отчёт создан:\n${file.relativeTo(os.pwd)}"))

      case Info =>
        val text = """Лабораторная работа: протоколируемая терминальная сессия
                     |Вариант 9
                     |Разработать консольное приложение, через которое пользователь может выполнять обычные терминальные команды и одновременно формировать отчёт о проделанной работе.""".stripMargin
        Right(TextOutput(text))

      case InstructionsCmd(args) =>
        //Instructions теперь возвращает строку, а не печатает сам
        Right(TextOutput(Instructions.getText(args)))

      case EndSession =>
        session.endSession() match
          case Left(err) => Left(err)
          case Right(finished) =>
            val md = ReportGenerator.generate(finished, config, engine)
            val outDir = os.Path(config.reportDirectory, os.pwd)
            os.makeDir.all(outDir)
            val file = outDir / s"${finished.name}.md"
            os.write.over(file, md)
            Right(TextOutput(s"Сессия завершена. Отчёт сохранён: ${file.relativeTo(os.pwd)}\nОтчет появится после выхода из программы (команда exit [0])"))

      //Для файловых команд используем внешний исполнитель
      case Dir(pathOpt) =>
        CommandExecutor.execute("dir " + pathOpt.getOrElse(""), session, config)

      case _: MkDir | _: TypeFile | _: CopyFile | _: MoveFile | _: DelFile | _: ExecExternal =>
        val raw = cmdToRawString(cmd)
        CommandExecutor.execute(raw, session, config)

      case Exit =>
        Right(SilentSuccess) //Обрабатывается в Main цикле

  //Превращаем типизированную команду обратно в строку для ОС
  private def cmdToRawString(cmd: Command): String = cmd match
    case MkDir(n) => s"mkdir $n"
    case TypeFile(f) => s"type $f"
    case CopyFile(s, d) => s"copy $s $d"
    case MoveFile(s, d) => s"move $s $d"
    case DelFile(f) => s"del $f"
    case ExecExternal(raw) => raw
    case _ => ""