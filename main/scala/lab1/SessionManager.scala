package lab1

import java.time.LocalDateTime
import upickle.default.*

//Управление сессией: начать, завершить, шаги, заметки, cwd, сохранение
class SessionManager(config: AppConfig, initialCwd: String):

  private var currentSession: Option[Session] = None
  private var standaloneCwd: String = initialCwd
  private var currentStepIndex: Int = -1
  private var skipNext: Boolean = false
  private var commandCounter: Int = 0

  //Текущий рабочий каталог
  def cwd: String = currentSession.map(_.currentDirectory).getOrElse(standaloneCwd)

  def isActive: Boolean = currentSession.exists(_.status == "active")

  def setSkipFlag(): Unit = skipNext = true

  //Путь к файлу сохранённой сессии
  private def sessionFilePath: String =
    val dir = os.Path(config.sessionDirectory, os.pwd)
    os.makeDir.all(dir)
    (dir / "current-session.json").toString

  //Записать текущую сессию
  def saveSession(): Unit =
    currentSession.foreach { s =>
      try os.write.over(os.Path(sessionFilePath), write(s))
      catch case e: Exception => Logger.error(s"Не удалось сохранить сессию: ${e.getMessage}")
    }

  //Удалить файл сессии (после ends)
  def clearSessionFile(): Unit =
    try
      val p = os.Path(sessionFilePath)
      if os.exists(p) then os.remove(p)
    catch case _: Exception => ()

  //Прочитать сессию
  def loadSessionFromDisk(): Option[Session] =
    try
      val p = os.Path(sessionFilePath)
      if !os.exists(p) then None
      else Some(read[Session](os.read(p)))
    catch
      case e: Exception =>
        Logger.error(s"Не удалось загрузить сессию: ${e.getMessage}")
        None

  //Восстановить работу с уже загруженной сессией
  def resumeSession(s: Session): Unit =
    currentSession = Some(s)
    currentStepIndex = s.steps.size - 1
    commandCounter = s.steps.flatMap(_.commands).size

  //Начать новую сессию
  def startSession(name: String): Either[AppError, Unit] =
    if isActive then Left(SessionAlreadyActive)
    else
      val session = Session(
        name = if name.trim.isEmpty then "Без названия" else name.trim,
        startTime = LocalDateTime.now(),
        endTime = None,
        rootDirectory = cwd,
        currentDirectory = cwd,
        shell = config.shell,
        profile = config.defaultProfile
      )
      currentSession = Some(session)
      currentStepIndex = -1
      commandCounter = 0
      Logger.info(s"Начата сессия: \"${session.name}\"")
      saveSession()
      Right(())

  //Задать корневой каталог сессии
  def setRoot(path: String): Either[AppError, Unit] =
    currentSession match
      case None => Left(SessionNotActive)
      case Some(s) =>
        val resolved = resolvePath(path)
        val p = os.Path(resolved)
        if !os.exists(p) then Left(DirectoryNotFound(path))
        else if !os.isDir(p) then Left(DirectoryNotFound(path))
        else
          currentSession = Some(s.copy(rootDirectory = resolved))
          Logger.info(s"Root установлен: $resolved")
          saveSession()
          Right(())

  //Создать новый шаг
  def newStep(title: String): Either[AppError, Unit] =
    currentSession match
      case None => Left(SessionNotActive)
      case Some(s) =>
        val idx = s.steps.size
        val actualTitle = if title.trim.isEmpty then s"Шаг ${idx + 1}" else title.trim
        currentSession = Some(s.copy(steps = s.steps :+ Step(idx, actualTitle)))
        currentStepIndex = idx
        Logger.info(s"Создан шаг #$idx: \"$actualTitle\"")
        saveSession()
        Right(())

  //Добавить комментарий в текущий шаг. Если шага нет — создаёт "Шаг 1"
  def addNote(text: String): Either[AppError, Unit] =
    currentSession match
      case None => Left(SessionNotActive)
      case Some(s) =>
        if currentStepIndex < 0 then newStep("Шаг 1")
        currentSession match
          case Some(s2) =>
            val step = s2.steps(currentStepIndex)
            val newSteps = s2.steps.updated(
              currentStepIndex,
              step.copy(notes = step.notes :+ text)
            )
            currentSession = Some(s2.copy(steps = newSteps))
            saveSession()
            Right(())
          case None => Left(SessionNotActive)

  //Записать команду в текущий шаг или создать "Шаг 1", если его нет
  def recordCommand(
                     text: String, commandName: String, args: List[String],
                     cwdBefore: String, cwdAfter: String,
                     startTime: LocalDateTime, endTime: LocalDateTime,
                     exitCode: Int, stdout: String, stderr: String
                   ): Unit =
    currentSession.foreach { s =>
      if skipNext then
        //Команда была помечена mskip, то есть, не попадает в отчёт
        Logger.info(s"Команда id=$commandCounter исключена из отчёта")
        commandCounter += 1
        skipNext = false
        saveSession()
      else
        val rec = CommandRecord(
          id = commandCounter, text = text, commandName = commandName,
          arguments = args, cwdBefore = cwdBefore, cwdAfter = cwdAfter,
          startTime = startTime, endTime = endTime,
          exitCode = exitCode, stdout = stdout, stderr = stderr,
          stepIndex = if currentStepIndex < 0 then 0 else currentStepIndex,
          included = true
        )
        val steps =
          if s.steps.isEmpty then List(Step(0, "Шаг 1", Nil, List(rec)))
          else
            val idx = currentStepIndex.max(0)
            val step = s.steps(idx)
            s.steps.updated(idx, step.copy(commands = step.commands :+ rec))

        currentSession = Some(s.copy(steps = steps))
        commandCounter += 1
        saveSession()
    }

  //Сменить рабочий каталог. Работает и до, и во время сессии
  def changeDirectory(path: String): Either[AppError, String] =
    val actualPath = if path.trim == "up" then ".." else path.trim
    val resolved = resolvePath(actualPath)
    val p = os.Path(resolved)

    if !os.exists(p) then Left(DirectoryNotFound(path))
    else if !os.isDir(p) then Left(DirectoryNotFound(path))
    else
      currentSession match
        case Some(s) => currentSession = Some(s.copy(currentDirectory = resolved))
        case None    => standaloneCwd = resolved
      saveSession()
      Right(resolved)

  //Завершить сессию
  def endSession(): Either[AppError, Session] =
    currentSession match
      case None => Left(SessionNotActive)
      case Some(s) =>
        val finished = s.copy(endTime = Some(LocalDateTime.now()), status = "finished")
        currentSession = Some(finished)
        Logger.info(s"Сессия завершена: \"${s.name}\"")
        clearSessionFile()
        Right(finished)

  def getSession: Option[Session] = currentSession
  def getCurrentStepIndex: Int = currentStepIndex

  private def resolvePath(path: String): String =
    val trimmed = path.trim
    val base = if trimmed.startsWith("~") then sys.env.getOrElse("HOME", "/home") else cwd
    val expanded = if trimmed.startsWith("~") then trimmed.replaceFirst("^~", base) else trimmed
    os.Path(expanded, os.Path(base)).toString