package lab1

//Разбор строки на имя команды и список аргументов
object CommandParser:

  //Цифры и команды (как запомнить слово маппинг?
  val numberMap: Map[String, String] = Map(
    "1" -> "cd", "2" -> "pwd", "3" -> "dir", "4" -> "mkdir", "5" -> "type",
    "6" -> "copy", "7" -> "move", "8" -> "del", "9" -> "starts", "10" -> "prootd",
    "11" -> "mstep", "12" -> "mnote", "13" -> "mskip", "14" -> "mstatus",
    "15" -> "mpreview", "16" -> "info", "17" -> "instructions", "18" -> "ends", "0" -> "exit"
  )

  //Возвращаем Either (монаду), чтобы явно указать: либо ошибка, либо команда
  def parse(line: String): Either[AppError, Command] =
    val tokens = tokenize(line.trim)
    if tokens.isEmpty then Left(ParseError("Пустая команда"))
    else
      //Если ввели цифру, меняем её на текстовое имя команды
      val cmdName = numberMap.getOrElse(tokens.head, tokens.head)
      val args = tokens.tail

      cmdName match
        case "cd"       => getArg(args, "cd").map(Cd.apply)
        case "pwd"      => Right(Pwd)
        case "dir"      => Right(Dir(args.headOption))
        case "mkdir"    => getArg(args, "mkdir").map(MkDir.apply)
        case "type"     => getArg(args, "type").map(TypeFile.apply)
        case "copy"     => getTwoArgs(args, "copy").map((s, d) => CopyFile(s, d))
        case "move"     => getTwoArgs(args, "move").map((s, d) => MoveFile(s, d))
        case "del"      => getArg(args, "del").map(DelFile.apply)

        case "starts"   => Right(StartSession(args.mkString(" ")))
        case "prootd"   => Right(SetRoot(if args.isEmpty then "." else args.mkString(" ")))
        case "mstep"    => Right(MakeStep(args.mkString(" ")))
        case "mnote"    => Right(MakeNote(args.mkString(" ")))
        case "mskip"    => Right(SkipNext)
        case "mstatus"  => Right(Status)
        case "mpreview" => Right(Preview)
        case "info"     => Right(Info)
        case "instructions" | "instruction" => Right(InstructionsCmd(args))
        case "ends"     => Right(EndSession)
        case "exit" | "quit" => Right(Exit)

        //Если ничего не совпало - отправляем на выполнение во внешнюю ОС
        case _          => Right(ExecExternal(line))

  //Вспомогательные методы для безопасного извлечения аргументов
  private def getArg(args: List[String], cmdName: String): Either[AppError, String] =
    args.headOption.toRight(InvalidArguments(cmdName, "Не хватает аргументов"))

  private def getTwoArgs(args: List[String], cmdName: String): Either[AppError, (String, String)] =
    args match
      case a :: b :: _ => Right((a, b))
      case _           => Left(InvalidArguments(cmdName, "Нужно два аргумента"))

  //Посимвольно разбирает строку, отслеживает кавычки
  private def tokenize(s: String): List[String] =
    val result = scala.collection.mutable.ListBuffer[String]()
    val current = new StringBuilder
    var inQuotes = false
    var quoteChar = ' '
    var i = 0

    while i < s.length do
      val c = s.charAt(i)

      if inQuotes then
        //Внутри кавычек — закрывающая кавычка или символ
        if c == quoteChar then inQuotes = false
        else current.append(c)
      else if c == '"' || c == '\'' then
        //Открываем кавычки
        inQuotes = true
        quoteChar = c
      else if c.isWhitespace then
        //Пробел — конец текущего токена
        if current.nonEmpty then
          result += current.toString
          current.clear()
      else
        current.append(c)

      i += 1

    if current.nonEmpty then result += current.toString
    result.toList