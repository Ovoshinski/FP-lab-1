package lab1

//Преобразование POSIX-команд в аналоги Windows.
object CommandTranslator:

  val isWindows: Boolean =
    System.getProperty("os.name").toLowerCase.contains("win")


  //Возвращает пару (команду, аргументы)
  def translate(cmd: String, args: List[String], cwd: String): (String, List[String]) =
    if !isWindows then (cmd, args)
    else
      cmd match
        case "ls" => translateLs(args)
        case "cat" => ("cmd.exe", List("/c", "type") ++ args)
        case "pwd" => ("cmd.exe", List("/c", "cd"))
        case "mkdir" => ("cmd.exe", List("/c", "mkdir") ++ args)
        case "touch" => translateTouch(args)
        case "rm" => translateRm(args, cwd)
        case "cp" => translateCp(args)
        case "mv" => ("cmd.exe", List("/c", "move") ++ args.filterNot(_.startsWith("-")))
        case "grep" => ("cmd.exe", List("/c", "findstr") ++ args)
        case _ => (cmd, args)


  //ls -> dir. Флаг -a даёт /a — показать скрытые файлы
  private def translateLs(args: List[String]): (String, List[String]) =
    val showAll = args.exists(a => a.startsWith("-") && a.contains("a"))
    val paths   = args.filterNot(_.startsWith("-"))
    val flags   = if showAll then List("/a") else Nil
    ("cmd.exe", List("/c", "dir") ++ flags ++ paths)


  //touch -> создать пустой файл
  private def translateTouch(args: List[String]): (String, List[String]) =
    args.headOption match
      case Some(f) => ("cmd.exe", List("/c", s"type nul > $f"))
      case None    => ("cmd.exe", List("/c", "type", "nul"))


  //rm -> автоматически выбирает del (файл) или rmdir /s /q (каталог)
  private def translateRm(args: List[String], cwd: String): (String, List[String]) =
    val files = args.filterNot(_.startsWith("-"))
    files.headOption match
      case Some(f) =>
        val isDir =
          try
            val p = os.Path(f, os.Path(cwd))
            os.exists(p) && os.isDir(p)
          catch
            case _: Exception => false

        if isDir then ("cmd.exe", List("/c", "rmdir", "/s", "/q", f))
        else         ("cmd.exe", List("/c", "del", f))

      case None =>
        ("cmd.exe", List("/c", "del"))


  // cp -> copy
  private def translateCp(args: List[String]): (String, List[String]) =
    val clean = args.filterNot(_.startsWith("-"))
    ("cmd.exe", List("/c", "copy") ++ clean)