package lab1

import upickle.default.*

case class InstructionOption(flag: String, description: String) derives ReadWriter
case class InstructionExample(cmd: String, desc: String) derives ReadWriter

case class Instruction(
                        name: String, category: String, description: String, usage: String,
                        options: List[InstructionOption] = Nil,
                        examples: List[InstructionExample] = Nil,
                        reportBehavior: String = ""
                      ) derives ReadWriter

case class InstructionsData(
                             categories: List[Map[String, String]],
                             commands: Map[String, Instruction]
                           ) derives ReadWriter


//Справка по командам. Загружает JSON из resources, кэширует
object Instructions:

  private lazy val data: InstructionsData =
    val stream = getClass.getResourceAsStream("/instructions.json")
    if stream == null then
      fallback
    else
      try
        val text = scala.io.Source.fromInputStream(stream, "UTF-8").mkString
        read[InstructionsData](text)
      catch
        case e: Exception => fallback


  private val fallback = InstructionsData(
    categories = List(Map("id" -> "all", "name" -> "Все команды")),
    commands = Map(
      "instructions" -> Instruction(
        name = "instructions", category = "report",
        description = "Справка по командам",
        usage = "instructions [номер_или_имя]"
      )
    )
  )

  //Вместо печати в консоль возвращаем строку
  def getText(args: List[String]): String =
    if args.isEmpty then getListText
    else
      val arg = args.head
      if arg.matches("\\d+") then
        val num = arg.toInt
        CommandParser.numberMap.get(num.toString) match
          case Some(cmdName) =>
            data.commands.get(cmdName) match
              case Some(instr) => getOneText(instr, Some(num))
              case None        => s"Команда #$num ($cmdName) не имеет справки.\n"
          case None =>
            s"Команда с номером $num не найдена.\n"
      else
        data.commands.get(arg) match
          case Some(instr) =>
            val num = CommandParser.numberMap.find(_._2 == arg).map(_._1.toInt)
            getOneText(instr, num)
          case None        => s"Команда не найдена: $arg\n"

  private def getListText: String =
    val sb = new StringBuilder
    val fileCommands = data.commands.values.toList
      .filter(_.category == "files")
      .sortBy(c => CommandParser.numberMap.find(_._2 == c.name).map(_._1.toInt).getOrElse(999))

    val reportCommands = data.commands.values.toList
      .filter(_.category == "report")
      .sortBy(c => CommandParser.numberMap.find(_._2 == c.name).map(_._1.toInt).getOrElse(999))

    sb.append("Команды для работы с файлами:\n")
    fileCommands.foreach(c => sb.append(printCmdLine(c)))

    sb.append("\nКоманды для работы с отчётом:\n")
    reportCommands.foreach(c => sb.append(printCmdLine(c)))

    sb.append("\nВведите номер или имя команды.\n")
    sb.append("Для подробной справки: 17 <номер_или_имя>\n")
    sb.toString

  private def printCmdLine(c: Instruction): String =
    val num = CommandParser.numberMap.find(_._2 == c.name).map(_._1).getOrElse("")
    f"  [$num%2s]  ${c.name}%-14s - ${c.description}\n"

  private def getOneText(instr: Instruction, num: Option[Int]): String =
    val sb = new StringBuilder
    val numStr = num.map(n => s" [#$n]").getOrElse("")
    sb.append(s"${instr.name}$numStr - ${instr.description}\n\n")
    sb.append("Использование:\n")
    sb.append(s"  ${instr.usage}\n\n")

    if instr.options.nonEmpty then
      sb.append("Опции:\n")
      instr.options.foreach(o => sb.append(f"  ${o.flag}%-6s ${o.description}\n"))
      sb.append("\n")

    if instr.examples.nonEmpty then
      sb.append("Примеры:\n")
      instr.examples.foreach(e => sb.append(f"  ${e.cmd}%-40s # ${e.desc}\n"))
      sb.append("\n")

    if instr.reportBehavior.nonEmpty then
      sb.append(s"Что будет написано в отчёте: ${instr.reportBehavior}\n")

    sb.toString