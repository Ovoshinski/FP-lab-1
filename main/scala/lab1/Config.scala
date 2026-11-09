package lab1

import upickle.default.*
import java.nio.file.{Files, Paths}


//Настройки отчёта — то, что лежит в блоке "report" файла config.json
case class ReportSettings(
                           includeTimestamps: Boolean = false,
                           includeExitCodes: String = "errors-only",
                           includeDurations: Boolean = false,
                           maximumOutputLines: Int = 100,
                           saveFullOutputSeparately: Boolean = true,
                           allowMarkdownInNotes: Boolean = false,
                           useRelativePaths: Boolean = true
                         )
  derives ReadWriter


//Профиль отчёта — шаблон заголовка и список используемых правил
case class Profile(titleTemplate: String, rules: List[String]) derives ReadWriter


//Условие на аргументы команды — какие флаги должны быть или отсутствовать
case class ArgumentCondition(
                              containsAll: List[String] = Nil,
                              containsAny: List[String] = Nil,
                              containsNone: List[String] = Nil
                            )
  derives ReadWriter


//Правило генерации: как превратить команду в абзац отчёта
case class RuleConfig(
                       id: String,
                       command: String,
                       arguments: Option[ArgumentCondition] = None,
                       priority: Int,
                       descriptionTemplate: String,
                       showCommand: Boolean = true,
                       outputMode: String = "full",
                       outputLanguage: String = "text",
                       group: Option[String] = None,
                       enabled: Boolean = true,
                       maximumLines: Option[Int] = None
                     )
  derives ReadWriter


//Правило группировки — какие команды можно объединять в одно описание
case class GroupingRule(name: String, rules: List[String], strategy: String) derives ReadWriter


//Вся конфигурация приложения
case class AppConfig(
                      shell: String = "/bin/bash",
                      reportDirectory: String = "reports",
                      sessionDirectory: String = "sessions",
                      logFile: String = "logs/report-shell.log",
                      defaultProfile: String = "linux-lab",
                      report: ReportSettings = ReportSettings(),
                      profiles: Map[String, Profile] = Map.empty,
                      rules: List[RuleConfig] = Nil,
                      groupingRules: List[GroupingRule] = Nil
                    )
  derives ReadWriter


//Запасной вариант конфига — используется, если файл не найден или сломан
object AppConfig:
  val default: AppConfig = AppConfig(
    shell = "bash",
    profiles = Map(
      "linux-lab" -> Profile(
        titleTemplate = "Лабораторная работа: {sessionName}",
        rules = List(
          "change-directory", "list-directory", "list-directory-detailed",
          "create-directory", "create-file", "show-file",
          "copy-file", "move-file", "remove-file",
          "print-working-directory", "generic-command"
        )
      )
    ),
    rules = List(
      RuleConfig(id = "change-directory", command = "cd", priority = 100,
        descriptionTemplate = "Перейдём в каталог `{relativeDirectory}`.",
        showCommand = false, outputMode = "errors-only"),

      RuleConfig(id = "list-directory", command = "ls", priority = 50,
        descriptionTemplate = "Просмотрим содержимое каталога `{relativeDirectory}`.",
        showCommand = true, outputMode = "full"),

      RuleConfig(id = "list-directory-detailed", command = "ls",
        arguments = Some(ArgumentCondition(containsAll = List("-l", "-a"))),
        priority = 100,
        descriptionTemplate = "Просмотрим подробное содержимое каталога `{relativeDirectory}`.",
        showCommand = true, outputMode = "full"),

      RuleConfig(id = "create-directory", command = "mkdir", priority = 80,
        descriptionTemplate = "Создадим каталог `{argument1}`.",
        showCommand = true, outputMode = "errors-only"),

      RuleConfig(id = "create-file", command = "touch", priority = 80,
        descriptionTemplate = "Создадим файл `{argument1}`.",
        showCommand = true, outputMode = "errors-only"),

      RuleConfig(id = "show-file", command = "cat", priority = 80,
        descriptionTemplate = "Просмотрим содержимое файла `{argument1}`.",
        showCommand = true, outputMode = "full"),

      RuleConfig(id = "copy-file", command = "cp", priority = 80,
        descriptionTemplate = "Скопируем `{argument1}` в `{argument2}`.",
        showCommand = true, outputMode = "errors-only"),

      RuleConfig(id = "move-file", command = "mv", priority = 80,
        descriptionTemplate = "Переместим `{argument1}` в `{argument2}`.",
        showCommand = true, outputMode = "errors-only"),

      RuleConfig(id = "remove-file", command = "rm", priority = 90,
        descriptionTemplate = "**Внимание:** удаление `{argument1}`.",
        showCommand = true, outputMode = "errors-only"),

      RuleConfig(id = "print-working-directory", command = "pwd", priority = 80,
        descriptionTemplate = "Текущий рабочий каталог: `{relativeDirectory}`.",
        showCommand = true, outputMode = "full"),

      RuleConfig(id = "generic-command", command = "*", priority = 0,
        descriptionTemplate = "Выполним команду `{command}`.",
        showCommand = true, outputMode = "full")
    ),
    groupingRules = List(
      GroupingRule("navigation-group", List("change-directory"), "path-concatenation"),
      GroupingRule("creation-group",   List("create-directory"), "list-creation")
    )
  )


//Загрузка конфига и проверка его на ошибки
object ConfigLoader:

  //Пробуем: файл по указанному пути → файл в resources → запасной
  def load(path: String): AppConfig =
    val p = Paths.get(path)
    if Files.exists(p) then
      return readFromFile(p.toString)

    val stream = getClass.getResourceAsStream("/config.json")
    if stream != null then
      try
        val text = scala.io.Source.fromInputStream(stream, "UTF-8").mkString
        return read[AppConfig](text)
      catch
        case e: Exception =>
          println(s"Ошибка чтения config.json из ресурсов: ${e.getMessage}")

    println(s"Конфиг не найден: $path. Используем значения по умолчанию.")
    AppConfig.default


  private def readFromFile(path: String): AppConfig =
    try
      val text = new String(Files.readAllBytes(Paths.get(path)), "UTF-8")
      read[AppConfig](text)
    catch
      case e: Exception =>
        println(s"Ошибка чтения конфига: ${e.getMessage}. Используем значения по умолчанию.")
        AppConfig.default


  //Проверка конфига: возвращает список найденных проблем
  def validate(config: AppConfig): List[String] =
    val errors = scala.collection.mutable.ListBuffer[String]()

    if config.shell.trim.isEmpty then errors += "shell не указан"
    if config.reportDirectory.trim.isEmpty then errors += "reportDirectory не указан"
    if !config.profiles.contains(config.defaultProfile) then
      errors += s"Профиль по умолчанию '${config.defaultProfile}' не существует"

    val ids = config.rules.map(_.id)
    val duplicates = ids.diff(ids.distinct)
    if duplicates.nonEmpty then
      errors += s"Повторяющиеся id правил: ${duplicates.mkString(", ")}"

    val validModes = Set("none", "full", "errors-only", "first-lines",
      "last-lines", "limited", "summary")

    config.rules.foreach { r =>
      if r.priority < 0 then errors += s"Правило '${r.id}': приоритет отрицательный"
      if !validModes.contains(r.outputMode) then
        errors += s"Правило '${r.id}': неподдерживаемый outputMode '${r.outputMode}'"
      if r.maximumLines.exists(_ < 0) then
        errors += s"Правило '${r.id}': maximumLines отрицательный"
    }

    config.groupingRules.foreach { g =>
      g.rules.foreach { ruleId =>
        if !ids.contains(ruleId) then
          errors += s"Группировка '${g.name}' ссылается на несуществующее правило '$ruleId'"
      }
    }

    errors.toList