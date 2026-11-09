package lab1

import java.time.format.DateTimeFormatter

//Генерация Markdown-отчёта из готовой сессии
object ReportGenerator:

  private val dateFmt = DateTimeFormatter.ofPattern("d MMMM yyyy, HH:mm")


  def generate(session: Session, config: AppConfig, engine: RuleEngine): String =
    val sb = new StringBuilder

    //Заголовок
    val profile = config.profiles.getOrElse(session.profile, config.profiles(config.defaultProfile))
    val title = profile.titleTemplate.replace("{sessionName}", session.name)
    sb.append(s"# $title\n\n")

    //Сведения о сессии
    sb.append("## Сведения о сессии\n\n")
    sb.append(s"- Начало: ${session.startTime.format(dateFmt)}\n")
    session.endTime.foreach(t => sb.append(s"- Завершение: ${t.format(dateFmt)}\n"))
    sb.append(s"- Корневой каталог: `${session.rootDirectory}`\n")
    sb.append(s"- Командный интерпретатор: `${session.shell}`\n")
    sb.append(s"- Профиль: `${session.profile}`\n\n")

    //Шаги — пустые пропускаем
    val nonEmptySteps = session.steps.filterNot(_.isEmpty)
    nonEmptySteps.zipWithIndex.foreach { case (step, idx) =>
      sb.append(s"## Шаг ${idx + 1}. ${step.title}\n\n")

      step.notes.foreach(note => sb.append(s"$note\n\n"))

      step.commands.filter(_.included).foreach { rec =>
        //Прямая цитата команды
        sb.append(s"Выполнена команда:\n\n")
        sb.append(s"```bash\n${rec.text}\n```\n\n")

        //Вывод, если он есть
        if rec.stdout.nonEmpty || rec.stderr.nonEmpty then
          sb.append("Результат:\n\n")
          val output = rec.stdout + (if rec.stderr.nonEmpty then "\n" + rec.stderr else "")
          sb.append(s"```text\n${escapeFences(output)}\n```\n\n")
      }
    }

    //Ошибки
    val errors = session.steps.flatMap(_.commands).filter(c => c.included && c.exitCode != 0)
    if errors.nonEmpty then
      sb.append("## Ошибки и предупреждения\n\n")
      errors.foreach { rec =>
        sb.append(s"Команда `${rec.text}` завершилась с кодом `${rec.exitCode}`.\n\n")
      }

    //Итоги
    val allCommands = session.steps.flatMap(_.commands)
    val included = allCommands.count(_.included)
    val success  = allCommands.count(c => c.included && c.exitCode == 0)
    val failed   = allCommands.count(c => c.included && c.exitCode != 0)

    sb.append("## Итоги\n\n")
    sb.append(s"- Выполнено команд: $included\n")
    sb.append(s"- Успешно: $success\n")
    sb.append(s"- Завершилось с ошибкой: $failed\n")
    sb.append(s"- Исключено из отчёта: ${session.excludedCount}\n")

    sb.toString

  //Экранировать ``` внутри текста, чтобы не сломать Markdown
  private def escapeFences(text: String): String =
    text.replace("```", "`` `")