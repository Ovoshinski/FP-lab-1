package lab1

// --- 1. ADT Команд (Sum type) ---
// Описываем все возможные команды, которые понимает система.
sealed trait Command
case class Cd(path: String) extends Command
case object Pwd extends Command
case class Dir(path: Option[String]) extends Command
case class MkDir(dirName: String) extends Command
case class TypeFile(fileName: String) extends Command
case class CopyFile(src: String, dst: String) extends Command
case class MoveFile(src: String, dst: String) extends Command
case class DelFile(fileName: String) extends Command

// Команды управления отчетом
case class StartSession(name: String) extends Command
case class SetRoot(path: String) extends Command
case class MakeStep(title: String) extends Command
case class MakeNote(text: String) extends Command
case object SkipNext extends Command
case object Status extends Command
case object Preview extends Command
case object Info extends Command
case class InstructionsCmd(args: List[String]) extends Command
case object EndSession extends Command
case object Exit extends Command

// Выполнение внешней команды (если ввели что-то неизвестное, пробуем запустить в cmd/bash)
case class ExecExternal(rawCommand: String) extends Command


// --- 2. ADT Ошибок (Sum type) ---
// Вместо строк "Ошибка 1", мы создаем типы. Это позволяет обрабатывать ошибки программно.
sealed trait AppError
case class DirectoryNotFound(path: String) extends AppError
case object SessionAlreadyActive extends AppError
case object SessionNotActive extends AppError
case class UnknownCommand(cmd: String) extends AppError
case class InvalidArguments(cmd: String, msg: String) extends AppError
case class ExecutionFailed(msg: String) extends AppError
case class ParseError(msg: String) extends AppError


// --- 3. ADT Результатов (Product/Sum type) ---
// Бизнес-логика не печатает текст, она возвращает этот тип.
// А Main.scala смотрит на него и решает, как вывести на экран.
sealed trait CommandResult
case class TextOutput(text: String) extends CommandResult
case class ExecResult(stdout: String, stderr: String, exitCode: Int) extends CommandResult
case object SilentSuccess extends CommandResult