package lab1

import java.time.LocalDateTime
import scala.util.Try

//Запуск внешних команд напрямую через cmd.exe
object CommandExecutor:

  def execute(rawCommand: String, session: SessionManager, config: AppConfig): Either[AppError, CommandResult] =
    val cwdBefore = session.cwd
    val startTime = LocalDateTime.now()

    //Try перехватывает любые Java-исключения.
    //toEither превращает Success(T) в Right(T), а Failure(E) в Left(E).
        val result = Try {
          os.proc(config.shell, "/c", rawCommand).call(
            cwd = os.Path(cwdBefore),
            check = false,
            stdout = os.Pipe,
            stderr = os.Pipe,
            mergeErrIntoOut = false
          )
        }.toEither

    val endTime = LocalDateTime.now()

    result match
      case Left(ex) =>
        Left(ExecutionFailed(ex.getMessage))

      case Right(proc) =>
        val exitCode = proc.exitCode
        
        val isWindows = System.getProperty("os.name").toLowerCase.contains("win")
        val charset = if (isWindows) java.nio.charset.Charset.forName("CP866")
        else java.nio.charset.StandardCharsets.UTF_8
        
        val stdout = new String(proc.out.bytes, charset)
        val stderr = new String(proc.err.bytes, charset)

        session.recordCommand(
          text = rawCommand,
          commandName = rawCommand.split(" ").headOption.getOrElse(""),
          args = rawCommand.split(" ").drop(1).toList,
          cwdBefore = cwdBefore, cwdAfter = session.cwd,
          startTime = startTime, endTime = endTime,
          exitCode = exitCode, stdout = stdout, stderr = stderr
        )
        
        //Возвращаем результат, Main сам его напечатает
        Right(ExecResult(stdout, stderr, exitCode))