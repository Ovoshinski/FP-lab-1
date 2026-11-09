package lab1

import java.time.LocalDateTime
import upickle.default.*

//Преобразование LocalDateTime в строку и обратно для JSON
given ReadWriter[LocalDateTime] = readwriter[String].bimap[LocalDateTime](_.toString, LocalDateTime.parse)

//Запись об одной выполненной команде
case class CommandRecord(
                          id: Int, 
                          text: String,
                          commandName: String,
                          arguments: List[String],
                          cwdBefore: String,
                          cwdAfter: String,
                          startTime: LocalDateTime,
                          endTime: LocalDateTime,
                          exitCode: Int,
                          stdout: String,
                          stderr: String,
                          stepIndex: Int,
                          included: Boolean = true,
                          ruleId: Option[String] = None
                        )
  derives ReadWriter

//Шаг отчёта. Содержит заметки пользователя и команды
case class Step(
                 index: Int,
                 title: String,
                 notes: List[String] = Nil,
                 commands: List[CommandRecord] = Nil
               ) derives ReadWriter:
  def isEmpty: Boolean = notes.isEmpty && commands.isEmpty

//Сессия — единица записи. Хранит название, время, пути и список шагов
case class Session(
                    name: String,
                    startTime: LocalDateTime,
                    endTime: Option[LocalDateTime],
                    rootDirectory: String,
                    currentDirectory: String,
                    shell: String,
                    profile: String,
                    steps: List[Step] = Nil,
                    excludedCount: Int = 0,
                    status: String = "active"
                  ) 
  derives ReadWriter