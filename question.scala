package Fluent

import PollDeps._
import scala.util._

class Question(val question: String, val questionType: Option[QuestionType] = Some(Open),
               val options: Option[Array[String]] = None, val errorMessage: Option[String] = None) {
  def withType(typeTry: Try[String]): Question =
    if (errorMessage.nonEmpty) this
    else typeTry match {
      case Success(value) =>
        value.toLowerCase match {
          case "open" => Question.from(this, questionType = Some(Open))
          case "choice" => Question.from(this, questionType = Some(Choice))
          case "multi" => Question.from(this, questionType = Some(Multi))
          case _ => Question.from(this, errorMessage = Some("Faulted: Incorrect question format"))
        }
      case Failure(_) => this
    }

  def withOptions(optionsTry: Try[Array[String]]): Question =
    if (errorMessage.nonEmpty) this
    else optionsTry match {
      case Success(value) => Question.from(this, options = Some(value))
      case Failure(_) => this
    }

  def createOrMessage: Either[String, Question] =
    if (errorMessage.nonEmpty) Left(errorMessage.get)
    else if (questionType.contains(Open) && options.nonEmpty) Left("Faulted: open question should has no options")
    else if (!questionType.contains(Open) && options.isEmpty) Left("Faulted: choice or multi question should has options")
    else Right(this)
}

object Question {
  def apply(question: String) = new Question(question)

  def from(question: Question, questionType: Option[QuestionType] = None,
           options: Option[Array[String]] = None, errorMessage: Option[String] = None): Question =
    new Question(question.question,
      questionType = if (questionType.nonEmpty) questionType else question.questionType,
      options = if (options.nonEmpty) options else question.options,
      errorMessage = if (errorMessage.nonEmpty) errorMessage else question.errorMessage)
}
