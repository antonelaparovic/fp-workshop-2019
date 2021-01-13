package io.lambdaworks.workshop.exceptions

import scala.util.matching.Regex

object PasswordChecker {

  val numberPattern: Regex = "[0-9]".r
  val upperPattern: Regex  = "[A-Z]".r
  val lowerPattern: Regex  = "[a-z]".r

  def validate(password: String): Either[List[Throwable], String] = {

    val validators =
      List(minNumberOfChars(password, 5),
           containsLowerCase(password),
           containsUpperCase(password),
           containsNumber(password))

    val res: Either[List[Throwable], String] = Left(validators.collect { case Left(x) => x })

    if (validators.forall(_.isRight)) Right(password)
    else res
  }

  private def minNumberOfChars(password: String, length: Int): Either[Throwable, String] = {
    if (password.length() < length) Left(InvalidLength)
    else Right(password)
  }

  private def containsUpperCase(password: String): Either[Throwable, String] = {
    upperPattern.findFirstMatchIn(password) match {
      case Some(_) => Right(password)
      case None    => Left(MissingUppercase)
    }
  }

  private def containsLowerCase(password: String): Either[Throwable, String] = {
    lowerPattern.findFirstMatchIn(password) match {
      case Some(_) => Right(password)
      case None    => Left(MissingLowercase)
    }
  }

  private def containsNumber(password: String): Either[Throwable, String] = {
    numberPattern.findFirstMatchIn(password) match {
      case Some(_) => Right(password)
      case None    => Left(MissingNumber)
    }
  }

  object InvalidLength    extends Throwable("Password must contain at least 5 characters.")
  object MissingUppercase extends Throwable("Password must contain uppercase letter.")
  object MissingLowercase extends Throwable("Password must contain lowercase letter.")
  object MissingNumber    extends Throwable("Password must contain number.")

}
