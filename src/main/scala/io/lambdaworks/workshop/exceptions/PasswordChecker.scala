package io.lambdaworks.workshop.exceptions

import java.lang

import scala.runtime.Nothing$
import scala.util.matching.Regex

object PasswordChecker {

  def validate(password: String): Either[List[Throwable], String] = {

    def validateLoop(resList: List[Either[Throwable, String]],
                     acc: List[Throwable] = Nil): List[Throwable] =
      resList match {
        case x :: xs if x.isLeft  => validateLoop(xs, x :: acc)
        case x :: xs if x.isRight => validateLoop(xs, acc)
        case Nil                  => acc
      }

    val res0 = validateLoop(
      List(minNumberOfChars(password, 5),
           containsLowerCase(password),
           containsUpperCase(password),
           containsNumber(password)))

    if (res0.nonEmpty) Left(res0)
    else Right(password)
  }

  private def minNumberOfChars(password: String, length: Int): Either[Throwable, String] = {
    if (password.length() < length) Left(InvalidLength)
    else Right(password)
  }

  private def containsUpperCase(password: String): Either[Throwable, String] = {
    if (password.compareTo(password.toLowerCase) == 1) Left(MissingUppercase)
    else Right(password)
  }

  private def containsLowerCase(password: String): Either[Throwable, String] = {
    if (password.compareTo(password.toUpperCase) == 1) Left(MissingLowercase)
    else Right(password)
  }

  private def containsNumber(password: String): Either[Throwable, String] = {
    val numberPattern: Regex = "[0-9]".r
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
