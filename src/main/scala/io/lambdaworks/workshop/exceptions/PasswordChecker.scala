package io.lambdaworks.workshop.exceptions

import scala.util.matching.Regex

object PasswordChecker {

  def validate(password: String): Either[List[Throwable], String] = {
    val res0    = minNumberOfChars(password, 5)
    val res1    = containsLowerCase(password)
    val res2    = containsUpperCase(password)
    val res3    = containsNumber(password)
    val resList = List()
    if (res0.isLeft) resList :+ res0
    if (res1.isLeft) resList :+ res1
    if (res2.isLeft) res2 :: resList
    if (res3.isLeft) res3 :: resList

    if (resList.nonEmpty) Left(resList)
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
