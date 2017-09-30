package fi.kapsi.kosmik.sfti.ch08

/**
  * Extend the following BankAccount class to a CheckingAccount class that charges $1
  * for every deposit and withdrawal.
  */
object Ex01 {

  class BankAccount(initialBalance: Double) {
    private var balance = initialBalance

    def currentBalance: Double = balance

    def deposit(amount: Double): Double = {
      balance += amount
      balance
    }

    def withdraw(amount: Double): Double = {
      balance -= amount
      balance
    }
  }

  class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
    override def deposit(amount: Double): Double = super.deposit(amount - 1)

    override def withdraw(amount: Double): Double = super.withdraw(amount + 1)
  }

}
