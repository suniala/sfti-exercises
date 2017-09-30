package fi.kapsi.kosmik.sfti.ch08

/**
  * Extend the BankAccount class of the preceding exercise into a class SavingsAccount
  * that earns interest every month (when a method earnMonthlyInterest is called)
  * and has three free deposits or withdrawals every month. Reset the transaction
  * count in the earnMonthlyInterest method.
  */
object Ex02 {

  class SavingsAccount(initialBalance: Double) extends Ex01.BankAccount(initialBalance) {
    val freeTransactions: Int = 3

    var remainingFreeTransactions: Int = freeTransactions

    def earnMonthlyInterest(rate: Double): Double = {
      remainingFreeTransactions = freeTransactions
      super.deposit(rate * super.currentBalance)
    }

    private def transactionFee() = {
      val fee = if (remainingFreeTransactions > 0) 0.0 else 1.0
      remainingFreeTransactions -= 1
      fee
    }

    override def deposit(amount: Double): Double = super.deposit(amount - transactionFee())

    override def withdraw(amount: Double): Double = super.withdraw(amount + transactionFee())
  }

}
