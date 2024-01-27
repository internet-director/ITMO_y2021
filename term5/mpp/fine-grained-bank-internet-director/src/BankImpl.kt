import java.util.concurrent.locks.ReentrantLock;

/**
 * Bank implementation.
 *
 * :TODO: This implementation has to be made thread-safe.
 *
 * @author :TODO: LastName FirstName
 */
class BankImpl(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val numberOfAccounts: Int
        get() = accounts.size

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun getAmount(index: Int): Long {
        val account = accounts[index]
        account.lock()
        var res = account.amount
        account.unlock()
        return res
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override val totalAmount: Long
        get() {
            var res = 0L
            try {
                for (account in accounts) {
                    account.lock()
                    res += account.amount
                }
            } finally {
                for (account in accounts) {
                    account.unlock()
                }
            }
            return res
        }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun deposit(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[index]
        val res: Long

        try {
            account.lock()
            check(!(amount > Bank.MAX_AMOUNT || account.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }
            account.amount += amount
            res = account.amount
        } finally {
            account.unlock()
        }

        return res
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun withdraw(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[index]
        var res: Long

        try {
            account.lock()
            check(account.amount - amount >= 0) { "Underflow" }
            account.amount -= amount
            res = account.amount
        } finally {
            account.unlock()
        }

        return res
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromIndex != toIndex) { "fromIndex == toIndex" }
        var to: Account
        var from: Account

        if (fromIndex < toIndex) {
            from = accounts[fromIndex]
            from.lock()
            to = accounts[toIndex]
            to.lock()
        } else {
            to = accounts[toIndex]
            to.lock()
            from = accounts[fromIndex]
            from.lock()
        }
        try {
            check(amount <= from.amount) { "Underflow" }
            check(!(amount > Bank.MAX_AMOUNT || to.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }
            from.amount -= amount
            to.amount += amount
        } finally {
            from.unlock()
            to.unlock()
        }
    }

    /**
     * Private account data structure.
     */
    class Account {
        /**
         * Amount of funds in this account.
         */
        var amount: Long = 0

        private val lock = ReentrantLock()

        fun lock() {
            lock.lock()
        }

        fun unlock() {
            lock.unlock()
        }
    }
}