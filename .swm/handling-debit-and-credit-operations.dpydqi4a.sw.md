---
title: Handling Debit and Credit Operations
---
In this document, we will explain the process of handling debit and credit operations on a bank account. The process involves retrieving account details, opening a database connection, and updating the account balance.

The flow starts by logging the entry and retrieving the account details. If the account is found, it opens a connection to the database and prepares SQL statements for querying and updating the account balance. The method then updates the account's actual and available balances and executes the update SQL statement. If the account is not found, it logs a warning and exits.

# Flow drill down

```mermaid
graph TD;
      subgraph srcwebuisrcmainjavacomibmcicscipbankliberty["src/webui/src/main/java/com/ibm/cics/cip/bankliberty"]
13975a5e665f46ae98687eb021e52bc4d72ae85492d9f841cfb6a5cb09ef6c0b(debitCredit) --> cb5c98c0144af37ade4635cc742aafdb2b750e19319280e1a5f441154aad213f(getAccount)
end

subgraph srcwebuisrcmainjavacomibmcicscipbankliberty["src/webui/src/main/java/com/ibm/cics/cip/bankliberty"]
cb5c98c0144af37ade4635cc742aafdb2b750e19319280e1a5f441154aad213f(getAccount) --> 4ccb7e7f1ffffa4e90c42aeedb17171c5fac78dfdb55c155b9b79af1f1797072(openConnection)
end

subgraph srcwebuisrcmainjavacomibmcicscipbankliberty["src/webui/src/main/java/com/ibm/cics/cip/bankliberty"]
4ccb7e7f1ffffa4e90c42aeedb17171c5fac78dfdb55c155b9b79af1f1797072(openConnection) --> a45bfb1a8c2dd416e124b8a4fc5988afb239da44caac19057d872f11b7c78c0c(openConnectionInternal)
end


      classDef mainFlowStyle color:#000000,fill:#7CB9F4
classDef rootsStyle color:#000000,fill:#00FFF4
classDef Style1 color:#000000,fill:#00FFAA
classDef Style2 color:#000000,fill:#FFFF00
classDef Style3 color:#000000,fill:#AA7CB9
```

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" line="1000">

---

## Handling account debit and credit operations

First, the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="1000:5:5" line-data="	public boolean debitCredit(BigDecimal apiAmount)">`debitCredit`</SwmToken> method is responsible for handling the debit and credit operations on an account. It starts by logging the entry and then retrieves the account details using the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="1003:9:9" line-data="		Account temp = this.getAccount(">`getAccount`</SwmToken> method. If the account is not found, it logs a warning and exits. If the account is found, it opens a connection to the database and prepares SQL statements for querying and updating the account balance. The method then updates the account's actual and available balances and executes the update SQL statement.

```java
	public boolean debitCredit(BigDecimal apiAmount)
	{
		logger.entering(this.getClass().getName(), DEBIT_CREDIT_ACCOUNT);
		Account temp = this.getAccount(
				Integer.parseInt(this.getAccountNumber()),
				Integer.parseInt(this.getSortcode()));
		if (temp == null)
		{
			logger.log(Level.WARNING,
					() -> "Unable to find account " + this.getAccountNumber());
			logger.exiting(this.getClass().getName(), DEBIT_CREDIT_ACCOUNT,
					false);
			return false;
		}

		openConnection();
		String accountNumberString = temp.getAccountNumber();

		String sortCodeString = padSortCode(
				Integer.parseInt(this.getSortcode()));
		String sql1 = SQL_SELECT;
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" line="402">

---

### Retrieving account details

Next, the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="402:5:5" line-data="	public Account getAccount(int accountNumber, int sortCode)">`getAccount`</SwmToken> method retrieves the account details from the database. It opens a connection and prepares SQL statements to query the account based on the account number and sort code. If the account number is a special value (99999999), it uses a different SQL query. The method then processes the result set to create an <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="402:3:3" line-data="	public Account getAccount(int accountNumber, int sortCode)">`Account`</SwmToken> object with the retrieved details.

```java
	public Account getAccount(int accountNumber, int sortCode)
	{
		logger.entering(this.getClass().getName(), GET_ACCOUNT + accountNumber);
		openConnection();
		Account temp = null;

		String sortCodeString = padSortCode(sortCode);
		String sql9999 = "SELECT * from ACCOUNT where ACCOUNT_EYECATCHER LIKE 'ACCT' AND ACCOUNT_SORTCODE like ? order by ACCOUNT_NUMBER DESC";
		String sql = SQL_SELECT;
		try (PreparedStatement stmt9999 = conn.prepareStatement(sql9999);
				PreparedStatement stmt = conn.prepareStatement(sql);)
		{
			if (accountNumber == 99999999)
			{

				logger.log(Level.FINE, () -> PRE_SELECT_MSG + sql9999 + ">");

				stmt9999.setString(1, sortCodeString);
				ResultSet rs = stmt9999.executeQuery();
				if (rs.next())
				{
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="69">

---

### Opening a database connection

Then, the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="69:5:5" line-data="	protected void openConnection()">`openConnection`</SwmToken> method is responsible for opening a connection to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="71:13:13" line-data="		// Open a connection to the DB2 database">`DB2`</SwmToken> database. It checks if a connection already exists for the current task and reuses it if available. If the connection is closed or does not exist, it calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="87:1:1" line-data="			openConnectionInternal();">`openConnectionInternal`</SwmToken> method to create a new connection.

```java
	protected void openConnection()
	{
		// Open a connection to the DB2 database
		logger.entering(this.getClass().getName(), "openConnection()");

		Integer taskNumberInteger = Task.getTask().getTaskNumber();
		String db2ConnString = DB2CONN.concat(taskNumberInteger.toString());
		logger.log(Level.FINE,
				() -> "Attempting to get DB2CONN for task number "
						+ taskNumberInteger.toString());
		this.conn = (Connection) cornedBeef.get(db2ConnString);
		if (this.conn == null)
		{
			HBankDataAccess.incrementConnCount();
			logger.log(Level.FINE,
					() -> "Attempting to create DB2CONN for task number "
							+ taskNumberInteger.toString());
			// Attempt to open a connection
			openConnectionInternal();
			logger.log(Level.FINE,
					() -> "Creation succcessful for DB2CONN for task number "
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="162">

---

### Creating a new database connection

Finally, the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="163:3:3" line-data="	void openConnectionInternal()">`openConnectionInternal`</SwmToken> method creates a new database connection using a JNDI lookup. It retrieves the data source and attempts to get a connection. If successful, it sets the transaction isolation level and stores the connection in a map for reuse. If any errors occur, it logs the error and triggers an abend (abnormal end) for the task.

```java
	@SuppressWarnings("unchecked")
	void openConnectionInternal()
	{
		logger.entering(this.getClass().getName(), "openConnectionInternal");
		String jndiString = "jdbc/defaultCICSDataSource";
		Context ctx;

		try
		{
			ctx = new InitialContext();
			DataSource ds = (DataSource) ctx.lookup(jndiString);
			logger.log(Level.FINE, () -> "jndi string is " + jndiString);
			// If there is no current connection
			if (this.conn == null)
			{
				logger.log(Level.FINE,
						() -> "About to attempt to get DB2 connection");
				// Try and get a connection
				this.conn = ds.getConnection();
				this.conn.setTransactionIsolation(
						Connection.TRANSACTION_READ_UNCOMMITTED);
```

---

</SwmSnippet>

# Where is this flow used?

This flow is used once, in a flow starting from `transferLocalExternal` as represented in the following diagram:

```mermaid
graph TD;
      subgraph srcwebuisrcmainjavacomibmcicscipbankliberty["src/webui/src/main/java/com/ibm/cics/cip/bankliberty"]
8c557be70d5cffb48464db8c3fa20127229a94d6b7bf6c6bf7e22c79de855b0b(transferLocalExternal):::rootsStyle --> 784d93156fb26c57e93317f00d3c1bc048385c3433f1f88b0101fa5d65cbe069(transferLocalInternal)
end

subgraph srcwebuisrcmainjavacomibmcicscipbankliberty["src/webui/src/main/java/com/ibm/cics/cip/bankliberty"]
784d93156fb26c57e93317f00d3c1bc048385c3433f1f88b0101fa5d65cbe069(transferLocalInternal) --> 13975a5e665f46ae98687eb021e52bc4d72ae85492d9f841cfb6a5cb09ef6c0b(debitCredit)
end


      classDef mainFlowStyle color:#000000,fill:#7CB9F4
classDef rootsStyle color:#000000,fill:#00FFF4
classDef Style1 color:#000000,fill:#00FFAA
classDef Style2 color:#000000,fill:#FFFF00
classDef Style3 color:#000000,fill:#AA7CB9
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
