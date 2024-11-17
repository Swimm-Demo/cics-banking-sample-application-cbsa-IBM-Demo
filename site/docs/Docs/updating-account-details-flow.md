---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: bkbtl1i9
---
# Updating Account Details Flow

In this document, we will explain the process of updating an account's details. The process involves validating the account details, retrieving the account from the database, updating the account details, and handling the database connection.

The flow starts with validating the account details provided by the user. If the details are valid, the system retrieves the account from the database. Once the account is found, the system updates the account details and saves them back to the database. Throughout this process, the system ensures that the database connection is properly managed.

## Flow drill down

```mermaid
graph TD;
      subgraph srcwebuisrcmainjavacomibmcicscipbanklibertywebdb2Accountjava["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java"]
659ae52dac99528690aec49762355c1b5ce7ba8c60b88f17dff82c3976e37980(updateAccountInternal) --> 36796487795e009196a5bf943bc42edc2709d69f47e54cbecd4678d8f5153c26(updateAccount)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertywebdb2Accountjava["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java"]
36796487795e009196a5bf943bc42edc2709d69f47e54cbecd4678d8f5153c26(updateAccount) --> cb5c98c0144af37ade4635cc742aafdb2b750e19319280e1a5f441154aad213f(getAccount)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
cb5c98c0144af37ade4635cc742aafdb2b750e19319280e1a5f441154aad213f(getAccount) --> 4ccb7e7f1ffffa4e90c42aeedb17171c5fac78dfdb55c155b9b79af1f1797072(openConnection)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
4ccb7e7f1ffffa4e90c42aeedb17171c5fac78dfdb55c155b9b79af1f1797072(openConnection) --> a45bfb1a8c2dd416e124b8a4fc5988afb239da44caac19057d872f11b7c78c0c(openConnectionInternal)
end


      classDef mainFlowStyle color:#000000,fill:#7CB9F4
classDef rootsStyle color:#000000,fill:#00FFF4
classDef Style1 color:#000000,fill:#00FFAA
classDef Style2 color:#000000,fill:#FFFF00
classDef Style3 color:#000000,fill:#AA7CB9
```

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" line="652">

---

### Validating Account Details

First, the function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" pos="65:14:14" line-data="	private static final String UPDATE_ACCOUNT_INTERNAL = &quot;updateAccountInternal(Long id, AccountJSON account)&quot;;">`updateAccountInternal`</SwmToken> validates the account details provided in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="932:7:7" line-data="	public Account updateAccount(AccountJSON account)">`AccountJSON`</SwmToken> object. It checks if the account type is valid, if the interest rate is within acceptable bounds, and if the sort code matches the bank's sort code. If any of these validations fail, an appropriate error response is returned.

```java
		if (!(account.validateType(account.getAccountType().trim())))
		// If account type invalid
		{
			JSONObject error = new JSONObject();
			error.put(JSON_ERROR_MSG,
					ACC_TYPE_STRING + account.getAccountType() + NOT_SUPPORTED);
			logger.log(Level.WARNING, () -> (ACC_TYPE_STRING
					+ account.getAccountType() + NOT_SUPPORTED));
			myResponse = Response.status(400).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(), UPDATE_ACCOUNT_INTERNAL,
					myResponse);
			return myResponse;
		}

		if (account.getInterestRate().doubleValue() < 0.00)
		{
			// If interest rate < 0
			JSONObject error = new JSONObject();
			error.put(JSON_ERROR_MSG, INTEREST_RATE_LESS_THAN_ZERO);
			logger.log(Level.WARNING, () -> (INTEREST_RATE_LESS_THAN_ZERO));
			myResponse = Response.status(400).entity(error.toString()).build();
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" line="725">

---

### Retrieving and Updating the Account

Next, the function retrieves the account from the database using the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" pos="727:7:7" line-data="		db2Account = db2Account.getAccount(Integer.parseInt(account.getId()),">`getAccount`</SwmToken> method. If the account exists, it updates the account details using the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" pos="732:7:7" line-data="			db2Account = db2Account.updateAccount(account);">`updateAccount`</SwmToken> method. If the update is successful, a response with the updated account details is returned. If the account does not exist or the update fails, an appropriate error response is returned.

```java
		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new com.ibm.cics.cip.bankliberty.web.db2.Account();
		account.setId(id.toString());
		db2Account = db2Account.getAccount(Integer.parseInt(account.getId()),
				this.getSortCode().intValue());

		if (db2Account != null)
		{
			db2Account = db2Account.updateAccount(account);
			if (db2Account != null)
			{
				response.put(JSON_SORT_CODE, db2Account.getSortcode().trim());
				response.put("id", db2Account.getAccountNumber());
				response.put(JSON_CUSTOMER_NUMBER,
						db2Account.getCustomerNumber());
				response.put(JSON_ACCOUNT_TYPE, db2Account.getType().trim());
				response.put(JSON_AVAILABLE_BALANCE,
						BigDecimal.valueOf(db2Account.getAvailableBalance()));
				response.put(JSON_ACTUAL_BALANCE,
						BigDecimal.valueOf(db2Account.getActualBalance()));
				response.put(JSON_INTEREST_RATE,
						BigDecimal.valueOf(db2Account.getInterestRate()));
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" line="932">

---

#### Updating Account in Database

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="932:5:5" line-data="	public Account updateAccount(AccountJSON account)">`updateAccount`</SwmToken> method in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="932:3:3" line-data="	public Account updateAccount(AccountJSON account)">`Account`</SwmToken> class is responsible for updating the account details in the database. It first retrieves the account using the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="935:9:9" line-data="		Account db2Account = this.getAccount(Integer.parseInt(account.getId()),">`getAccount`</SwmToken> method. If the account is found, it prepares and executes an SQL update statement to update the account type, interest rate, and overdraft limit.

```java
	public Account updateAccount(AccountJSON account)
	{
		logger.entering(this.getClass().getName(), UPDATE_ACCOUNT);
		Account db2Account = this.getAccount(Integer.parseInt(account.getId()),
				Integer.parseInt(sortcode));
		if (db2Account == null)
		{
			logger.log(Level.WARNING,
					() -> "Unable to access DB2 account " + account.getId());
			logger.exiting(this.getClass().getName(), UPDATE_ACCOUNT, null);
			return null;
		}
		Account temp = null;
		openConnection();
		String accountNumberString = padAccountNumber(
				Integer.parseInt(db2Account.getAccountNumber()));

		String sortCodeString = padSortCode(Integer.valueOf(sortcode));
		String sql1 = SQL_SELECT;
		logger.log(Level.FINE,
				() -> "About to perform query SQL <" + sql1 + ">");
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" line="402">

---

#### Retrieving Account from Database

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="402:5:5" line-data="	public Account getAccount(int accountNumber, int sortCode)">`getAccount`</SwmToken> method retrieves the account details from the database based on the account number and sort code. It executes an SQL query to fetch the account details and returns an <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="402:3:3" line-data="	public Account getAccount(int accountNumber, int sortCode)">`Account`</SwmToken> object if the account is found.

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

#### Opening Database Connection

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="69:5:5" line-data="	protected void openConnection()">`openConnection`</SwmToken> method opens a connection to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="71:13:13" line-data="		// Open a connection to the DB2 database">`DB2`</SwmToken> database. It checks if a connection already exists for the current task. If not, it calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="87:1:1" line-data="			openConnectionInternal();">`openConnectionInternal`</SwmToken> method to create a new connection.

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

#### Creating Database Connection

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="163:3:3" line-data="	void openConnectionInternal()">`openConnectionInternal`</SwmToken> method creates a new connection to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="178:17:17" line-data="						() -&gt; &quot;About to attempt to get DB2 connection&quot;);">`DB2`</SwmToken> database using a JNDI lookup. It sets the transaction isolation level and stores the connection in a map for reuse.

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

## Where is this flow used?

This flow is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD;
      subgraph srcwebuisrcmainjavacomibmcicscipbankliberty["src/webui/src/main/java/com/ibm/cics/cip/bankliberty"]
9fba9eb2ea59f01a8aab904dd35b0442d2f47d11ea76a5d62379cb018ada9b33(updateThis):::rootsStyle --> 659ae52dac99528690aec49762355c1b5ce7ba8c60b88f17dff82c3976e37980(updateAccountInternal)
end

subgraph srcwebuisrcmainjavacomibmcicscipbankliberty["src/webui/src/main/java/com/ibm/cics/cip/bankliberty"]
1dd989ecbebebd74e5091f7e3c6917744ddc0c5fa647e46ef1707e302a081449(updateAccountExternal):::rootsStyle --> 659ae52dac99528690aec49762355c1b5ce7ba8c60b88f17dff82c3976e37980(updateAccountInternal)
end


      classDef mainFlowStyle color:#000000,fill:#7CB9F4
classDef rootsStyle color:#000000,fill:#00FFF4
classDef Style1 color:#000000,fill:#00FFAA
classDef Style2 color:#000000,fill:#FFFF00
classDef Style3 color:#000000,fill:#AA7CB9
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
