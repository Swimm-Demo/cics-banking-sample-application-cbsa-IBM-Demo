---
title: Introduction to Web Components
---
## Data Access and Processing

# Introduction to Web Components

Web components are the building blocks of web-based interactions within the application. They encompass various classes and methods that manage data access and processing for web interfaces.

## Interaction with Databases

Web components play a crucial role in handling data access and processing. They interact with databases and other services to retrieve and manipulate data, ensuring seamless integration and functionality.

## Usage Example

The interaction with databases is a key aspect of web components. They include various classes and methods designed to manage data access and processing, facilitating efficient data handling for web interfaces.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" line="40">

---

An example of web component usage can be seen in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="402:3:3" line-data="	public Account getAccount(int accountNumber, int sortCode)">`Account`</SwmToken> class, which includes a method for creating accounts.

```java
	private static final String CREATE_ACCOUNT = "createAccount(AccountJSON account, Integer sortcode, boolean use NamedCounter)";
```

---

</SwmSnippet>

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="402:5:5" line-data="	public Account getAccount(int accountNumber, int sortCode)">`getAccount`</SwmToken>

# Web Endpoints

Web endpoints are specific functions that handle requests and responses for various operations within the application.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" line="402">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="402:5:5" line-data="	public Account getAccount(int accountNumber, int sortCode)">`getAccount`</SwmToken> endpoint retrieves account details based on the provided account number and sort code. This method is defined in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java" pos="402:3:3" line-data="	public Account getAccount(int accountNumber, int sortCode)">`Account`</SwmToken> class and is used to fetch account information from the database.

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

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" line="224">

---

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="224:7:7" line-data="	public ProcessedTransaction[] getProcessedTransactions(int sortCode,">`getProcessedTransactions`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="224:7:7" line-data="	public ProcessedTransaction[] getProcessedTransactions(int sortCode,">`getProcessedTransactions`</SwmToken> endpoint retrieves processed transactions based on the provided sort code, limit, and offset. This method is defined in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="224:3:3" line-data="	public ProcessedTransaction[] getProcessedTransactions(int sortCode,">`ProcessedTransaction`</SwmToken> class and is used to fetch transaction records from the database.

```java
	public ProcessedTransaction[] getProcessedTransactions(int sortCode,
			Integer limit, Integer offset)
	{
		logger.entering(this.getClass().getName(), GET_PROCESSED_TRANSACTIONS);

		ProcessedTransaction[] temp = new ProcessedTransaction[limit];

		this.offset = offset.intValue();
		this.limit = limit.intValue();

		StringBuilder myStringBuilder = new StringBuilder();

		for (int i = Integer.toString(sortCode).length(); i < SORT_CODE_LENGTH; i++)
		{
			myStringBuilder.append('0');
		}

		myStringBuilder.append(Integer.toString(sortCode));
		String sortCodeString = myStringBuilder.toString();

		openConnection();
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
