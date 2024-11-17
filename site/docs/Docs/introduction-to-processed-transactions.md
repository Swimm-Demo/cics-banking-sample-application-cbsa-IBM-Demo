---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: y7c7g81x
---
# Introduction to Processed Transactions

## Introduction to Processed Transactions

Processed transactions refer to transactions that have been completed and recorded in the system. The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="24:4:4" line-data="public class ProcessedTransaction extends HBankDataAccess">`ProcessedTransaction`</SwmToken> class is responsible for handling various operations related to these transactions, such as retrieving, writing, and managing transaction records.

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="24:4:4" line-data="public class ProcessedTransaction extends HBankDataAccess">`ProcessedTransaction`</SwmToken> Class

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="24:4:4" line-data="public class ProcessedTransaction extends HBankDataAccess">`ProcessedTransaction`</SwmToken> class includes methods for writing different types of transactions, such as debits, credits, transfers, and customer or account creation and deletion. It also includes methods for retrieving processed transactions from the database, processing transaction records, and managing transaction details like amount, description, and date. This class ensures that all transaction records are properly logged and stored in the database.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" line="24">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="24:4:4" line-data="public class ProcessedTransaction extends HBankDataAccess">`ProcessedTransaction`</SwmToken> class extends <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="24:8:8" line-data="public class ProcessedTransaction extends HBankDataAccess">`HBankDataAccess`</SwmToken>, indicating it inherits database access functionalities.

```java
public class ProcessedTransaction extends HBankDataAccess
{
```

---

</SwmSnippet>

### Writing Transactions

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="24:4:4" line-data="public class ProcessedTransaction extends HBankDataAccess">`ProcessedTransaction`</SwmToken> class provides several methods to write different types of transactions to the database. These methods ensure that the transaction details are correctly formatted and stored.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" line="510">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="510:5:5" line-data="	public boolean writeDebit(String accountNumber, String sortcode,">`writeDebit`</SwmToken> method writes a debit transaction to the database. It logs the transaction, prepares the SQL insert statement, and executes it to store the transaction details.

```java
	public boolean writeDebit(String accountNumber, String sortcode,
			BigDecimal amount2)
	{
		logger.entering(this.getClass().getName(), WRITE_DEBIT);

		sortOutDateTimeTaskString();

		openConnection();

		logger.log(Level.FINE, () -> ABOUT_TO_INSERT + SQL_INSERT + ">");
		try (PreparedStatement stmt = conn.prepareStatement(SQL_INSERT);)
		{
			stmt.setString(1, PROCTRAN.PROC_TRAN_VALID);
			stmt.setString(2, sortcode);
			stmt.setString(3,
					String.format("%08d", Integer.parseInt(accountNumber)));
			stmt.setString(4, dateString);
			stmt.setString(5, timeString);
			stmt.setString(6, taskRef);
			stmt.setString(7, PROCTRAN.PROC_TY_DEBIT);
			stmt.setString(8, "INTERNET WTHDRW");
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" line="546">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="35:14:14" line-data="	private static final String WRITE_CREDIT = &quot;writeCredit(String accountNumber, String sortcode, BigDecimal amount2)&quot;;">`writeCredit`</SwmToken> method logs and writes a credit transaction to the database. It prepares the SQL insert statement and executes it to store the transaction details.

```java
			BigDecimal amount2)
	{
		logger.entering(this.getClass().getName(), WRITE_CREDIT, false);
		sortOutDateTimeTaskString();

		openConnection();

		logger.log(Level.FINE, () -> ABOUT_TO_INSERT + SQL_INSERT + ">");
		try (PreparedStatement stmt = conn.prepareStatement(SQL_INSERT);)
		{
			stmt.setString(1, PROCTRAN.PROC_TRAN_VALID);
			stmt.setString(2, sortcode);
			stmt.setString(3,
					String.format("%08d", Integer.parseInt(accountNumber)));
			stmt.setString(4, dateString);
			stmt.setString(5, timeString);
			stmt.setString(6, taskRef);
			stmt.setString(7, PROCTRAN.PROC_TY_CREDIT);
			stmt.setString(8, "INTERNET RECVED");
			stmt.setBigDecimal(9, amount2);
			stmt.executeUpdate();
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" line="579">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="579:5:5" line-data="	public boolean writeTransferLocal(String sortCode2, String accountNumber2,">`writeTransferLocal`</SwmToken> method logs and writes a local transfer transaction to the database. It prepares the SQL insert statement and executes it to store the transaction details, including the target account and sort code.

```java
	public boolean writeTransferLocal(String sortCode2, String accountNumber2,
			BigDecimal amount2, String targetAccountNumber2)
	{
		logger.entering(this.getClass().getName(), WRITE_TRANSFER_LOCAL);

		sortOutDateTimeTaskString();

		String transferDescription = "";
		transferDescription = transferDescription
				+ PROCTRAN.PROC_TRAN_DESC_XFR_FLAG;
		transferDescription = transferDescription.concat("                  ");

		transferDescription = transferDescription
				.concat(padSortCode(Integer.parseInt(sortCode2)));

		transferDescription = transferDescription.concat(
				padAccountNumber(Integer.parseInt(targetAccountNumber2)));

		openConnection();

		logger.log(Level.FINE, () -> ABOUT_TO_INSERT + SQL_INSERT + ">");
```

---

</SwmSnippet>

### Retrieving Transactions

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="24:4:4" line-data="public class ProcessedTransaction extends HBankDataAccess">`ProcessedTransaction`</SwmToken> class also provides methods to retrieve processed transactions from the database. These methods construct SQL queries to fetch the relevant records and process each transaction to set its details.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" line="224">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="224:7:7" line-data="	public ProcessedTransaction[] getProcessedTransactions(int sortCode,">`getProcessedTransactions`</SwmToken> method retrieves processed transactions from the database based on the provided sort code, limit, and offset. It constructs a SQL query to fetch the relevant records and processes each transaction to set its details.

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

### Processing Transfer Records

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="412:5:5" line-data="	private ProcessedTransaction processTransferRecord(">`processTransferRecord`</SwmToken> method processes transfer records by setting appropriate flags and details. This ensures that transfer transactions are correctly identified and managed.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" line="412">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/ProcessedTransaction.java" pos="412:5:5" line-data="	private ProcessedTransaction processTransferRecord(">`processTransferRecord`</SwmToken> method processes transfer records by setting appropriate flags and details. If the transaction type is a transfer, it sets the target account number and sort code, and marks the transaction as a transfer.

```java
	private ProcessedTransaction processTransferRecord(
			ProcessedTransaction processedTransaction)
	{
		// If we're a "Transfer between accounts" record, set flags
		// appropriately
		if (processedTransaction.getType().compareTo("TFR") == 0)
		{
			String targetSortcodeInRecord = processedTransaction
					.getDescription().substring(26, 32);
			String targetAccountInRecord = processedTransaction.getDescription()
					.substring(32, 40);

			processedTransaction.setTargetAccountNumber(targetAccountInRecord);
			processedTransaction.setTargetSortcode(targetSortcodeInRecord);
			processedTransaction.setTransfer(true);
		}
		return processedTransaction;
	}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
