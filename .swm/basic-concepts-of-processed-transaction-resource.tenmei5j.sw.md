---
title: Basic Concepts of Processed Transaction Resource
---
# Overview

The Processed Transaction Resource is a class that provides methods to handle processed transactions. It is annotated with the path <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="38:4:5" line-data="@Path(&quot;/processedTransaction&quot;)">`/processedTransaction`</SwmToken> and produces JSON responses. The class includes methods to retrieve and manipulate processed transaction data, such as getting processed transactions, debiting or crediting accounts, transferring funds, and creating or deleting customers and accounts.

# Methods in Processed Transaction Resource

The class includes several methods to handle different types of transactions. These methods are designed to retrieve, process, and manipulate transaction data, and they produce JSON responses containing the transaction details.

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="96:5:5" line-data="	public Response getProcessedTransactionExternal(">`getProcessedTransactionExternal`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="96:5:5" line-data="	public Response getProcessedTransactionExternal(">`getProcessedTransactionExternal`</SwmToken> method retrieves processed transactions based on a limit and offset. It calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="100:7:7" line-data="		Response myResponse = getProcessedTransactionInternal(limit, offset);">`getProcessedTransactionInternal`</SwmToken> method to perform the actual data retrieval.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" line="94">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="96:5:5" line-data="	public Response getProcessedTransactionExternal(">`getProcessedTransactionExternal`</SwmToken> method is defined here. It retrieves processed transactions based on a limit and offset, and calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="100:7:7" line-data="		Response myResponse = getProcessedTransactionInternal(limit, offset);">`getProcessedTransactionInternal`</SwmToken> method to perform the actual data retrieval.

```java
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getProcessedTransactionExternal(
			@QueryParam(LIMIT) Integer limit,
			@QueryParam(OFFSET) Integer offset)
	{
		Response myResponse = getProcessedTransactionInternal(limit, offset);
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		return myResponse;
	}
```

---

</SwmSnippet>

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="100:7:7" line-data="		Response myResponse = getProcessedTransactionInternal(limit, offset);">`getProcessedTransactionInternal`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="100:7:7" line-data="		Response myResponse = getProcessedTransactionInternal(limit, offset);">`getProcessedTransactionInternal`</SwmToken> method fetches processed transactions from the database, processes each transaction, and constructs a JSON response containing the transaction details.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" line="107">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="107:5:5" line-data="	public Response getProcessedTransactionInternal(">`getProcessedTransactionInternal`</SwmToken> method is defined here. It fetches processed transactions from the database, processes each transaction, and constructs a JSON response containing the transaction details.

```java
	public Response getProcessedTransactionInternal(
			@QueryParam(LIMIT) Integer limit,
			@QueryParam(OFFSET) Integer offset)
	{

		if (offset == null)
		{
			offset = 0;
		}
		if (limit == null)
		{
			limit = 250000;
		}
		JSONObject response = new JSONObject();
		JSONArray processedTransactionsJSON = null;
		int numberOfProcessedTransactions = 0;
		Integer sortCode = this.getSortCode();

		com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction myProcessedTransaction = new com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction();
		com.ibm.cics.cip.bankliberty.web.db2.ProcessedTransaction[] processedTransactions = null;
```

---

</SwmSnippet>

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="183:5:5" line-data="	private JSONObject processTransfer(JSONObject proctran,">`processTransfer`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="183:5:5" line-data="	private JSONObject processTransfer(JSONObject proctran,">`processTransfer`</SwmToken> method processes bank-to-bank transfer records and adds relevant details to the JSON response.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" line="183">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="183:5:5" line-data="	private JSONObject processTransfer(JSONObject proctran,">`processTransfer`</SwmToken> method is defined here. It processes bank-to-bank transfer records and adds relevant details to the JSON response.

```java
	private JSONObject processTransfer(JSONObject proctran,
			ProcessedTransaction processedTransaction)
	{
		// Process bank to bank transfer records
		if (processedTransaction.getType()
				.compareTo(PROCTRAN.PROC_TY_TRANSFER) == 0)
		{
			proctran.put(JSON_TARGET_ACCOUNT,
					processedTransaction.getTargetAccountNumber());
			proctran.put(JSON_TARGET_SORT_CODE,
					processedTransaction.getTargetSortcode());
		}
		return proctran;
	}
```

---

</SwmSnippet>

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="199:5:5" line-data="	private JSONObject processDeleteCreateCustomer(JSONObject proctran,">`processDeleteCreateCustomer`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="199:5:5" line-data="	private JSONObject processDeleteCreateCustomer(JSONObject proctran,">`processDeleteCreateCustomer`</SwmToken> method deals with creating and deleting customer records and adds relevant details to the JSON response.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" line="199">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="199:5:5" line-data="	private JSONObject processDeleteCreateCustomer(JSONObject proctran,">`processDeleteCreateCustomer`</SwmToken> method is defined here. It deals with creating and deleting customer records and adds relevant details to the JSON response.

```java
	private JSONObject processDeleteCreateCustomer(JSONObject proctran,
			ProcessedTransaction processedTransaction, DateFormat myDateFormat)
	{
		// Deal with create account and delete customer
		if (processedTransaction.getType()
				.compareTo(PROCTRAN.PROC_TY_BRANCH_DELETE_CUSTOMER) == 0
				|| processedTransaction.getType()
						.compareTo(PROCTRAN.PROC_TY_WEB_DELETE_CUSTOMER) == 0
				|| (processedTransaction.getType()
						.compareTo(PROCTRAN.PROC_TY_BRANCH_CREATE_CUSTOMER) == 0
						|| processedTransaction.getType().compareTo(
								PROCTRAN.PROC_TY_WEB_CREATE_CUSTOMER) == 0))
		{
			proctran.put(JSON_DATE_OF_BIRTH,
					myDateFormat.format(processedTransaction.getDateOfBirth()));
			proctran.put(JSON_CUSTOMER_NAME,
					processedTransaction.getCustomerName());
			proctran.put(JSON_CUSTOMER, processedTransaction.getCustomer());
		}
		return proctran;
	}
```

---

</SwmSnippet>

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="222:5:5" line-data="	private JSONObject processDeleteCreateAccount(JSONObject proctran,">`processDeleteCreateAccount`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="222:5:5" line-data="	private JSONObject processDeleteCreateAccount(JSONObject proctran,">`processDeleteCreateAccount`</SwmToken> method deals with creating and deleting account records and adds relevant details to the JSON response.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" line="222">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/ProcessedTransactionResource.java" pos="222:5:5" line-data="	private JSONObject processDeleteCreateAccount(JSONObject proctran,">`processDeleteCreateAccount`</SwmToken> method is defined here. It deals with creating and deleting account records and adds relevant details to the JSON response.

```java
	private JSONObject processDeleteCreateAccount(JSONObject proctran,
			ProcessedTransaction processedTransaction, DateFormat myDateFormat)
	{
		// Deal with create account and delete account
		if (processedTransaction.getType()
				.compareTo(PROCTRAN.PROC_TY_BRANCH_DELETE_ACCOUNT) == 0
				|| processedTransaction.getType()
						.compareTo(PROCTRAN.PROC_TY_WEB_DELETE_ACCOUNT) == 0
				|| processedTransaction.getType()
						.compareTo(PROCTRAN.PROC_TY_BRANCH_CREATE_ACCOUNT) == 0
				|| processedTransaction.getType()
						.compareTo(PROCTRAN.PROC_TY_WEB_CREATE_ACCOUNT) == 0)
		{
			proctran.put(JSON_ACCOUNT_TYPE,
					processedTransaction.getAccountType());
			proctran.put(JSON_LAST_STATEMENT, myDateFormat
					.format(processedTransaction.getLastStatement()));
			proctran.put(JSON_NEXT_STATEMENT, myDateFormat
					.format(processedTransaction.getNextStatement()));
			proctran.put(JSON_CUSTOMER, processedTransaction.getCustomer());
		}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
