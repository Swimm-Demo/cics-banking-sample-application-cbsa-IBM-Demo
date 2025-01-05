---
title: AccountList Java Class Definition
---
# Introduction

This document will walk you through the `AccountList` Java class definition.

The `AccountList` class is responsible for managing a list of bank accounts, including retrieving account data from external resources and filtering it based on various criteria.

We will cover:

1. Class properties and initialization
2. Retrieving and filtering account data
3. Handling account data and logging

# Class properties and initialization

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="29">

---

The `AccountList` class starts by defining its properties and initializing the logger and account list.

```java
	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.webui.dataAccess");

	private List<Account> listOfAccounts = new ArrayList<>();
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="361">

---

The constructor initializes logging and sets the sort code.

```java
	public AccountList()
	{
		sortOutLogging();
		AccountList.setSortcode();
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="368">

---

The `setSortcode` method retrieves the sort code from an external resource if it is not already set.

```java
	private static void setSortcode()
	{
		if (sortcode == null)
		{
			SortCodeResource mySortCodeResource = new SortCodeResource();
			Response mySortCodeJSON = mySortCodeResource.getSortCode();
			sortcode = ((String) mySortCodeJSON.getEntity()).substring(13, 19);
		}

	}
```

---

</SwmSnippet>

# Retrieving and filtering account data

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="62">

---

The `getCount` method returns the count of accounts based on a filter. If the account list is empty, it calls the `howMany` method to populate the count.

```java
	private static final String JSON_ID = "id";


	public int getCount(String filter)
	{
		if (this.listOfAccounts.isEmpty())
		{
			howMany(filter);
		}
		return this.count;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="75">

---

The `howMany` method handles different filters to retrieve account data from external resources.

```java
	public int howMany(String filter)
	{

		// AND ACCOUNT_NUMBER = 0000000024
		// AND ACCOUNT_CUSTOMER_NUMBER
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="92">

---

For example, it processes filters related to account balance:

```java
				String operator = filter.substring(31, 32);
				BigDecimal balance = BigDecimal
						.valueOf(Double.parseDouble(filter.substring(34)));
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="101">

---

And filters related to account number:

```java
			if (filter.contains("AND ACCOUNT_NUMBER"))
			{
				String accountNumberFilter = filter.substring(22);
				if (accountNumberFilter.indexOf(' ') >= 0)
				{
					accountNumberFilter = accountNumberFilter.substring(0,
							accountNumberFilter.indexOf(' '));
				}
				Long accountNumberFilterLong = Long
						.parseLong(accountNumberFilter);
				myAccountsResponse = myAccountsResource
						.getAccountExternal(accountNumberFilterLong);
			}
```

---

</SwmSnippet>

# Handling account data and logging

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="167">

---

The `doGet` method retrieves account data with pagination and filtering. It clears the account list before fetching new data.

```java
	public void doGet(int limit, int offset, String filter) throws IOException
	{

		AccountsResource myAccountsResource = new AccountsResource();
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="176">

---

It processes filters related to account balance:

```java
		try
		{

			if (filter.contains(" AND ACCOUNT_AVAILABLE_BALANCE"))
			{
				this.listOfAccounts.clear();
				String operator = filter.substring(31, 32);
				BigDecimal balance = BigDecimal
						.valueOf(Double.parseDouble(filter.substring(34)));
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="203">

---

And filters related to customer number:

```java
			// 0123456789012345678901234567890
			if (filter.contains(" AND ACCOUNT_CUSTOMER_NUMBER = "))
			{
				this.listOfAccounts.clear();
				String customerNumberFilter = filter.substring(31);
				Long customerNumber = Long.parseLong(customerNumberFilter);
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="392">

---

The `sortOutDate` method converts a date string into a `Date` object.

```java
	private Date sortOutDate(String dateString)
	{
		String[] dateArray = dateString.split("-");
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="410">

---

The `sortOutLogging` method configures the logging settings.

```java
	private static void sortOutLogging()
	{
		try
		{
			LogManager.getLogManager().readConfiguration();
		}
		catch (SecurityException | IOException e)
		{
			logger.severe(e.toString());
		}
	}

}
```

---

</SwmSnippet>

This document covered the main ideas of the `AccountList` class, focusing on its properties, methods for retrieving and filtering account data, and handling logging.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](http://localhost:5000/)</sup></SwmMeta>
