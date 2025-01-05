---
title: CustomerList Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="300:3:3" line-data="	public CustomerList()">`CustomerList`</SwmToken> class definition in the <SwmPath>[src/…/data_access/CustomerList.java](src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java)</SwmPath> file.

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="300:3:3" line-data="	public CustomerList()">`CustomerList`</SwmToken> class is responsible for managing a list of customers, including retrieving customer data from an external resource, filtering the data, and handling logging.

We will cover:

1. Initialization and logging setup
2. Retrieving customer data
3. Filtering customer data
4. Handling customer data

# Initialization and logging setup

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="300">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="300:3:3" line-data="	public CustomerList()">`CustomerList`</SwmToken> class initializes logging and sets up the sort code during construction. This ensures that logging is configured and the sort code is available for use in other methods.

```java
	public CustomerList()
	{
		sortOutLogging();
		CustomerList.setSortcode();
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="325">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="325:7:7" line-data="	private static void sortOutLogging()">`sortOutLogging`</SwmToken> method configures the logging system by reading the logging configuration.

```java
	private static void sortOutLogging()
	{
		try
		{
			LogManager.getLogManager().readConfiguration();
		}
		catch (SecurityException | IOException e)
		{
			logger.log(Level.SEVERE, e::toString);
		}
	}

}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="307">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="307:7:7" line-data="	private static void setSortcode()">`setSortcode`</SwmToken> method retrieves the sort code from an external resource if it is not already set.

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

# Retrieving customer data

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="149">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="149:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken> method retrieves customer data based on the provided limit, offset, and filter. It uses the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="152:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> class to fetch the data.

```java
	public void doGet(int limit, int offset, String filter) throws IOException
	{

		CustomerResource myCustomerResource = new CustomerResource();
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="67">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="67:5:5" line-data="	private void howMany(String filter)">`howMany`</SwmToken> method is called to determine the number of customers that match the filter criteria. This method handles different filter types and retrieves the appropriate customer data.

```java
	private void howMany(String filter)
	{

		CustomerResource myCustomerResource = new CustomerResource();
		Response myCustomerResponse = null;
```

---

</SwmSnippet>

# Filtering customer data

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="158">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="149:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken> method handles different filter types to retrieve customer data. For example, it checks if the filter is empty or if it starts with specific strings to determine the type of filter to apply.

```java
		try
		{
			if (filter.length() == 0)
			{
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="167">

---

&nbsp;

```java
			if (filter.startsWith(" AND CUSTOMER_NUMBER = "))
			{

				this.listOfCustomers.clear();
				String customerNumberFilter = filter.substring(23);
				Long customerNumber = Long.parseLong(customerNumberFilter);
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="178">

---

&nbsp;

```java
			if (filter.startsWith(" AND CUSTOMER_NAME like '"))
			{
				String customerNameFilter = filter.substring(25);
				customerNameFilter = customerNameFilter.substring(0,
						customerNameFilter.length() - 1);
```

---

</SwmSnippet>

# Handling customer data

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="194">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="149:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken> method processes the customer data retrieved from the external resource. It parses the JSON response and populates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="201:3:3" line-data="				this.listOfCustomers.clear();">`listOfCustomers`</SwmToken> with <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="222:1:1" line-data="						Customer myListCustomer = new Customer(id,">`Customer`</SwmToken> objects.

```java
			if (myCustomerResponse == null)
			{
				return;
			}
			if (myCustomerResponse.getStatus() == 200)
			{
				myCustomerString = myCustomerResponse.getEntity().toString();
				this.listOfCustomers.clear();

				JSONObject myCustomersJSON = JSONObject.parse(myCustomerString);
				JSONArray myCustomersArrayJSON = (JSONArray) myCustomersJSON
						.get(JSON_CUSTOMERS);
				long customerCount = 1;
				if (myCustomersArrayJSON != null)
				{
					customerCount = myCustomersArrayJSON.size();
					for (int i = 0; i < customerCount; i++)
					{
						JSONObject myCustomer = (JSONObject) myCustomersArrayJSON
								.get(i);
						Date dateOfBirth = sortOutDate(
								(String) myCustomer.get(JSON_DATE_OF_BIRTH));
						Date creditScoreReviewDate = sortOutDate(
								(String) myCustomer
										.get(JSON_CUSTOMER_REVIEW_DATE));

						String id = (String) myCustomer.get(JSON_ID);

						Customer myListCustomer = new Customer(id,
								(String) myCustomer.get(JSON_SORT_CODE),
								(String) myCustomer.get(JSON_CUSTOMER_NAME),
								(String) myCustomer.get(JSON_CUSTOMER_ADDRESS),
								dateOfBirth,
								(String) myCustomer
										.get(JSON_CUSTOMER_CREDIT_SCORE),
								creditScoreReviewDate);
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="271">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="271:5:5" line-data="	private Date sortOutDate(String dateString)">`sortOutDate`</SwmToken> method converts a date string into a <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="271:3:3" line-data="	private Date sortOutDate(String dateString)">`Date`</SwmToken> object. This method is used to handle date fields in the customer data.

```java
	private Date sortOutDate(String dateString)
	{
		String[] dateArray = dateString.split("-");
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="284">

---

&nbsp;

```java
		return new Date(myCalendar.getTimeInMillis());
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="288">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="288:5:5" line-data="	public Customer getCustomer(int i)">`getCustomer`</SwmToken> and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="209:7:7" line-data="					customerCount = myCustomersArrayJSON.size();">`size`</SwmToken> methods provide access to the customer list and its size.

```java
	public Customer getCustomer(int i)
	{
		return this.listOfCustomers.get(i);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="294">

---

&nbsp;

```java
	public int size()
	{
		return this.listOfCustomers.size();
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="54">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="57:5:5" line-data="	public int getCount(String filter)">`getCount`</SwmToken> method returns the number of customers that match the filter criteria. It calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="61:1:1" line-data="			howMany(filter);">`howMany`</SwmToken> method if the customer list is empty.

```java
	private static final String JSON_NUMBER_OF_CUSTOMERS = "numberOfCustomers";


	public int getCount(String filter)
	{
		if (this.listOfCustomers.isEmpty())
		{
			howMany(filter);
		}
		return this.count;
	}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
