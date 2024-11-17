---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: eyaiassh
---
# Customer List Feature Overview

## Overview

The Customer List in the WebUI is a feature that allows users to view and manage a list of customers. It retrieves customer data from the backend and displays it in a user-friendly format. The Customer List can be filtered by customer name or customer number to narrow down the search results. It also provides functionality to count the number of customers that match the given filter criteria.

## Implementation

The Customer List is implemented in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="25:4:4" line-data="public class CustomerList">`CustomerList`</SwmToken> class, which handles the data retrieval and processing. The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="25:4:4" line-data="public class CustomerList">`CustomerList`</SwmToken> class uses the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="70:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> class to fetch customer data from external sources. The retrieved customer data is stored in a list of <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="32:5:5" line-data="	private List&lt;Customer&gt; listOfCustomers = new ArrayList&lt;&gt;();">`Customer`</SwmToken> objects, which can be accessed and manipulated as needed.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="25">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="25:4:4" line-data="public class CustomerList">`CustomerList`</SwmToken> class handles the data retrieval and processing for the Customer List feature. It uses the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="70:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> class to fetch customer data from external sources and stores the retrieved data in a list of <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="32:5:5" line-data="	private List&lt;Customer&gt; listOfCustomers = new ArrayList&lt;&gt;();">`Customer`</SwmToken> objects.

```java
public class CustomerList
{


	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.webui.dataAccess");

	private List<Customer> listOfCustomers = new ArrayList<>();

	private static String sortcode = null;

	private int count;

	private static final String JSON_SORT_CODE = "sortCode";

	private static final String JSON_ID = "id";

	private static final String JSON_CUSTOMER_NAME = "customerName";

	private static final String JSON_CUSTOMER_ADDRESS = "customerAddress";
```

---

</SwmSnippet>

## Main Functions

There are several main functions in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="25:4:4" line-data="public class CustomerList">`CustomerList`</SwmToken> class. Some of them are <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="57:5:5" line-data="	public int getCount(String filter)">`getCount`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="61:1:1" line-data="			howMany(filter);">`howMany`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="149:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="288:5:5" line-data="	public Customer getCustomer(int i)">`getCustomer`</SwmToken>, and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="294:5:5" line-data="	public int size()">`size`</SwmToken>. Below, we will dive a little into <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="57:5:5" line-data="	public int getCount(String filter)">`getCount`</SwmToken> and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="61:1:1" line-data="			howMany(filter);">`howMany`</SwmToken>.

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="57:5:5" line-data="	public int getCount(String filter)">`getCount`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="57:5:5" line-data="	public int getCount(String filter)">`getCount`</SwmToken> function is used to get the count of customers that match a given filter. It calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="61:1:1" line-data="			howMany(filter);">`howMany`</SwmToken> function if the list of customers is empty.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="57">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="57:5:5" line-data="	public int getCount(String filter)">`getCount`</SwmToken> function checks if the list of customers is empty and calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="61:1:1" line-data="			howMany(filter);">`howMany`</SwmToken> function to get the count of customers that match the given filter.

```java
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

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="61:1:1" line-data="			howMany(filter);">`howMany`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="61:1:1" line-data="			howMany(filter);">`howMany`</SwmToken> function is used to determine the number of customers that match a given filter. It interacts with the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="70:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> class to fetch customer data based on the filter criteria.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="67">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="67:5:5" line-data="	private void howMany(String filter)">`howMany`</SwmToken> function uses the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="70:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> class to fetch customer data based on the filter criteria and processes the response to determine the number of matching customers.

```java
	private void howMany(String filter)
	{

		CustomerResource myCustomerResource = new CustomerResource();
		Response myCustomerResponse = null;

		// 0123456789012345678901234

		try
		{
			if (filter.startsWith(" AND CUSTOMER_NAME like '"))
			{

				String customerNameFilter = filter.substring(25);
				customerNameFilter = customerNameFilter.substring(0,
						customerNameFilter.length() - 1);

				myCustomerResponse = myCustomerResource
						.getCustomersByNameExternal(customerNameFilter, 0, 0,
								true);
				String myCustomersString = myCustomerResponse.getEntity()
```

---

</SwmSnippet>

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="149:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="149:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken> function retrieves a list of customers based on the provided limit, offset, and filter. It uses the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="70:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> class to fetch customer data and processes the response to populate the list of customers.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="149">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="149:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken> function interacts with the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="152:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> class to fetch customer data based on the provided limit, offset, and filter, and processes the response to populate the list of customers.

```java
	public void doGet(int limit, int offset, String filter) throws IOException
	{

		CustomerResource myCustomerResource = new CustomerResource();

		Response myCustomerResponse = null;

		String myCustomerString = null;

		try
		{
			if (filter.length() == 0)
			{

				myCustomerResponse = myCustomerResource
						.getCustomersExternal(limit, offset, false);

			}
			if (filter.startsWith(" AND CUSTOMER_NUMBER = "))
			{
```

---

</SwmSnippet>

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="288:5:5" line-data="	public Customer getCustomer(int i)">`getCustomer`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="288:5:5" line-data="	public Customer getCustomer(int i)">`getCustomer`</SwmToken> function returns a customer object from the list of customers based on the provided index.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="288">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="288:5:5" line-data="	public Customer getCustomer(int i)">`getCustomer`</SwmToken> function retrieves a customer object from the list of customers based on the provided index.

```java
	public Customer getCustomer(int i)
	{
		return this.listOfCustomers.get(i);
	}
```

---

</SwmSnippet>

### size

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="294:5:5" line-data="	public int size()">`size`</SwmToken> function returns the size of the list of customers.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="294">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="294:5:5" line-data="	public int size()">`size`</SwmToken> function returns the number of customers in the list.

```java
	public int size()
	{
		return this.listOfCustomers.size();
	}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
