---
title: Customer Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="61:3:3" line-data="	public Customer(String custNo, String sortCode, String name, String address,">`Customer`</SwmToken> class definition in the <SwmPath>[src/…/data_access/Customer.java](src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java)</SwmPath> file.

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="61:3:3" line-data="	public Customer(String custNo, String sortCode, String name, String address,">`Customer`</SwmToken> class is designed to manage customer data, including creation, updating, and deletion of customer records. It also handles logging and data formatting.

We will cover:

1. Class properties and their significance.
2. Constructors for creating and editing customers.
3. Methods for accessing and modifying customer data.
4. Methods for database operations.
5. Utility methods for logging and date formatting.

# Class properties

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="24">

---

The class properties store customer-related data. These properties are used throughout the class to manage customer information.

```java
	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.webui.dataAccess");

	private static final String JSON_SORT_CODE = "sortCode";

	private static final String JSON_ID = "id";

	private static final String JSON_CUSTOMER_NAME = "customerName";

	private static final String JSON_CUSTOMER_ADDRESS = "customerAddress";

	private static final String JSON_CUSTOMER_CREDIT_SCORE = "customerCreditScore";

	private static final String JSON_CUSTOMER_REVIEW_DATE = "customerCreditScoreReviewDate";

	private static final String JSON_DATE_OF_BIRTH = "dateOfBirth";

	private static final String DASHES = "------------";

	private String customerNumber;

	private String sortcode;

	private String name;

	private String address;

	private Date dob;

	private String creditScore;

	private Date creditScoreReviewDate;
```

---

</SwmSnippet>

# Constructors

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="57">

---

The constructors initialize the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="61:3:3" line-data="	public Customer(String custNo, String sortCode, String name, String address,">`Customer`</SwmToken> object. There are two constructors: one for creating a new customer and another for editing an existing customer.

```java
	private Boolean editingCustomer;


	// NEW CUSTOMER
	public Customer(String custNo, String sortCode, String name, String address,
			Date dob)
	{
		sortOutLogging();
		editingCustomer = false;
		setCustomerNumber(custNo);
		setSortcode(sortCode);
		setName(name);
		setAddress(address);
		setDob(dob);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="74">

---

&nbsp;

```java
	// EDITING CUSTOMER
	public Customer(String custNo, String sortCode, String name, String address,
			Date dob, String creditScore, Date creditScoreReviewDate)
	{
		sortOutLogging();
		editingCustomer = true;
		setCustomerNumber(custNo);
		setSortcode(sortCode);
		setName(name);
		setAddress(address);
		setDob(dob);
		setCreditScore(creditScore);
		setCreditScoreReviewDate(creditScoreReviewDate);
	}
```

---

</SwmSnippet>

# Accessor and mutator methods

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="90">

---

These methods provide access to and modification of the customer properties. They are essential for encapsulating the data and ensuring that it can be accessed and modified safely.

```java
	public String getCustomerNumber()
	{
		return customerNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="96">

---

&nbsp;

```java
	public void setCustomerNumber(String custNo)
	{
		this.customerNumber = custNo;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="102">

---

&nbsp;

```java
	public String getSortcode()
	{
		return sortcode;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="108">

---

&nbsp;

```java
	public void setSortcode(String sortcode)
	{
		this.sortcode = sortcode;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="114">

---

&nbsp;

```java
	public String getName()
	{
		return name;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="120">

---

&nbsp;

```java
	public void setName(String name)
	{
		this.name = name;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="126">

---

&nbsp;

```java
	public String getAddress()
	{
		return address;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="132">

---

&nbsp;

```java
	public void setAddress(String address)
	{
		this.address = address;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="138">

---

&nbsp;

```java
	public Date getDob()
	{
		return dob;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="144">

---

&nbsp;

```java
	public void setDob(Date dob)
	{
		this.dob = dob;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="150">

---

&nbsp;

```java
	public String getCreditScore()
	{
		return creditScore;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="156">

---

&nbsp;

```java
	public void setCreditScore(String creditScore)
	{
		this.creditScore = creditScore;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="162">

---

&nbsp;

```java
	public Date getCreditScoreReviewDate()
	{
		return creditScoreReviewDate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="168">

---

&nbsp;

```java
	public void setCreditScoreReviewDate(Date creditScoreReviewDate)
	{
		this.creditScoreReviewDate = creditScoreReviewDate;
	}
```

---

</SwmSnippet>

# Database operations

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="174">

---

These methods handle interactions with the database, including updating, deleting, and adding customer records. They use the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="176:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="178:1:1" line-data="		CustomerJSON myCustomerJSON = new CustomerJSON();">`CustomerJSON`</SwmToken> classes to perform these operations.

```java
	public boolean updateThis()
	{
		CustomerResource myCustomerResource = new CustomerResource();

		CustomerJSON myCustomerJSON = new CustomerJSON();

		myCustomerJSON.setCustomerAddress(this.getAddress());
		myCustomerJSON.setCustomerName(this.getName());
		myCustomerJSON.setSortCode(this.getSortcode());
		myCustomerJSON.setSortCode(this.getSortcode());

		Response myCustomerResponse = myCustomerResource.updateCustomerExternal(
				Long.parseLong(this.getCustomerNumber()), myCustomerJSON);

		String myCustomerString = null;
		JSONObject myCustomer = null;

		if (myCustomerResponse.getStatus() == 200)
		{
			myCustomerString = myCustomerResponse.getEntity().toString();
			try
			{
				myCustomer = JSONObject.parse(myCustomerString);
			}
			catch (IOException e)
			{
				return false;
			}

			this.setDob(
					sortOutDate((String) myCustomer.get(JSON_DATE_OF_BIRTH)));
			this.setAddress((String) myCustomer.get(JSON_CUSTOMER_ADDRESS));
			this.setName((String) myCustomer.get(JSON_CUSTOMER_NAME));
			this.setSortcode((String) myCustomer.get(JSON_SORT_CODE));
			String customerNoString = (String) myCustomer.get(JSON_ID);
			this.setCustomerNumber(customerNoString);
		}
		else
		{
			return false;
		}

		return true;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="220">

---

&nbsp;

```java
	public boolean deleteFromDB()
	{
		CustomerResource myCustomerResource = new CustomerResource();

		Response myCustomerResponse = myCustomerResource.deleteCustomerExternal(
				Long.parseLong(this.getCustomerNumber()));

		String myCustomerString = null;
		JSONObject myCustomer = null;

		if (myCustomerResponse.getStatus() == 200)
		{
			myCustomerString = myCustomerResponse.getEntity().toString();
			try
			{
				myCustomer = JSONObject.parse(myCustomerString);
			}
			catch (IOException e)
			{
				logger.log(Level.SEVERE, e::toString);

				return false;
			}

			this.setDob(
					sortOutDate((String) myCustomer.get(JSON_DATE_OF_BIRTH)));
			this.setAddress((String) myCustomer.get(JSON_CUSTOMER_ADDRESS));
			this.setName((String) myCustomer.get(JSON_CUSTOMER_NAME));
			this.setSortcode((String) myCustomer.get(JSON_SORT_CODE));
			this.setCreditScore(
					(String) myCustomer.get(JSON_CUSTOMER_CREDIT_SCORE));
			this.setCreditScoreReviewDate(sortOutDate(
					(String) myCustomer.get(JSON_CUSTOMER_REVIEW_DATE)));
			String customerNoString = (String) myCustomer.get(JSON_ID);

			this.setCustomerNumber(customerNoString);
		}
		else
		{
			return false;
		}
		return true;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="265">

---

&nbsp;

```java
	public String addToDB()
	{
		CustomerResource myCustomerResource = new CustomerResource();

		CustomerJSON myCustomerJSON = new CustomerJSON();

		myCustomerJSON.setCustomerAddress(this.getAddress());
		myCustomerJSON.setCustomerName(this.getName());
		myCustomerJSON.setDateOfBirth(this.getDob());
		myCustomerJSON.setSortCode(this.getSortcode());
		Response myCustomerResponse = myCustomerResource
				.createCustomerExternal(myCustomerJSON);

		String myCustomerString = null;
		JSONObject myCustomer = null;

		if (myCustomerResponse.getStatus() == 201)
		{
			myCustomerString = myCustomerResponse.getEntity().toString();
			try
			{
				myCustomer = JSONObject.parse(myCustomerString);

				this.setDob(sortOutDate(
						(String) myCustomer.get(JSON_DATE_OF_BIRTH)));
				this.setAddress((String) myCustomer.get(JSON_CUSTOMER_ADDRESS));
				this.setName((String) myCustomer.get(JSON_CUSTOMER_NAME));
				this.setSortcode((String) myCustomer.get(JSON_SORT_CODE));

				String customerNoString = (String) myCustomer.get(JSON_ID);
				this.setCustomerNumber(customerNoString);
				myCustomerResponse.close();
				return customerNoString;
			}
			catch (IOException e)
			{
				logger.log(Level.SEVERE, e::toString);
				myCustomerResponse.close();
				return "-1";
			}
		}
		else
		{
			logger.log(Level.SEVERE, () -> myCustomerResponse.getStatus() + " "
					+ myCustomerResponse.getEntity().toString());
			myCustomerResponse.close();
			return "-1";
		}

	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="317">

---

&nbsp;

```java
	/**
	 * inDB
	 * 
	 * Checks if the customer is in the database
	 * 
	 * @return
	 */
	public boolean inDB()
	{
		CustomerResource myCustomerResource = new CustomerResource();

		Response myCustomerResponse = null;
		String myCustomerString = null;
		JSONObject myCustomer = null;

		myCustomerResponse = myCustomerResource
				.getCustomerExternal(Long.parseLong(this.customerNumber));
		if (myCustomerResponse.getStatus() == 200)
		{
			myCustomerString = myCustomerResponse.getEntity().toString();
			try
			{
				myCustomer = JSONObject.parse(myCustomerString);
			}
			catch (IOException e)
			{
				logger.log(Level.SEVERE, e::toString);
				myCustomerResponse.close();
				return false;
			}

			this.setDob(
					sortOutDate((String) myCustomer.get(JSON_DATE_OF_BIRTH)));

			String customerNoString = (String) myCustomer.get(JSON_ID);
			this.setCustomerNumber(customerNoString);
			this.setAddress((String) myCustomer.get(JSON_CUSTOMER_ADDRESS));
			this.setName((String) myCustomer.get(JSON_CUSTOMER_NAME));
			this.setSortcode((String) myCustomer.get(JSON_SORT_CODE));
			myCustomerResponse.close();
			return true;
		}
		myCustomerResponse.close();
		return false;
	}
```

---

</SwmSnippet>

# Utility methods

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="364">

---

These methods handle logging and date formatting. They ensure that the class operates correctly and that data is formatted consistently.

```java
	/**
	 * sortOutDate
	 * 
	 * Returns a correctly formatted java date from the one input
	 * 
	 * @param dateString
	 * @return
	 */
	private Date sortOutDate(String dateString)
	{
		String[] dateArray = dateString.split("-");

		Integer year = Integer.valueOf(dateArray[0]);
		Integer month = Integer.valueOf(dateArray[1]);
		Integer day = Integer.valueOf(dateArray[2]);

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.set(Calendar.YEAR, year);
		myCalendar.set(Calendar.MONTH, month - 1);
		myCalendar.set(Calendar.DATE, day);

		return new Date(myCalendar.getTimeInMillis());
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="389">

---

&nbsp;

```java
	/**
	 * showInfo
	 * 
	 * Displays all the info stored about the customer Will show a credit score
	 * if the customer is being edited
	 * 
	 */
	public void showInfo()
	{

		if (Boolean.FALSE.equals(editingCustomer))
		{
			logger.log(Level.INFO, () -> DASHES + this.customerNumber + ":"
					+ this.sortcode + DASHES);
			logger.log(Level.INFO, () -> "Sortcode - " + this.sortcode);
			logger.log(Level.INFO, () -> "Customer name - " + this.getName());
			logger.log(Level.INFO,
					() -> "Customer address - " + this.getAddress());
			logger.log(Level.INFO, () -> "Customer Date of Birth - "
					+ this.getDob().toString());
			logger.log(Level.INFO, () -> "Customer is new");
		}
		else
		{
			logger.log(Level.INFO, () -> DASHES + this.customerNumber + ":"
					+ this.sortcode + DASHES);
			logger.log(Level.INFO, () -> "Sortcode - " + this.sortcode);
			logger.log(Level.INFO, () -> "Customer name - " + this.getName());
			logger.log(Level.INFO,
					() -> "Customer address - " + this.getAddress());
			logger.log(Level.INFO, () -> "Customer Date of Birth - "
					+ this.getDob().toString());
			logger.log(Level.INFO,
					() -> "Customer credit score - " + this.getCreditScore());
			logger.log(Level.INFO, () -> "Customer cs review date - "
					+ this.getCreditScoreReviewDate().toString());
			logger.log(Level.INFO, () -> "Customer is being edited");
		}
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="430">

---

&nbsp;

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

This document provides an overview of the main components and design decisions in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="61:3:3" line-data="	public Customer(String custNo, String sortCode, String name, String address,">`Customer`</SwmToken> class. The class is designed to manage customer data effectively, with a focus on encapsulation, database operations, and utility functions.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
