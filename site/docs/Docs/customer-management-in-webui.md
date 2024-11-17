---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: z4p7t4sl
---
# Customer Management in Webui

## Understanding Customer in Webui

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="404:16:16" line-data="			logger.log(Level.INFO, () -&gt; &quot;Customer name - &quot; + this.getName());">`Customer`</SwmToken> class represents a bank customer, including their personal and financial information. This class provides various methods to manage customer data, such as creating, editing, and deleting customer instances.

### Customer Attributes

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="404:16:16" line-data="			logger.log(Level.INFO, () -&gt; &quot;Customer name - &quot; + this.getName());">`Customer`</SwmToken> class includes several attributes to store customer information. These attributes are:

- <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="43:5:5" line-data="	private String customerNumber;">`customerNumber`</SwmToken>
- <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="45:5:5" line-data="	private String sortcode;">`sortcode`</SwmToken>
- <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="47:5:5" line-data="	private String name;">`name`</SwmToken>
- <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="49:5:5" line-data="	private String address;">`address`</SwmToken>
- <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="51:5:5" line-data="	private Date dob;">`dob`</SwmToken> (date of birth)
- <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="53:5:5" line-data="	private String creditScore;">`creditScore`</SwmToken>
- <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="55:5:5" line-data="	private Date creditScoreReviewDate;">`creditScoreReviewDate`</SwmToken>
- <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="57:5:5" line-data="	private Boolean editingCustomer;">`editingCustomer`</SwmToken>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="43">

---

These attributes are defined in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="404:16:16" line-data="			logger.log(Level.INFO, () -&gt; &quot;Customer name - &quot; + this.getName());">`Customer`</SwmToken> class to store customer information.

```java
	private String customerNumber;

	private String sortcode;

	private String name;

	private String address;

	private Date dob;

	private String creditScore;

	private Date creditScoreReviewDate;

	private Boolean editingCustomer;
```

---

</SwmSnippet>

### Creating a Customer

To create a new customer, click on the 'Create customer' button on the landing page. This action will invoke the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="265:5:5" line-data="	public String addToDB()">`addToDB`</SwmToken> method to add the customer to the database.

### Viewing Customer Details

To view customer details, click the 'View customer details' button on the landing page. This will display all stored information about the customer.

### Adding a Customer to the Database

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="265:5:5" line-data="	public String addToDB()">`addToDB`</SwmToken> method is used to add a new customer to the database. It creates a <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="269:1:1" line-data="		CustomerJSON myCustomerJSON = new CustomerJSON();">`CustomerJSON`</SwmToken> object with the customer's details and sends a request to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="222:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> to create the customer externally. If the response status is 201, it parses the response and updates the customer's details.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="265">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="265:5:5" line-data="	public String addToDB()">`addToDB`</SwmToken> method adds a customer to the database by creating a <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="269:1:1" line-data="		CustomerJSON myCustomerJSON = new CustomerJSON();">`CustomerJSON`</SwmToken> object and sending a request to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="267:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken>.

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
```

---

</SwmSnippet>

### Updating Customer Information

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="174:5:5" line-data="	public boolean updateThis()">`updateThis`</SwmToken> method updates an existing customer's information in the database. It creates a <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="269:1:1" line-data="		CustomerJSON myCustomerJSON = new CustomerJSON();">`CustomerJSON`</SwmToken> object with the updated details and sends a request to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="222:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> to update the customer externally. If the response status is 200, it parses the response and updates the customer's details.

### Deleting a Customer from the Database

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="220:5:5" line-data="	public boolean deleteFromDB()">`deleteFromDB`</SwmToken> method deletes a customer from the database. It sends a request to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="222:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> to delete the customer externally. If the response status is 200, it confirms the deletion.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="220">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="220:5:5" line-data="	public boolean deleteFromDB()">`deleteFromDB`</SwmToken> method deletes a customer from the database by sending a request to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="222:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken>.

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
```

---

</SwmSnippet>

### Checking if a Customer Exists in the Database

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="318:3:3" line-data="	 * inDB">`inDB`</SwmToken> method checks if a customer exists in the database. It sends a request to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="222:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> to retrieve the customer details. If the response status is 200, it confirms the existence of the customer.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="317">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="318:3:3" line-data="	 * inDB">`inDB`</SwmToken> method checks if a customer exists in the database by sending a request to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="326:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken>.

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
```

---

</SwmSnippet>

### Displaying Customer Information

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="390:3:3" line-data="	 * showInfo">`showInfo`</SwmToken> method displays all stored information about a customer, including their credit score if the customer is being edited.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="389">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="390:3:3" line-data="	 * showInfo">`showInfo`</SwmToken> method displays all stored information about a customer.

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
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
