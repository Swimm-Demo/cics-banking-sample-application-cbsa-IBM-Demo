---
title: The CustomerResource class
---
This document will cover the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="102:3:3" line-data="	public CustomerResource()">`CustomerResource`</SwmToken> class. We will explain:

1. What <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="102:3:3" line-data="	public CustomerResource()">`CustomerResource`</SwmToken> is and its purpose.
2. The variables and functions defined in <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="102:3:3" line-data="	public CustomerResource()">`CustomerResource`</SwmToken>.

# Variables and functions

# What is <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="102:3:3" line-data="	public CustomerResource()">`CustomerResource`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="102:3:3" line-data="	public CustomerResource()">`CustomerResource`</SwmToken> class in <SwmPath>[src/…/json/CustomerResource.java](src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java)</SwmPath> is a RESTful API resource class that handles customer-related operations. It provides methods for creating, updating, retrieving, and deleting customer data. This class is used to manage customer information and interactions within the banking application.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="102">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="102:3:3" line-data="	public CustomerResource()">`CustomerResource`</SwmToken> is the constructor of the class. It initializes the logging configuration by calling the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="104:1:1" line-data="		sortOutLogging();">`sortOutLogging`</SwmToken> method.

```java
	public CustomerResource()
	{
		sortOutLogging();
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="108">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="110:5:5" line-data="	public Response createCustomerExternal(CustomerJSON customer)">`createCustomerExternal`</SwmToken> is a POST method that creates a new customer. It logs the entry, calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="114:7:7" line-data="		Response myResponse = createCustomerInternal(customer);">`createCustomerInternal`</SwmToken> method, and terminates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="115:1:1" line-data="		HBankDataAccess myHBankDataAccess = new HBankDataAccess();">`HBankDataAccess`</SwmToken> instance.

```java
	@POST
	@Produces(MediaType.APPLICATION_JSON)
	public Response createCustomerExternal(CustomerJSON customer)
	{
		logger.entering(this.getClass().getName(),
				CREATE_CUSTOMER_EXTERNAL + customer.toString());
		Response myResponse = createCustomerInternal(customer);
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(), CREATE_CUSTOMER_EXTERNAL_EXIT,
				myResponse);
		return myResponse;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="123">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="123:5:5" line-data="	public Response createCustomerInternal(CustomerJSON customer)">`createCustomerInternal`</SwmToken> handles the internal logic for creating a customer. It validates the customer data, creates a customer in the VSAM data store, and writes the transaction to the PROCTRAN data store.

```java
	public Response createCustomerInternal(CustomerJSON customer)
	{
		logger.entering(this.getClass().getName(),
				CREATE_CUSTOMER_INTERNAL + customer.toString());
		JSONObject response = new JSONObject();

		
		if(customer.getCustomerName() == null)
		{
			JSONObject error = new JSONObject();
			error.put(JSON_ERROR_MSG,
					"Customer name is null");
			Response myResponse = Response.status(400).entity(error.toString())
					.build();
			logger.log(Level.WARNING,
					() -> "Customer name is null in CustomerResource.createCustomerInternal(), "
							+ customer.toString());
			logger.exiting(this.getClass().getName(),
					CREATE_CUSTOMER_INTERNAL_EXIT, myResponse);
			return myResponse;
		}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="309">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="313:5:5" line-data="	public Response updateCustomerExternal(@PathParam(JSON_ID) Long id,">`updateCustomerExternal`</SwmToken> is a PUT method that updates an existing customer. It logs the entry, calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="318:7:7" line-data="		Response myResponse = updateCustomerInternal(id, customer);">`updateCustomerInternal`</SwmToken> method, and terminates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="319:1:1" line-data="		HBankDataAccess myHBankDataAccess = new HBankDataAccess();">`HBankDataAccess`</SwmToken> instance.

```java
	@PUT
	@Path("/{id}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response updateCustomerExternal(@PathParam(JSON_ID) Long id,
			CustomerJSON customer)
	{
		logger.entering(this.getClass().getName(),
				UPDATE_CUSTOMER_EXTERNAL + id);
		Response myResponse = updateCustomerInternal(id, customer);
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(), UPDATE_CUSTOMER_EXTERNAL + id,
				myResponse);
		return myResponse;

```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="328">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="328:5:5" line-data="	public Response updateCustomerInternal(@PathParam(JSON_ID) Long id,">`updateCustomerInternal`</SwmToken> handles the internal logic for updating a customer. It validates the customer data, updates the customer in the VSAM data store, and returns the response.

```java
	public Response updateCustomerInternal(@PathParam(JSON_ID) Long id,
			CustomerJSON customer)
	{
		logger.entering(this.getClass().getName(),
				UPDATE_CUSTOMER_INTERNAL + id);
		
		if(customer.getCustomerName() == null)
		{
			JSONObject error = new JSONObject();
			error.put(JSON_ERROR_MSG,
					"Customer name is null");
			Response myResponse = Response.status(400).entity(error.toString())
					.build();
			logger.log(Level.WARNING,
					() -> "Customer name is null in CustomerResource.updateCustomerInternal(), "
							+ customer.toString());
			logger.exiting(this.getClass().getName(),
					UPDATE_CUSTOMER_INTERNAL_EXIT, myResponse);
			return myResponse;
		}
		
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="460">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="463:5:5" line-data="	public Response getCustomerExternal(@PathParam(JSON_ID) Long id)">`getCustomerExternal`</SwmToken> is a GET method that retrieves a customer by ID. It logs the entry, calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="469:7:7" line-data="			Response myResponse = getCustomerInternal(id);">`getCustomerInternal`</SwmToken> method, and terminates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="470:1:1" line-data="			HBankDataAccess myHBankDataAccess = new HBankDataAccess();">`HBankDataAccess`</SwmToken> instance.

```java
	@GET
	@Path("/{id}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getCustomerExternal(@PathParam(JSON_ID) Long id)
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMER_EXTERNAL + id);

		try
		{
			Response myResponse = getCustomerInternal(id);
			HBankDataAccess myHBankDataAccess = new HBankDataAccess();
			myHBankDataAccess.terminate();
			logger.exiting(this.getClass().getName(), "getCustomerExternal",
					myResponse);
			return myResponse;

		}
		catch (Exception ex)
		{
			// Log the exception
			logger.log(Level.WARNING,
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="491">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="491:5:5" line-data="	public Response getCustomerInternal(@PathParam(JSON_ID) Long id)">`getCustomerInternal`</SwmToken> handles the internal logic for retrieving a customer by ID. It validates the customer ID, retrieves the customer from the VSAM data store, and returns the response.

```java
	public Response getCustomerInternal(@PathParam(JSON_ID) Long id)
	{
		logger.entering(this.getClass().getName(),
				"getCustomerInternal for customerNumber " + id);
		Integer sortCode = this.getSortCode();

		JSONObject response = new JSONObject();

		if (id.longValue() < 0)
		{
			// Customer number cannot be negative
			response.put(JSON_ERROR_MSG, "Customer number cannot be negative");
			Response myResponse = Response.status(404)
					.entity(response.toString()).build();
			logger.log(Level.WARNING,
					() -> "Customer number supplied was negative in CustomerResource.getCustomerInternal");
			logger.exiting(this.getClass().getName(),
					GET_CUSTOMER_INTERNAL_EXIT, myResponse);
			return myResponse;
		}

```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="547">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="550:5:5" line-data="	public Response deleteCustomerExternal(@PathParam(JSON_ID) Long id)">`deleteCustomerExternal`</SwmToken> is a DELETE method that deletes a customer by ID. It logs the entry, calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="554:7:7" line-data="		Response myResponse = deleteCustomerInternal(id);">`deleteCustomerInternal`</SwmToken> method, and terminates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="555:1:1" line-data="		HBankDataAccess myHBankDataAccess = new HBankDataAccess();">`HBankDataAccess`</SwmToken> instance.

```java
	@DELETE
	@Path("/{id}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response deleteCustomerExternal(@PathParam(JSON_ID) Long id)
	{
		logger.entering(this.getClass().getName(),
				"deleteCustomerExtnernal(Long id) for customerNumber " + id);
		Response myResponse = deleteCustomerInternal(id);
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),
				"deleteCustomerExternal(Long id)", myResponse);
		return myResponse;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="563">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="563:5:5" line-data="	public Response deleteCustomerInternal(Long id)">`deleteCustomerInternal`</SwmToken> handles the internal logic for deleting a customer by ID. It validates the customer ID, deletes the customer and their accounts from the VSAM data store, and writes the transaction to the PROCTRAN data store.

```java
	public Response deleteCustomerInternal(Long id)
	{
		logger.entering(this.getClass().getName(),
				"deleteCustomerInternal(Long id) for customerNumber " + id);

		Integer sortCode = this.getSortCode();

		JSONObject response = new JSONObject();

		if (id.longValue() < 0)
		{
			// Customer number cannot be negative
			response.put(JSON_ERROR_MSG, "Customer number cannot be negative");
			Response myResponse = Response.status(404)
					.entity(response.toString()).build();
			logger.log(Level.WARNING,
					() -> "Customer number supplied was negative in deleteCustomerInternal()");
			logger.exiting(this.getClass().getName(),
					DELETE_CUSTOMER_INTERNAL_EXIT, myResponse);
			return myResponse;
		}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="733">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="736:5:5" line-data="	public Response getCustomersTownExternal(@PathParam(&quot;town&quot;) String town)">`getCustomersTownExternal`</SwmToken> is a GET method that retrieves customers by town. It logs the entry, calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="741:7:7" line-data="		Response myResponse = getCustomersTownInternal(town);">`getCustomersTownInternal`</SwmToken> method, and terminates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="742:1:1" line-data="		HBankDataAccess myHBankDataAccess = new HBankDataAccess();">`HBankDataAccess`</SwmToken> instance.

```java
	@GET
	@Path("/all/town/{town}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getCustomersTownExternal(@PathParam("town") String town)
	{

		logger.entering(this.getClass().getName(),
				"getCustomersTownExternal(String town) for town " + town);
		Response myResponse = getCustomersTownInternal(town);
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),
				"getCustomersTownExternal(String town)", myResponse);
		return myResponse;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="750">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="750:5:5" line-data="	public Response getCustomersTownInternal(String town)">`getCustomersTownInternal`</SwmToken> handles the internal logic for retrieving customers by town. It retrieves the customers from the VSAM data store and returns the response.

```java
	public Response getCustomersTownInternal(String town)
	{

		logger.entering(this.getClass().getName(),
				"getCustomersTownInternal(String town) for town " + town);

		JSONArray allCustomers = new JSONArray();

		JSONObject response = new JSONObject();

		com.ibm.cics.cip.bankliberty.web.vsam.Customer myCustomer = new Customer();
		myCustomer.setSortcode(this.getSortCode().toString());
		com.ibm.cics.cip.bankliberty.web.vsam.Customer[] vsamCustomers = myCustomer
				.getCustomersByTown(town);

		for (int i = 0; i < vsamCustomers.length; i++)
		{
			response.put(JSON_ID, vsamCustomers[i].getCustomerNumber().trim());
			response.put(JSON_CUSTOMER_NAME, vsamCustomers[i].getName().trim());
			response.put(JSON_CUSTOMER_ADDRESS,
					vsamCustomers[i].getAddress().trim());
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="796">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="799:5:5" line-data="	public Response getCustomersSurnameExternal(">`getCustomersSurnameExternal`</SwmToken> is a GET method that retrieves customers by surname. It logs the entry, calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="805:7:7" line-data="		Response myResponse = getCustomersSurnameInternal(surname);">`getCustomersSurnameInternal`</SwmToken> method, and terminates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="806:1:1" line-data="		HBankDataAccess myHBankDataAccess = new HBankDataAccess();">`HBankDataAccess`</SwmToken> instance.

```java
	@GET
	@Path("/all/surname/{surname}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getCustomersSurnameExternal(
			@PathParam("surname") String surname)
	{
		logger.entering(this.getClass().getName(),
				"getCustomersSurnameExternal(String surname) for surname "
						+ surname);
		Response myResponse = getCustomersSurnameInternal(surname);
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),
				"getCustomersSurnameExternal(String surname)", myResponse);
		return myResponse;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="814">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="814:5:5" line-data="	public Response getCustomersSurnameInternal(String surname)">`getCustomersSurnameInternal`</SwmToken> handles the internal logic for retrieving customers by surname. It retrieves the customers from the VSAM data store and returns the response.

```java
	public Response getCustomersSurnameInternal(String surname)
	{

		logger.entering(this.getClass().getName(),
				"getCustomersSurnameInternal(String surname) for surname "
						+ surname);

		JSONArray allCustomers = new JSONArray();

		JSONObject response = new JSONObject();

		com.ibm.cics.cip.bankliberty.web.vsam.Customer myCustomer = new Customer();
		myCustomer.setSortcode(this.getSortCode().toString());
		com.ibm.cics.cip.bankliberty.web.vsam.Customer[] vsamCustomers = myCustomer
				.getCustomersBySurname(surname);

		for (int i = 0; i < vsamCustomers.length; i++)
		{
			response.put(JSON_ID, vsamCustomers[i].getCustomerNumber().trim());
			response.put(JSON_CUSTOMER_NAME, vsamCustomers[i].getName().trim());
			response.put(JSON_CUSTOMER_ADDRESS,
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="861">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="864:5:5" line-data="	public Response getCustomersAgeExternal(@PathParam(&quot;age&quot;) String age)">`getCustomersAgeExternal`</SwmToken> is a GET method that retrieves customers by age. It logs the entry, calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="868:7:7" line-data="		Response myResponse = getCustomersAgeInternal(age);">`getCustomersAgeInternal`</SwmToken> method, and terminates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="869:1:1" line-data="		HBankDataAccess myHBankDataAccess = new HBankDataAccess();">`HBankDataAccess`</SwmToken> instance.

```java
	@GET
	@Path("/all/age/{age}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getCustomersAgeExternal(@PathParam("age") String age)
	{
		logger.entering(this.getClass().getName(),
				"getCustomersAgeExternal(String age) for age " + age);
		Response myResponse = getCustomersAgeInternal(age);
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.entering(this.getClass().getName(),
				"getCustomersAgeExternal(String age)", myResponse);
		return myResponse;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="877">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="877:5:5" line-data="	public Response getCustomersAgeInternal(String age)">`getCustomersAgeInternal`</SwmToken> handles the internal logic for retrieving customers by age. It retrieves the customers from the VSAM data store and returns the response.

```java
	public Response getCustomersAgeInternal(String age)
	{

		logger.entering(this.getClass().getName(),
				"getCustomersAgeInternalInternal(String age) for age " + age);

		JSONArray allCustomers = new JSONArray();
		JSONObject response = new JSONObject();

		com.ibm.cics.cip.bankliberty.web.vsam.Customer myCustomer = new Customer();
		myCustomer.setSortcode(this.getSortCode().toString());
		com.ibm.cics.cip.bankliberty.web.vsam.Customer[] vsamCustomers = myCustomer
				.getCustomersByAge(Integer.parseInt(age));

		for (int i = 0; i < vsamCustomers.length; i++)
		{
			JSONObject customer = new JSONObject();
			customer.put(JSON_ID, vsamCustomers[i].getCustomerNumber().trim());
			customer.put(JSON_CUSTOMER_NAME, vsamCustomers[i].getName().trim());
			customer.put(JSON_CUSTOMER_ADDRESS,
					vsamCustomers[i].getAddress().trim());
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="925">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="925:5:5" line-data="	private Integer getSortCode()">`getSortCode`</SwmToken> retrieves the sort code for the bank. If the sort code is not already set, it fetches it from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="930:1:1" line-data="			SortCodeResource mySortCodeResource = new SortCodeResource();">`SortCodeResource`</SwmToken>.

```java
	private Integer getSortCode()
	{
		logger.entering(this.getClass().getName(), "getSortCode()");
		if (sortcode == null)
		{
			SortCodeResource mySortCodeResource = new SortCodeResource();
			Response mySortCodeJSON = mySortCodeResource.getSortCode();
			CustomerResource.setSortcode(
					((String) mySortCodeJSON.getEntity()).substring(13, 19));
		}
		logger.exiting(this.getClass().getName(), "getSortCode()", sortcode);
		return Integer.parseInt(sortcode);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="940">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="942:5:5" line-data="	public Response getCustomersExternal(@QueryParam(&quot;limit&quot;) Integer limit,">`getCustomersExternal`</SwmToken> is a GET method that retrieves customers with optional parameters for limit, offset, and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="944:5:5" line-data="			@QueryParam(&quot;countOnly&quot;) Boolean countOnly)">`countOnly`</SwmToken>. It logs the entry, calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="954:7:7" line-data="		Response myResponse = getCustomersInternal(limit, offset,">`getCustomersInternal`</SwmToken> method, and terminates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="956:1:1" line-data="		HBankDataAccess myHBankDataAccess = new HBankDataAccess();">`HBankDataAccess`</SwmToken> instance.

```java
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Response getCustomersExternal(@QueryParam("limit") Integer limit,
			@QueryParam("offset") Integer offset,
			@QueryParam("countOnly") Boolean countOnly)
	{
		logger.entering(this.getClass().getName(),
				"getCustomersExternal(Integer limit, Integer offset, Boolean countOnly) "
						+ limit + " " + offset + " " + countOnly);
		boolean countOnlyReal = false;
		if (countOnly != null)
		{
			countOnlyReal = countOnly.booleanValue();
		}
		Response myResponse = getCustomersInternal(limit, offset,
				countOnlyReal);
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(),
				"getCustomersExternal(Integer limit, Integer offset, Boolean countOnly)",
				myResponse);
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="965">

---

The function <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="965:5:5" line-data="	public Response getCustomersInternal(@QueryParam(&quot;limit&quot;) Integer limit,">`getCustomersInternal`</SwmToken> handles the internal logic for retrieving customers with optional parameters for limit, offset, and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="966:16:16" line-data="			@QueryParam(&quot;offset&quot;) Integer offset, boolean countOnly)">`countOnly`</SwmToken>. It retrieves the customers from the VSAM data store and returns the response.

```java
	public Response getCustomersInternal(@QueryParam("limit") Integer limit,
			@QueryParam("offset") Integer offset, boolean countOnly)
	{
		logger.entering(this.getClass().getName(),
				"getCustomersInternal(Integer limit, Integer offset, Boolean countOnly) "
						+ limit + " " + offset + " " + countOnly);
		Integer sortCode = this.getSortCode();

		JSONObject response = new JSONObject();
		JSONArray customers = null;

		if (offset == null)
		{
			offset = 0;
		}

		if (limit == null)
		{
			limit = 250000;
		}

```

---

</SwmSnippet>

# Usage

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="174">

---

## Customer

In the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="134:2:2" line-data="					&quot;Customer name is null&quot;);">`Customer`</SwmToken> class, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="176:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> is instantiated multiple times to perform various operations such as updating, deleting, adding, and checking the existence of a customer in the database.

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
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" line="67">

---

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="25:4:4" line-data="public class CustomerList">`CustomerList`</SwmToken>

In the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="25:4:4" line-data="public class CustomerList">`CustomerList`</SwmToken> class, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/CustomerList.java" pos="70:1:1" line-data="		CustomerResource myCustomerResource = new CustomerResource();">`CustomerResource`</SwmToken> is used to determine the number of customers that match a given filter and to retrieve customer data with specific limits and offsets.

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

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" line="502">

---

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="587:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken>

In the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="587:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken> class, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" pos="505:1:1" line-data="		CustomerResource myCustomer = new CustomerResource();">`CustomerResource`</SwmToken> is utilized to fetch internal customer details based on the customer number.

```java
		Integer sortCode = this.getSortCode();
		int numberOfAccounts = 0;

		CustomerResource myCustomer = new CustomerResource();
		Response customerResponse = myCustomer
				.getCustomerInternal(customerNumber);

		if (customerResponse.getStatus() != 200)
		{
			if (customerResponse.getStatus() == 404)
			{
				// If cannot find response "CustomerResponse" then error 404
				// returned
				JSONObject error = new JSONObject();
				error.put(JSON_ERROR_MSG, CUSTOMER_NUMBER_LITERAL
						+ customerNumber.longValue() + CANNOT_BE_FOUND);
				logger.log(Level.SEVERE, () -> CUSTOMER_NUMBER_LITERAL
						+ customerNumber.longValue() + CANNOT_BE_FOUND);
				myResponse = Response.status(404).entity(error.toString())
						.build();
				logger.exiting(this.getClass().getName(),
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
