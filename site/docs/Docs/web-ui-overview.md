---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: q72j19wy
---
# Web UI Overview

## Web UI Overview

The Web UI is part of the CICS Bank Sample Application (CBSA) and is implemented using the WebSphere Liberty Profile application. It includes various web resources such as HTML files, images, and JSON manifests that define the structure and appearance of the web interface. The Web UI provides a user-friendly interface for interacting with the bank's functionalities, allowing users to perform tasks such as viewing account information and processing transactions. The source code for the Web UI is organized under the `src/webui/` directory, which contains all the necessary files and directories to build and deploy the web application.

## Web UI Endpoints

The Web UI includes several endpoints that facilitate interaction with the bank's functionalities. These endpoints are implemented as RESTful APIs and are defined in the `src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/` directory.

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="87:5:5" line-data="	public Response getAccountCounter()">`getAccountCounter`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="87:5:5" line-data="	public Response getAccountCounter()">`getAccountCounter`</SwmToken> endpoint is a GET request that retrieves the current account counter. It uses the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="93:1:1" line-data="		Program newaccnoProgam = new Program();">`Program`</SwmToken> class to link to a COBOL program named <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="94:5:5" line-data="		newaccnoProgam.setName(NEWACCNO);">`NEWACCNO`</SwmToken> to get the account number.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="84">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="87:5:5" line-data="	public Response getAccountCounter()">`getAccountCounter`</SwmToken> endpoint implementation shows how the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="93:1:1" line-data="		Program newaccnoProgam = new Program();">`Program`</SwmToken> class is used to link to the COBOL program <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="94:5:5" line-data="		newaccnoProgam.setName(NEWACCNO);">`NEWACCNO`</SwmToken> and retrieve the account number.

```java
	@GET
	@Path("/account")
	@Produces("application/json")
	public Response getAccountCounter()
	{
		logger.entering(this.getClass().getName(), GET_ACCOUNT_COUNTER);
		JSONObject response = new JSONObject();
		Response myResponse = null;

		Program newaccnoProgam = new Program();
		newaccnoProgam.setName(NEWACCNO);

		NewAccountNumber myNEWACCNO = new NewAccountNumber();

		myNEWACCNO.setNewaccnoFunction("C");
		byte[] data = myNEWACCNO.getByteBuffer();
		try
		{
			newaccnoProgam.link(data);
			myNEWACCNO = new NewAccountNumber(data);
			logger.fine(NEW_ACCNO_PREFIX + myNEWACCNO.getAccountNumber());
```

---

</SwmSnippet>

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="110:5:5" line-data="	public Response createCustomerExternal(CustomerJSON customer)">`createCustomerExternal`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="110:5:5" line-data="	public Response createCustomerExternal(CustomerJSON customer)">`createCustomerExternal`</SwmToken> endpoint is a POST request that creates a new customer. It calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="114:7:7" line-data="		Response myResponse = createCustomerInternal(customer);">`createCustomerInternal`</SwmToken> method to handle the creation logic and then terminates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="115:1:1" line-data="		HBankDataAccess myHBankDataAccess = new HBankDataAccess();">`HBankDataAccess`</SwmToken> session.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="108">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="110:5:5" line-data="	public Response createCustomerExternal(CustomerJSON customer)">`createCustomerExternal`</SwmToken> endpoint implementation demonstrates how the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="114:7:7" line-data="		Response myResponse = createCustomerInternal(customer);">`createCustomerInternal`</SwmToken> method is called to handle the creation logic and how the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="115:1:1" line-data="		HBankDataAccess myHBankDataAccess = new HBankDataAccess();">`HBankDataAccess`</SwmToken> session is terminated.

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
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
