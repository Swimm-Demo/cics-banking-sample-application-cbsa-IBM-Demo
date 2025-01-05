---
title: CounterResource API Endpoints
---
# Introduction

This document will walk you through the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="40:17:17" line-data="	 * This class describes the methods of the CounterResource. NamedCounters">`CounterResource`</SwmToken> API endpoints.

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="40:17:17" line-data="	 * This class describes the methods of the CounterResource. NamedCounters">`CounterResource`</SwmToken> class provides endpoints to manage account and customer counters. It interacts with COBOL programs via <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="41:11:11" line-data="	 * are not supported in jCICS, so we jCICS LINK to COBOL programs">`jCICS`</SwmToken> LINK to perform the necessary operations.

We will cover:

1. The purpose of the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="40:17:17" line-data="	 * This class describes the methods of the CounterResource. NamedCounters">`CounterResource`</SwmToken> class.
2. The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="87:5:5" line-data="	public Response getAccountCounter()">`getAccountCounter`</SwmToken> endpoint.
3. The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="133:5:5" line-data="	public Response getCustomerCounter()">`getCustomerCounter`</SwmToken> endpoint.
4. The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="180:5:5" line-data="	public Response incrementCustomerCounter()">`incrementCustomerCounter`</SwmToken> endpoint.
5. The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="228:5:5" line-data="	public Response decrementCustomerCounter()">`decrementCustomerCounter`</SwmToken> endpoint.
6. The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="280:5:5" line-data="	public Response incrementAccountCounter()">`incrementAccountCounter`</SwmToken> endpoint.
7. The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="327:5:5" line-data="	public Response decrementAccountCounter()">`decrementAccountCounter`</SwmToken> endpoint.

# Purpose of the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="40:17:17" line-data="	 * This class describes the methods of the CounterResource. NamedCounters">`CounterResource`</SwmToken> class

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="39">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="40:17:17" line-data="	 * This class describes the methods of the CounterResource. NamedCounters">`CounterResource`</SwmToken> class is designed to handle operations related to account and customer counters. It uses <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="41:11:11" line-data="	 * are not supported in jCICS, so we jCICS LINK to COBOL programs">`jCICS`</SwmToken> LINK to interact with COBOL programs since <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="40:20:20" line-data="	 * This class describes the methods of the CounterResource. NamedCounters">`NamedCounters`</SwmToken> are not supported in <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="41:11:11" line-data="	 * are not supported in jCICS, so we jCICS LINK to COBOL programs">`jCICS`</SwmToken>.

```java
	/**
	 * This class describes the methods of the CounterResource. NamedCounters
	 * are not supported in jCICS, so we jCICS LINK to COBOL programs
	 * 
	 */
```

---

</SwmSnippet>

# <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="87:5:5" line-data="	public Response getAccountCounter()">`getAccountCounter`</SwmToken> endpoint

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="84">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="87:5:5" line-data="	public Response getAccountCounter()">`getAccountCounter`</SwmToken> endpoint retrieves the current account counter value. It logs the entry and exit points, links to the COBOL program, and returns the account number in a JSON response.

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
			response.put(JSON_ACCOUNT_NUMBER, myNEWACCNO.getAccountNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), GET_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), GET_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}
	}
```

---

</SwmSnippet>

# <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="133:5:5" line-data="	public Response getCustomerCounter()">`getCustomerCounter`</SwmToken> endpoint

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="130">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="133:5:5" line-data="	public Response getCustomerCounter()">`getCustomerCounter`</SwmToken> endpoint retrieves the current customer counter value. Similar to <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="87:5:5" line-data="	public Response getAccountCounter()">`getAccountCounter`</SwmToken>, it logs the entry and exit points, links to the COBOL program, and returns the customer number in a JSON response.

```java
	@GET
	@Path("/customer")
	@Produces("application/json")
	public Response getCustomerCounter()
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMER_COUNTER);

		JSONObject response = new JSONObject();
		Response myResponse = null;

		Program newcusnoProgram = new Program();
		newcusnoProgram.setName(NEWCUSNO);

		NewCustomerNumber myNEWCUSNO = new NewCustomerNumber();

		myNEWCUSNO.setNewcusnoFunction("C");
		byte[] data = myNEWCUSNO.getByteBuffer();
		try
		{
			newcusnoProgram.link(data);
			myNEWCUSNO = new NewCustomerNumber(data);
			logger.fine(NEW_CUSNO_PREFIX + myNEWCUSNO.getCustomerNumber());
			response.put(JSON_CUSTOMER_NUMBER, myNEWCUSNO.getCustomerNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), GET_CUSTOMER_COUNTER,
					myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), GET_CUSTOMER_COUNTER,
					myResponse);
			return myResponse;
		}
	}
```

---

</SwmSnippet>

# <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="180:5:5" line-data="	public Response incrementCustomerCounter()">`incrementCustomerCounter`</SwmToken> endpoint

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="177">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="180:5:5" line-data="	public Response incrementCustomerCounter()">`incrementCustomerCounter`</SwmToken> endpoint increments the customer counter. It logs the entry and exit points, links to the COBOL program, and returns the updated customer number in a JSON response.

```java
	@POST
	@Path("/customer")
	@Produces("application/json")
	public Response incrementCustomerCounter()
	{
		logger.entering(this.getClass().getName(), INCREMENT_CUSTOMER_COUNTER);
		Response myResponse = null;

		JSONObject response = new JSONObject();

		Program newcusnoProgram = new Program();
		newcusnoProgram.setName(NEWCUSNO);

		NewCustomerNumber myNEWCUSNO = new NewCustomerNumber();

		myNEWCUSNO.setNewcusnoFunction("G");
		byte[] data = myNEWCUSNO.getByteBuffer();
		try
		{
			newcusnoProgram.link(data);
			myNEWCUSNO = new NewCustomerNumber(data);

			logger.fine(NEW_CUSNO_PREFIX + myNEWCUSNO.getCustomerNumber());
			response.put(JSON_CUSTOMER_NUMBER, myNEWCUSNO.getCustomerNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(),
					INCREMENT_CUSTOMER_COUNTER, myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(),
					INCREMENT_CUSTOMER_COUNTER, myResponse);
			return myResponse;
		}
	}
```

---

</SwmSnippet>

# <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="228:5:5" line-data="	public Response decrementCustomerCounter()">`decrementCustomerCounter`</SwmToken> endpoint

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="225">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="228:5:5" line-data="	public Response decrementCustomerCounter()">`decrementCustomerCounter`</SwmToken> endpoint decrements the customer counter. It logs the entry and exit points, links to the COBOL program, and returns the updated customer number in a JSON response.

```java
	@DELETE
	@Path("/customer")
	@Produces("application/json")
	public Response decrementCustomerCounter()
	{
		logger.entering(this.getClass().getName(), DECREMENT_CUSTOMER_COUNTER);
		Response myResponse = null;

		JSONObject response = new JSONObject();

		Program newcusnoProgram = new Program();
		newcusnoProgram.setName(NEWCUSNO);

		NewCustomerNumber myNEWCUSNO = new NewCustomerNumber();

		myNEWCUSNO.setNewcusnoFunction("C");
		byte[] data = myNEWCUSNO.getByteBuffer();
		try
		{
			newcusnoProgram.link(data);
			myNEWCUSNO = new NewCustomerNumber(data);
			myNEWCUSNO.setCustomerNumber(myNEWCUSNO.getCustomerNumber() - 1);
			myNEWCUSNO.setNewcusnoFunction("R");
			newcusnoProgram.link(data);

			logger.fine(NEW_CUSNO_PREFIX + myNEWCUSNO.getCustomerNumber());
			response.put(JSON_CUSTOMER_NUMBER, myNEWCUSNO.getCustomerNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(),
					DECREMENT_CUSTOMER_COUNTER, myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(),
					DECREMENT_CUSTOMER_COUNTER, myResponse);
			return myResponse;
		}

	}
```

---

</SwmSnippet>

# <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="280:5:5" line-data="	public Response incrementAccountCounter()">`incrementAccountCounter`</SwmToken> endpoint

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="277">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="280:5:5" line-data="	public Response incrementAccountCounter()">`incrementAccountCounter`</SwmToken> endpoint increments the account counter. It logs the entry and exit points, links to the COBOL program, and returns the updated account number in a JSON response.

```java
	@POST
	@Path("/account")
	@Produces("application/json")
	public Response incrementAccountCounter()
	{

		logger.entering(this.getClass().getName(), INCREMENT_ACCOUNT_COUNTER);
		Response myResponse = null;
		JSONObject response = new JSONObject();

		Program newaccnoProgram = new Program();
		newaccnoProgram.setName(NEWACCNO);

		NewAccountNumber myNEWACCNO = new NewAccountNumber();

		myNEWACCNO.setNewaccnoFunction("G");
		byte[] data = myNEWACCNO.getByteBuffer();
		try
		{
			newaccnoProgram.link(data);
			myNEWACCNO = new NewAccountNumber(data);
			logger.fine(NEW_ACCNO_PREFIX + myNEWACCNO.getAccountNumber());
			response.put(JSON_ACCOUNT_NUMBER, myNEWACCNO.getAccountNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), INCREMENT_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), INCREMENT_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}
	}
```

---

</SwmSnippet>

# <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="327:5:5" line-data="	public Response decrementAccountCounter()">`decrementAccountCounter`</SwmToken> endpoint

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="324">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="327:5:5" line-data="	public Response decrementAccountCounter()">`decrementAccountCounter`</SwmToken> endpoint decrements the account counter. It logs the entry and exit points, links to the COBOL program, and returns the updated account number in a JSON response.

```java
	@DELETE
	@Path("/account")
	@Produces("application/json")
	public Response decrementAccountCounter()
	{
		logger.entering(this.getClass().getName(), DECREMENT_ACCOUNT_COUNTER);
		Response myResponse = null;
		JSONObject response = new JSONObject();

		Program newaccnoProgram = new Program();
		newaccnoProgram.setName(NEWACCNO);

		NewAccountNumber myNEWACCNO = new NewAccountNumber();

		myNEWACCNO.setNewaccnoFunction("C");
		byte[] data = myNEWACCNO.getByteBuffer();
		try
		{
			newaccnoProgram.link(data);
			myNEWACCNO = new NewAccountNumber(data);
			myNEWACCNO.setAccountNumber(myNEWACCNO.getAccountNumber() - 1);
			myNEWACCNO.setNewaccnoFunction("R");
			newaccnoProgram.link(data);

			logger.fine(NEW_ACCNO_PREFIX + myNEWACCNO.getAccountNumber());
			response.put(JSON_ACCOUNT_NUMBER, myNEWACCNO.getAccountNumber());
			response.put(JSON_SUCCESS, "Y");
			myResponse = Response.status(200).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), DECREMENT_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}
		catch (InvalidRequestException | LengthErrorException
				| InvalidSystemIdException | NotAuthorisedException
				| InvalidProgramIdException | RolledBackException
				| TerminalException e)
		{
			logger.severe(e.getLocalizedMessage());
			response.put(JSON_ERROR_MSG, e.toString());
			response.put(JSON_SUCCESS, "N");
			myResponse = Response.status(500).entity(response.toString())
					.build();
			logger.exiting(this.getClass().getName(), DECREMENT_ACCOUNT_COUNTER,
					myResponse);
			return myResponse;
		}

	}

}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
