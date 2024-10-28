---
title: Retrieving Customer Account Information
---
In this document, we will explain the process of retrieving account information for a customer. The process involves several steps including validating the customer, fetching account details, and formatting the response.

The flow starts by validating the customer to ensure they exist. If the customer is not found, an error is returned. If the customer is valid, the next step is to fetch all accounts associated with the customer. If no accounts are found, another error is returned. Finally, if accounts are found, they are formatted into a JSON response and sent back to the client.

# Flow drill down

```mermaid
graph TD;
      subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
9f1d4b63259741678a7c0c7ed2019d4a91de568c643875c72c5810e44e5ba0fc(getAccountsByCustomerInternal) --> f42ad07b3c1434bec6dd81ea8109f8aebe7c804c1bb06f7252e816311ef05f35(getCustomerInternal)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertywebdb2Accountjava["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/db2/Account.java"]
9f1d4b63259741678a7c0c7ed2019d4a91de568c643875c72c5810e44e5ba0fc(getAccountsByCustomerInternal) --> 2ad4232e406fa96a04bfcded8864ac207154294d2be003ba59f1bc5e0b047210(getAccounts)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
2ad4232e406fa96a04bfcded8864ac207154294d2be003ba59f1bc5e0b047210(getAccounts) --> 4ccb7e7f1ffffa4e90c42aeedb17171c5fac78dfdb55c155b9b79af1f1797072(openConnection)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
4ccb7e7f1ffffa4e90c42aeedb17171c5fac78dfdb55c155b9b79af1f1797072(openConnection) --> a45bfb1a8c2dd416e124b8a4fc5988afb239da44caac19057d872f11b7c78c0c(openConnectionInternal)
end


      classDef mainFlowStyle color:#000000,fill:#7CB9F4
classDef rootsStyle color:#000000,fill:#00FFF4
classDef Style1 color:#000000,fill:#00FFAA
classDef Style2 color:#000000,fill:#FFFF00
classDef Style3 color:#000000,fill:#AA7CB9
```

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" line="492">

---

## Retrieving Customer Information

First, the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" pos="492:5:5" line-data="	public Response getAccountsByCustomerInternal(">`getAccountsByCustomerInternal`</SwmToken> method retrieves customer information by calling the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" pos="507:2:2" line-data="				.getCustomerInternal(customerNumber);">`getCustomerInternal`</SwmToken> method. This step ensures that the customer exists and is valid before proceeding to fetch account details.

```java
	public Response getAccountsByCustomerInternal(
			@PathParam(JSON_CUSTOMER_NUMBER) Long customerNumber)
	{
		logger.entering(this.getClass().getName(),
				GET_ACCOUNTS_BY_CUSTOMER_INTERNAL);

		JSONArray accounts = null;
		Response myResponse = null;

		JSONObject response = new JSONObject();
		Integer sortCode = this.getSortCode();
		int numberOfAccounts = 0;

		CustomerResource myCustomer = new CustomerResource();
		Response customerResponse = myCustomer
				.getCustomerInternal(customerNumber);
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" line="509">

---

## Handling Customer Not Found

Next, the method checks if the customer was found. If the customer is not found, it returns a 404 error response indicating that the customer number cannot be found.

```java
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
						GET_ACCOUNTS_BY_CUSTOMER_INTERNAL, myResponse);
				return myResponse;
			}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" line="544">

---

## Retrieving Account Information

Then, the method retrieves account information for the customer by calling the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" pos="545:11:11" line-data="		Account[] myAccounts = db2Account.getAccounts(customerNumber.intValue(),">`getAccounts`</SwmToken> method. This step fetches all accounts associated with the customer number.

```java
		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();
		Account[] myAccounts = db2Account.getAccounts(customerNumber.intValue(),
				sortCode);
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" line="547">

---

## Handling No Accounts Found

If no accounts are found for the customer, the method returns a 500 error response indicating that accounts cannot be accessed for the customer.

```java
		if (myAccounts == null)
		{
			JSONObject error = new JSONObject();
			error.put(JSON_ERROR_MSG,
					"Accounts cannot be accessed for customer "
							+ customerNumber.longValue() + CLASS_NAME_MSG);
			logger.log(Level.SEVERE,
					() -> "Accounts cannot be accessed for customer "
							+ customerNumber.longValue() + CLASS_NAME_MSG);
			myResponse = Response.status(500).entity(error.toString()).build();
			logger.exiting(this.getClass().getName(),
					GET_ACCOUNTS_BY_CUSTOMER_INTERNAL, myResponse);
			return myResponse;
		}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" line="562">

---

## Formatting Account Information

Finally, the method formats the retrieved account information into a JSON response and returns it with a 200 status code. This step ensures that the account details are properly structured and ready to be sent to the client.

```java
		numberOfAccounts = myAccounts.length;
		accounts = new JSONArray(numberOfAccounts);
		for (int i = 0; i < numberOfAccounts; i++)
		{

			JSONObject account = new JSONObject();
			account.put(JSON_SORT_CODE, myAccounts[i].getSortcode());
			account.put("id", myAccounts[i].getAccountNumber());
			account.put(JSON_CUSTOMER_NUMBER,
					myAccounts[i].getCustomerNumber());
			account.put(JSON_ACCOUNT_TYPE, myAccounts[i].getType());
			account.put(JSON_AVAILABLE_BALANCE,
					BigDecimal.valueOf(myAccounts[i].getAvailableBalance()));
			account.put(JSON_ACTUAL_BALANCE,
					BigDecimal.valueOf(myAccounts[i].getActualBalance()));
			account.put(JSON_INTEREST_RATE,
					BigDecimal.valueOf(myAccounts[i].getInterestRate()));
			account.put(JSON_OVERDRAFT, myAccounts[i].getOverdraftLimit());
			account.put(JSON_LAST_STATEMENT_DATE,
					myAccounts[i].getLastStatement().toString());
			account.put(JSON_NEXT_STATEMENT_DATE,
```

---

</SwmSnippet>

# Where is this flow used?

This flow is used multiple times in the codebase as represented in the following diagram:

```mermaid
graph TD;
      subgraph srcwebuisrcmainjavacomibmcicscipbankliberty["src/webui/src/main/java/com/ibm/cics/cip/bankliberty"]
bf450df65db79a11b5d0ec7b961c0e4a9b9e8e5b176f340cc93fd8be8f6b7986(addToDB):::rootsStyle --> 1d896006c602d7cab6d31becfef1bb9a8ea272578ecce0fccd7fbe497607cc46(createAccountInternal)
end

subgraph srcwebuisrcmainjavacomibmcicscipbankliberty["src/webui/src/main/java/com/ibm/cics/cip/bankliberty"]
1d896006c602d7cab6d31becfef1bb9a8ea272578ecce0fccd7fbe497607cc46(createAccountInternal) --> 9f1d4b63259741678a7c0c7ed2019d4a91de568c643875c72c5810e44e5ba0fc(getAccountsByCustomerInternal)
end

subgraph srcwebuisrcmainjavacomibmcicscipbankliberty["src/webui/src/main/java/com/ibm/cics/cip/bankliberty"]
809d795912cdf644c8a0c70e143a9c0f1b35e08a4f490b3f02493cdcd0ac8053(createAccountExternal):::rootsStyle --> 1d896006c602d7cab6d31becfef1bb9a8ea272578ecce0fccd7fbe497607cc46(createAccountInternal)
end


      classDef mainFlowStyle color:#000000,fill:#7CB9F4
classDef rootsStyle color:#000000,fill:#00FFF4
classDef Style1 color:#000000,fill:#00FFAA
classDef Style2 color:#000000,fill:#FFFF00
classDef Style3 color:#000000,fill:#AA7CB9
```

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
