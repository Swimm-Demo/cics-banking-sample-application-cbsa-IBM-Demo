---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: zx456qkk
---
# Customer Deletion Process

In this document, we will explain the process of deleting a customer and their associated accounts from the database. The process involves multiple steps, including initiating the deletion, handling external and internal requests, and ensuring all associated accounts are properly deleted.

The flow starts with initiating the customer deletion process. This involves calling an external function to handle the deletion request. The external function then calls an internal function to perform the actual deletion. The internal function first deletes all associated accounts by calling another function for each account. Once all accounts are deleted, the customer record is deleted, and the transaction is logged.

## Flow drill down

```mermaid
graph TD;
      subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
dc895fb1af8153ed69103a95f6d988807f3046b61264670935e6edaec5324d94(deleteFromDB) --> d1ad1388ee4fe24b99482da1a910946ae278b376df0f5f64079d6fb2a2adaa8a(deleteCustomerExternal)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
d1ad1388ee4fe24b99482da1a910946ae278b376df0f5f64079d6fb2a2adaa8a(deleteCustomerExternal) --> a78f499cf4d64ed3fc68304653f09260c3e734def3c416ddaf940cb617b87755(deleteCustomerInternal)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
a78f499cf4d64ed3fc68304653f09260c3e734def3c416ddaf940cb617b87755(deleteCustomerInternal) --> 1fb4076eaa29e9a08dc358071532b95d0dec9f80e70969c89370d0e981445cce(deleteAccountInternal)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
a78f499cf4d64ed3fc68304653f09260c3e734def3c416ddaf940cb617b87755(deleteCustomerInternal) --> 9f1d4b63259741678a7c0c7ed2019d4a91de568c643875c72c5810e44e5ba0fc(getAccountsByCustomerInternal)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyweb["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web"]
a78f499cf4d64ed3fc68304653f09260c3e734def3c416ddaf940cb617b87755(deleteCustomerInternal) --> 99ca350546699e2f4fc11413ad87ac17219ba83fd3e8f11113297f59f35c22ea(deleteCustomer)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyweb["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web"]
9f1d4b63259741678a7c0c7ed2019d4a91de568c643875c72c5810e44e5ba0fc(getAccountsByCustomerInternal) --> 2ad4232e406fa96a04bfcded8864ac207154294d2be003ba59f1bc5e0b047210(getAccounts)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
9f1d4b63259741678a7c0c7ed2019d4a91de568c643875c72c5810e44e5ba0fc(getAccountsByCustomerInternal) --> f42ad07b3c1434bec6dd81ea8109f8aebe7c804c1bb06f7252e816311ef05f35(getCustomerInternal)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyweb["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web"]
1fb4076eaa29e9a08dc358071532b95d0dec9f80e70969c89370d0e981445cce(deleteAccountInternal) --> 1eda9700aca767d83e2c8b6599f81f9b0c9c9d79f6c4c411869acef8c9e46df8(deleteAccount)
end

subgraph srcwebuisrcmainjavacomibmcicscipbanklibertyapijson["src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json"]
1fb4076eaa29e9a08dc358071532b95d0dec9f80e70969c89370d0e981445cce(deleteAccountInternal) --> 73cff60224df86393cc17c7d5ebe1aa5958d66358ef4e36ec3149acf2ccea1d2(writeDeleteAccountInternal)
end


      classDef mainFlowStyle color:#000000,fill:#7CB9F4
classDef rootsStyle color:#000000,fill:#00FFF4
classDef Style1 color:#000000,fill:#00FFAA
classDef Style2 color:#000000,fill:#FFFF00
classDef Style3 color:#000000,fill:#AA7CB9
```

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" line="220">

---

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="220:5:5" line-data="	public boolean deleteFromDB()">`deleteFromDB`</SwmToken> Function

Diving into the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="220:5:5" line-data="	public boolean deleteFromDB()">`deleteFromDB`</SwmToken> function, it initiates the customer deletion process by calling <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Customer.java" pos="224:9:9" line-data="		Response myCustomerResponse = myCustomerResource.deleteCustomerExternal(">`deleteCustomerExternal`</SwmToken> and handles the response to update the customer details if the deletion is successful.

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

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="547">

---

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="550:5:5" line-data="	public Response deleteCustomerExternal(@PathParam(JSON_ID) Long id)">`deleteCustomerExternal`</SwmToken> Function

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="550:5:5" line-data="	public Response deleteCustomerExternal(@PathParam(JSON_ID) Long id)">`deleteCustomerExternal`</SwmToken> function is responsible for handling the external request to delete a customer. It calls <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="554:7:7" line-data="		Response myResponse = deleteCustomerInternal(id);">`deleteCustomerInternal`</SwmToken> to perform the actual deletion and then terminates the data access session.

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

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="563:5:5" line-data="	public Response deleteCustomerInternal(Long id)">`deleteCustomerInternal`</SwmToken> Function

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="563:5:5" line-data="	public Response deleteCustomerInternal(Long id)">`deleteCustomerInternal`</SwmToken> function performs the core logic of deleting a customer. It first deletes all associated accounts by calling <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" pos="1241:5:5" line-data="	public Response deleteAccountInternal(Long accountNumber)">`deleteAccountInternal`</SwmToken> for each account. If successful, it proceeds to delete the customer record and logs the transaction.

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

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" line="1241">

---

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" pos="1241:5:5" line-data="	public Response deleteAccountInternal(Long accountNumber)">`deleteAccountInternal`</SwmToken> Function

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountsResource.java" pos="1241:5:5" line-data="	public Response deleteAccountInternal(Long accountNumber)">`deleteAccountInternal`</SwmToken> function handles the deletion of individual accounts associated with the customer. It ensures that each account is properly deleted and logs the transaction.

```java
	public Response deleteAccountInternal(Long accountNumber)
	{
		logger.entering(this.getClass().getName(), DELETE_ACCOUNT);
		Response myResponse = null;

		JSONObject response = new JSONObject();

		Integer sortCode = this.getSortCode();

		com.ibm.cics.cip.bankliberty.web.db2.Account db2Account = new Account();

		db2Account = db2Account.deleteAccount(accountNumber.intValue(),
				sortCode.intValue());
		if (db2Account != null)
		{
			response.put(JSON_SORT_CODE, db2Account.getSortcode().trim());
			response.put("id", db2Account.getAccountNumber());
			response.put(JSON_CUSTOMER_NUMBER, db2Account.getCustomerNumber());
			response.put(JSON_ACCOUNT_TYPE, db2Account.getType().trim());
			response.put(JSON_AVAILABLE_BALANCE,
					BigDecimal.valueOf(db2Account.getAvailableBalance()));
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
