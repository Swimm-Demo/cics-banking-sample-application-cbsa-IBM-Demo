---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: p2un3mt8
---
# Getting Started with Account List in WebUI

## Getting Started with Account List in WebUI

The Account List in the WebUI allows users to view a list of accounts associated with a particular customer. It retrieves account data from the backend using RESTful API calls and displays it in a user-friendly format. The Account List includes details such as account number, account type, available balance, actual balance, interest rate, overdraft limit, and statement dates. Users can filter the list based on various criteria like account number, customer number, and available balance. The data is fetched and processed by the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="25:4:4" line-data="public class AccountList">`AccountList`</SwmToken> class, which handles the retrieval and formatting of account information.

### Why and How to Use Account List

The Account List in the WebUI is a feature that allows users to view a list of accounts associated with a particular customer. It retrieves account data from the backend using RESTful API calls and displays it in a user-friendly format. The Account List includes details such as account number, account type, available balance, actual balance, interest rate, overdraft limit, and statement dates. Users can filter the list based on various criteria like account number, customer number, and available balance. The data is fetched and processed by the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="25:4:4" line-data="public class AccountList">`AccountList`</SwmToken> class, which handles the retrieval and formatting of account information.

### Where Account List is Used

The Account List is used in the Customer Services Interface and the Liberty UI.

### Usage Example

To display the accounts for a particular customer, click on 'List accounts belonging to customer'.

### Main Functions

There are several main functions in this folder. Some of them are <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="65:5:5" line-data="	public int getCount(String filter)">`getCount`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="69:1:1" line-data="			howMany(filter);">`howMany`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="167:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="349:5:5" line-data="	public Account getAccount(int i)">`getAccount`</SwmToken>, and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="355:5:5" line-data="	public int size()">`size`</SwmToken>. We will dive a little into <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="65:5:5" line-data="	public int getCount(String filter)">`getCount`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="69:1:1" line-data="			howMany(filter);">`howMany`</SwmToken>, and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="167:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken>.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="65">

---

#### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="65:5:5" line-data="	public int getCount(String filter)">`getCount`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="65:5:5" line-data="	public int getCount(String filter)">`getCount`</SwmToken> function returns the count of accounts based on a filter. If the list of accounts is empty, it calls the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="69:1:1" line-data="			howMany(filter);">`howMany`</SwmToken> function to determine the count.

```java
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

#### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="75:5:5" line-data="	public int howMany(String filter)">`howMany`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="75:5:5" line-data="	public int howMany(String filter)">`howMany`</SwmToken> function calculates the number of accounts that match a given filter. It interacts with the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="81:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken> to fetch account data and updates the count accordingly.

```java
	public int howMany(String filter)
	{

		// AND ACCOUNT_NUMBER = 0000000024
		// AND ACCOUNT_CUSTOMER_NUMBER

		AccountsResource myAccountsResource = new AccountsResource();
		Response myAccountsResponse = null;
		this.count = 0;

		try
		{
			if (filter.contains("AND ACCOUNT_AVAILABLE_BALANCE"))
			{
				// 01234567890123456789012345678901234567890
				// AND ACCOUNT_AVAILABLE_BALANCE <= 33558.0

				String operator = filter.substring(31, 32);
				BigDecimal balance = BigDecimal
						.valueOf(Double.parseDouble(filter.substring(34)));
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" line="167">

---

#### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="167:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="167:5:5" line-data="	public void doGet(int limit, int offset, String filter) throws IOException">`doGet`</SwmToken> function retrieves account data based on various filters and limits. It clears the current list of accounts, fetches new data from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/AccountList.java" pos="170:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken>, and updates the list of accounts.

```java
	public void doGet(int limit, int offset, String filter) throws IOException
	{

		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		try
		{

			if (filter.contains(" AND ACCOUNT_AVAILABLE_BALANCE"))
			{
				this.listOfAccounts.clear();
				String operator = filter.substring(31, 32);
				BigDecimal balance = BigDecimal
						.valueOf(Double.parseDouble(filter.substring(34)));

				myAccountsResponse = myAccountsResource
						.getAccountsByBalanceWithOffsetAndLimitExternal(balance,
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
