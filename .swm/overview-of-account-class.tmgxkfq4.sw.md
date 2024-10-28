---
title: Overview of Account Class
---
# Overview of Account Class

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="25:4:4" line-data="public class Account">`Account`</SwmToken> class represents a bank account and contains various attributes such as customer number, sort code, account number, type, interest rate, date opened, overdraft limit, last statement date, next statement date, available balance, and actual balance. This class is essential for managing bank account data and ensuring data integrity through encapsulation.

# Methods in Account Class

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="25:4:4" line-data="public class Account">`Account`</SwmToken> class includes methods to get and set these attributes, ensuring encapsulation and data integrity. It also provides methods to interact with the database, such as adding, updating, and deleting account records. These methods are crucial for maintaining the consistency and accuracy of account data.

# Logging Account Details

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="117:5:5" line-data="	public void showInfo()">`showInfo`</SwmToken> method logs the account details, which is useful for debugging and monitoring purposes. This method helps developers and administrators track the state of an account at any given time.

# Checking Account in Database

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="494:5:5" line-data="	public boolean inDB()">`inDB`</SwmToken> method checks if the account exists in the database and updates the account object with the latest data from the database. This method ensures that the account object always reflects the most current data.

# Updating Account Details

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="287:5:5" line-data="	public boolean updateThis()">`updateThis`</SwmToken> method updates the account details in the database and refreshes the account object with the updated data. This method is essential for keeping the account information up-to-date and accurate.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="287">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="287:5:5" line-data="	public boolean updateThis()">`updateThis`</SwmToken> method in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="25:4:4" line-data="public class Account">`Account`</SwmToken> class updates the account details in the database. It uses the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="289:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken> and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="291:1:1" line-data="		AccountJSON myAccountJSON = new AccountJSON();">`AccountJSON`</SwmToken> classes to set the new account details and sends an update request to the database. If the update is successful, it retrieves the updated account data.

```java
	public boolean updateThis()
	{
		AccountsResource myAccountsResource = new AccountsResource();

		AccountJSON myAccountJSON = new AccountJSON();

		myAccountJSON.setAccountType(this.getType());
		myAccountJSON.setInterestRate(this.getInterestRate());
		myAccountJSON.setOverdraft(this.getOverdraftLimit());
		myAccountJSON.setSortCode(this.getSortcode());

		Response myAccountsResponse = myAccountsResource.updateAccountInternal(
				Long.parseLong(this.getAccountNumber()), myAccountJSON);

		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		if (myAccountsResponse.getStatus() == 200)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
