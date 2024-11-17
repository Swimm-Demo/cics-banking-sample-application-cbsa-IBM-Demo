---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: oljcgd01
---
# Exploring DeleteAccountJson

## Overview

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:1:1" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`DeleteAccountJson`</SwmToken> class represents the JSON structure for deleting an account. It contains a field <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="31:3:3" line-data="		return delaccCommarea;">`delaccCommarea`</SwmToken>, which is an instance of <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="29:3:3" line-data="	public DelaccJson getDelaccCommarea()">`DelaccJson`</SwmToken>, representing the communication area for the delete account operation. The class includes methods to get and set the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="31:3:3" line-data="		return delaccCommarea;">`delaccCommarea`</SwmToken> field, allowing for manipulation of the account deletion data.

## Methods

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:1:1" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`DeleteAccountJson`</SwmToken> class provides several methods to interact with the account deletion data.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" line="29">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="29:5:5" line-data="	public DelaccJson getDelaccCommarea()">`getDelaccCommarea`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="29:5:5" line-data="	public DelaccJson getDelaccCommarea()">`getDelaccCommarea`</SwmToken> method retrieves the communication area for the delete account operation.

```java
	public DelaccJson getDelaccCommarea()
	{
		return delaccCommarea;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" line="35">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="35:5:5" line-data="	public void setDelaccCommarea(DelaccJson delaccCommareaIn)">`setDelaccCommarea`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="35:5:5" line-data="	public void setDelaccCommarea(DelaccJson delaccCommareaIn)">`setDelaccCommarea`</SwmToken> method sets the communication area for the delete account operation.

```java
	public void setDelaccCommarea(DelaccJson delaccCommareaIn)
	{
		delaccCommarea = delaccCommareaIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" line="49">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="49:5:5" line-data="	public String toPrettyString()">`toPrettyString`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="49:5:5" line-data="	public String toPrettyString()">`toPrettyString`</SwmToken> method formats the account details into a human-readable string, including information such as account number, sort code, account type, customer number, interest rate, overdraft limit, available balance, actual balance, account opened date, last statement date, and next statement date.

```java
	public String toPrettyString()
	{
		DelaccJson accInfo = delaccCommarea;
		String output = "";
		output += "Account Number: "
				+ OutputFormatUtils.leadingZeroes(8, accInfo.getDelaccAccno())
				+ "\n" + "Sort Code: " + accInfo.getDelaccSortcode() + "\n"
				+ "Account Type: " + accInfo.getDelaccAccType() + "\n"
				+ "Customer Number: "
				+ OutputFormatUtils.leadingZeroes(10, accInfo.getDelaccCustno())
				+ "\n" + "Interest Rate: "
				+ String.format(FLOAT_FORMAT, accInfo.getDelaccInterestRate())
				+ "\n" + "Overdraft Limit: " + accInfo.getDelaccOverdraft()
				+ "\n" + "Available Balance: "
				+ String.format(FLOAT_FORMAT,
						accInfo.getDelaccAvailableBalance())
				+ "\n" + "Actual Balance: "
				+ String.format(FLOAT_FORMAT, accInfo.getDelaccActualBalance())
				+ "\n" + "Account Opened: "
				+ OutputFormatUtils.date(accInfo.getDelaccOpened()) + "\n"
				+ "Last Statement Date: "
```

---

</SwmSnippet>

## Usage

To remove an account, click on 'Delete account' from the landing page.

## Example

In this example, the response from the delete account operation is parsed into a <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:1:1" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`DeleteAccountJson`</SwmToken> object.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="805">

---

The response from the delete account operation is logged and then parsed into a <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:1:1" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`DeleteAccountJson`</SwmToken> object using <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:9:9" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`ObjectMapper`</SwmToken>.

```java
				log.info(responseBody);
				DeleteAccountJson responseObj = new ObjectMapper()
						.readValue(responseBody, DeleteAccountJson.class);
```

---

</SwmSnippet>

## Additional Methods in <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="29:3:3" line-data="	public DelaccJson getDelaccCommarea()">`DelaccJson`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DeleteAccountJson.java" pos="29:3:3" line-data="	public DelaccJson getDelaccCommarea()">`DelaccJson`</SwmToken> class, which is used within <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:1:1" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`DeleteAccountJson`</SwmToken>, provides additional methods to interact with various account details.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="90">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="90:5:5" line-data="	public void setDelaccSuccess(String delaccSuccessIn)">`setDelaccSuccess`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="90:5:5" line-data="	public void setDelaccSuccess(String delaccSuccessIn)">`setDelaccSuccess`</SwmToken> method sets the success status of the delete account operation.

```java
	public void setDelaccSuccess(String delaccSuccessIn)
	{
		delaccSuccess = delaccSuccessIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="114">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="114:5:5" line-data="	public void setDelaccInterestRate(float delaccInterestRateIn)">`setDelaccInterestRate`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="114:5:5" line-data="	public void setDelaccInterestRate(float delaccInterestRateIn)">`setDelaccInterestRate`</SwmToken> method sets the interest rate of the account.

```java
	public void setDelaccInterestRate(float delaccInterestRateIn)
	{
		delaccInterestRate = delaccInterestRateIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="207">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="207:5:5" line-data="	public float getDelaccActualBalance()">`getDelaccActualBalance`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="207:5:5" line-data="	public float getDelaccActualBalance()">`getDelaccActualBalance`</SwmToken> method retrieves the actual balance of the account.

```java
	public float getDelaccActualBalance()
	{
		return delaccActualBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="213">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="213:5:5" line-data="	public void setDelaccActualBalance(float delaccActualBalanceIn)">`setDelaccActualBalance`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="213:5:5" line-data="	public void setDelaccActualBalance(float delaccActualBalanceIn)">`setDelaccActualBalance`</SwmToken> method sets the actual balance of the account.

```java
	public void setDelaccActualBalance(float delaccActualBalanceIn)
	{
		delaccActualBalance = delaccActualBalanceIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="219">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="219:5:5" line-data="	public float getDelaccAvailableBalance()">`getDelaccAvailableBalance`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="219:5:5" line-data="	public float getDelaccAvailableBalance()">`getDelaccAvailableBalance`</SwmToken> method retrieves the available balance of the account.

```java
	public float getDelaccAvailableBalance()
	{
		return delaccAvailableBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="225">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="225:5:5" line-data="	public void setDelaccAvailableBalance(float delaccAvailableBalanceIn)">`setDelaccAvailableBalance`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="225:5:5" line-data="	public void setDelaccAvailableBalance(float delaccAvailableBalanceIn)">`setDelaccAvailableBalance`</SwmToken> method sets the available balance of the account.

```java
	public void setDelaccAvailableBalance(float delaccAvailableBalanceIn)
	{
		delaccAvailableBalance = delaccAvailableBalanceIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="279">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="279:5:5" line-data="	public int getDelaccOverdraft()">`getDelaccOverdraft`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="279:5:5" line-data="	public int getDelaccOverdraft()">`getDelaccOverdraft`</SwmToken> method retrieves the overdraft limit of the account.

```java
	public int getDelaccOverdraft()
	{
		return delaccOverdraft;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="285">

---

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="285:5:5" line-data="	public void setDelaccOverdraft(int delaccOverdraftIn)">`setDelaccOverdraft`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="285:5:5" line-data="	public void setDelaccOverdraft(int delaccOverdraftIn)">`setDelaccOverdraft`</SwmToken> method sets the overdraft limit of the account.

```java
	public void setDelaccOverdraft(int delaccOverdraftIn)
	{
		delaccOverdraft = delaccOverdraftIn;
	}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
