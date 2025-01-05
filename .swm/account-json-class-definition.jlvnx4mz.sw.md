---
title: Account JSON Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" pos="18:4:4" line-data="public class AccountJSON">`AccountJSON`</SwmToken> class definition in <SwmPath>[src/…/json/AccountJSON.java](src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java)</SwmPath>.

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" pos="18:4:4" line-data="public class AccountJSON">`AccountJSON`</SwmToken> class is designed to represent account data in JSON format. It includes various fields and methods to handle account-related information.

We will cover:

1. Class definition and annotations
2. Important fields
3. Key methods

# Class definition and annotations

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="11">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" pos="18:4:4" line-data="public class AccountJSON">`AccountJSON`</SwmToken> class is defined to represent account data in JSON format. It uses annotations to ensure proper validation and mapping of form parameters.

```java
import javax.validation.constraints.NotNull;
import javax.ws.rs.FormParam;

/**
 * This class describes the parts of the Account record in JSON format
 */

public class AccountJSON
{
```

---

</SwmSnippet>

# Important fields

The class includes several fields that represent different aspects of an account. Some fields are mandatory, while others are optional.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="21">

---

Mandatory fields:

```java
	@NotNull
	@FormParam("accountType")
	String accountType;

	@NotNull
	@FormParam("customerNumber")
	String customerNumber;
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="35">

---

&nbsp;

```java
	@FormParam("nextStatementDate")
	Date nextStatementDate;

	@NotNull
	@FormParam("sortCode")
	String sortCode;

	@NotNull
	@FormParam("overdraft")
	Integer overdraft;
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="29">

---

Optional fields:

```java
	@FormParam("dateOpened")
	Date dateOpened;

	@FormParam("lastStatementDate")
	Date lastStatementDate;
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="46">

---

&nbsp;

```java
	@FormParam("actualBalance")
	BigDecimal actualBalance;

	@FormParam("availableBalance")
	BigDecimal availableBalance;

	@FormParam("interestRate")
	BigDecimal interestRate;
```

---

</SwmSnippet>

# Key methods

The class includes getter and setter methods for each field to allow easy access and modification of the account data.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="56">

---

Getter and setter for <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" pos="58:3:3" line-data="		return interestRate;">`interestRate`</SwmToken>:

```java
	public BigDecimal getInterestRate()
	{
		return interestRate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="62">

---

&nbsp;

```java
	public void setInterestRate(BigDecimal interestRate)
	{
		this.interestRate = interestRate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="67">

---

Getter and setter for <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" pos="72:3:3" line-data="		return accountType;">`accountType`</SwmToken>:

```java
	String id;


	public String getAccountType()
	{
		return accountType;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="76">

---

&nbsp;

```java
	public void setAccountType(String accountType)
	{
		this.accountType = accountType;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="190">

---

Validation method for <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" pos="22:5:5" line-data="	@FormParam(&quot;accountType&quot;)">`accountType`</SwmToken>:

```java
	// Account type must be one of the below
	public boolean validateType(String accountType2)
	{
		if (accountType2.equalsIgnoreCase("ISA"))
		{
			return true;
		}
		if (accountType2.equalsIgnoreCase("MORTGAGE"))
		{
			return true;
		}
		if (accountType2.equalsIgnoreCase("LOAN"))
		{
			return true;
		}
		if (accountType2.equalsIgnoreCase("SAVING"))
		{
			return true;
		}
		return accountType2.equalsIgnoreCase("CURRENT");
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" line="212">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/AccountJSON.java" pos="212:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a string representation of the account data, which is useful for logging and debugging.

```java
	public String toString()
	{
		return "accountNumber="+this.getId()+",customerNumber="+this.getCustomerNumber()+",dateOpened="+this.getDateOpened()+",actualBalance="+this.getActualBalance()+",sortCode="+this.getSortCode()+",availableBalance="+this.getAvailableBalance()+",accountType="+this.getAccountType()+",overdraft="+this.getOverdraft()+",lastStatementDate="+this.getLastStatementDate()+",nextStatementDate="+this.getNextStatementDate();
	}
	
}
```

---

</SwmSnippet>

This structure ensures that the account data is well-defined, validated, and easily accessible.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
