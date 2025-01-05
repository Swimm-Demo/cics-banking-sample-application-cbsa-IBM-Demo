---
title: AccountDetails Java Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" pos="12:4:4" line-data="public class AccountDetails">`AccountDetails`</SwmToken> Java class definition.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" pos="12:4:4" line-data="public class AccountDetails">`AccountDetails`</SwmToken> class is designed to represent account details in a structured format, making it easy to serialize and deserialize JSON data.

We will cover:

1. The purpose of the class and its annotations.
2. The main properties of the class.
3. Key methods for accessing and modifying the properties.
4. Utility methods for string representation.

# Class definition and annotations

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" line="11">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" pos="12:4:4" line-data="public class AccountDetails">`AccountDetails`</SwmToken> class is annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" pos="11:0:1" line-data="@JsonNaming(JsonPropertyNamingStrategy.class)">`@JsonNaming`</SwmToken> to specify a custom naming strategy for JSON properties. This ensures that the JSON keys match the expected format when serializing and deserializing.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class AccountDetails
{



	private static final String FLOAT_FORMAT = "%.02f";
```

---

</SwmSnippet>

# Class properties

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" line="19">

---

The class contains several properties, each annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" pos="19:1:2" line-data="	@JsonProperty(&quot;COMM_ACTUAL_BAL&quot;)">`@JsonProperty`</SwmToken> to map the JSON keys to the class fields. This is crucial for correctly parsing JSON data into the class and vice versa.

```java
	@JsonProperty("COMM_ACTUAL_BAL")
	private float commActualBalance;

	@JsonProperty("COMM_AVAIL_BAL")
	private float commAvailableBalance;

	@JsonProperty("COMM_SCODE")
	private int commSortcode;

	@JsonProperty("COMM_INT_RATE")
	private float commInterestRate;

	@JsonProperty("COMM_EYE")
	private String commEye;

	@JsonProperty("COMM_OPENED")
	private int commOpened;

	@JsonProperty("COMM_CUSTNO")
	private int commCustno;

	@JsonProperty("COMM_NEXT_STMT_DT")
	private int commNextStatementDate;

	@JsonProperty("COMM_ACC_TYPE")
	private String commAccType;

	@JsonProperty("COMM_OVERDRAFT")
	private int commOverdraft;

	@JsonProperty("COMM_ACCNO")
	private int commAccno;

	@JsonProperty("COMM_LAST_STMT_DT")
	private int commLastStatementDate;
```

---

</SwmSnippet>

# Getter and setter methods

The class provides getter and setter methods for each property. These methods are essential for accessing and modifying the values of the properties.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" line="56">

---

For example, here are the getter and setter methods for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" pos="58:3:3" line-data="		return commActualBalance;">`commActualBalance`</SwmToken>:

```java
	public float getCommActualBalance()
	{
		return commActualBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" line="62">

---

&nbsp;

```java
	public void setCommActualBalance(float commActualBalanceIn)
	{
		commActualBalance = commActualBalanceIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" line="68">

---

Similarly, here are the getter and setter methods for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" pos="70:3:3" line-data="		return commAvailableBalance;">`commAvailableBalance`</SwmToken>:

```java
	public float getCommAvailableBalance()
	{
		return commAvailableBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" line="74">

---

&nbsp;

```java
	public void setCommAvailableBalance(float commAvailableBalanceIn)
	{
		commAvailableBalance = commAvailableBalanceIn;
	}
```

---

</SwmSnippet>

# String representation methods

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" line="200">

---

The class includes methods to generate string representations of the account details. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" pos="201:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a basic string representation, while the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" pos="214:5:5" line-data="	public String toPrettyString()">`toPrettyString`</SwmToken> method offers a more formatted and readable output.

```java
	@Override
	public String toString()
	{
		return "AccountDetails [CommAccno=" + commAccno + ", CommAccType="
				+ commAccType + ", CommActualBal=" + commActualBalance
				+ ", CommCustno=" + commCustno + ", CommEye=" + commEye
				+ ", CommIntRate=" + commInterestRate + ", CommLastStmtDt="
				+ commLastStatementDate + ", CommNextStmtDt="
				+ commNextStatementDate + ", CommOpened=" + commOpened
				+ ", CommOverdraft=" + commOverdraft + ", CommScode="
				+ commSortcode + "]";
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" line="214">

---

&nbsp;

```java
	public String toPrettyString()
	{
		String output = "";
		output += "Account Number:       "
				+ OutputFormatUtils.leadingZeroes(8, commAccno) + "\n"
				+ "Sort Code:            " + String.format("%06d", commSortcode)
				+ "\n" + "Customer Number:      "
				+ OutputFormatUtils.leadingZeroes(10, commCustno) + "\n"
				+ "Account Type:         " + commAccType + "\n"
				+ "Available Balance:    "
				+ String.format(FLOAT_FORMAT, commAvailableBalance) + "\n"
				+ "Actual Balance:       "
				+ String.format(FLOAT_FORMAT, commActualBalance) + "\n"
				+ "Interest Rate:        "
				+ String.format(FLOAT_FORMAT, commInterestRate) + "\n"
				+ "Overdraft:            " + commOverdraft + "\n"
				+ "Account Opened: " + OutputFormatUtils.date(commOpened) + "\n"
				+ "Next Statement Date:  "
				+ OutputFormatUtils.date(commNextStatementDate) + "\n"
				+ "Last Statement Date:  "
				+ OutputFormatUtils.date(commLastStatementDate) + "\n";
		return output;
	}

}
```

---

</SwmSnippet>

These methods are useful for logging and debugging purposes, allowing developers to easily inspect the state of an <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/AccountDetails.java" pos="12:4:4" line-data="public class AccountDetails">`AccountDetails`</SwmToken> object.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
