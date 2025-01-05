---
title: InqAccczJson Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" pos="13:4:4" line-data="public class InqAccczJson">`InqAccczJson`</SwmToken> class definition in the <SwmPath>[src/…/listaccounts/InqAccczJson.java](src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" pos="13:4:4" line-data="public class InqAccczJson">`InqAccczJson`</SwmToken> class is designed to represent JSON data for account inquiries. It uses Jackson annotations to map JSON properties to Java fields and includes getter and setter methods for each field.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" pos="12:0:1" line-data="@JsonNaming(JsonPropertyNamingStrategy.class)">`@JsonNaming`</SwmToken> annotation.
2. The fields and their JSON mappings.
3. The getter and setter methods for key fields.
4. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" pos="110:5:5" line-data="	public String toString()">`toString`</SwmToken> method for debugging and logging.

# Json naming strategy

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" line="12">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" pos="12:0:1" line-data="@JsonNaming(JsonPropertyNamingStrategy.class)">`@JsonNaming`</SwmToken> annotation specifies the naming strategy for JSON properties. This ensures that the JSON properties follow a consistent naming convention.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqAccczJson
{



	@JsonProperty("COMM_FAIL_CODE")
	private int commFailCode;
```

---

</SwmSnippet>

# Fields and JSON mappings

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" line="21">

---

The class contains several fields that are mapped to JSON properties using the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" pos="21:1:2" line-data="	@JsonProperty(&quot;CUSTOMER_NUMBER&quot;)">`@JsonProperty`</SwmToken> annotation. This allows the fields to be serialized and deserialized correctly.

```java
	@JsonProperty("CUSTOMER_NUMBER")
	private int customerNumber;

	@JsonProperty("ACCOUNT_DETAILS")
	private List<AccountDetails> accountDetails;

	@JsonProperty("COMM_PCB_POINTER")
	private String commPcbPointer;

	@JsonProperty("CUSTOMER_FOUND")
	private String customerFound;

	@JsonProperty("COMM_SUCCESS")
	private String commSuccess;

	
	public int getCommFailCode()
	{
		return commFailCode;
	}
```

---

</SwmSnippet>

# Getter and setter methods

Getter and setter methods are provided for each field to allow access and modification. Here are examples for some key fields:

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" line="33">

---

Getter and setter for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" pos="39:3:3" line-data="		return commFailCode;">`commFailCode`</SwmToken>:

```java
	@JsonProperty("COMM_SUCCESS")
	private String commSuccess;

	
	public int getCommFailCode()
	{
		return commFailCode;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" line="43">

---

&nbsp;

```java
	public void setCommFailCode(int commFailCodeIn)
	{
		commFailCode = commFailCodeIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" line="49">

---

Getter and setter for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" pos="51:3:3" line-data="		return customerNumber;">`customerNumber`</SwmToken>:

```java
	public int getCustomerNumber()
	{
		return customerNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" line="55">

---

&nbsp;

```java
	public void setCustomerNumber(int customerNumberIn)
	{
		customerNumber = customerNumberIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" line="61">

---

Getter and setter for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" pos="63:3:3" line-data="		return accountDetails;">`accountDetails`</SwmToken>:

```java
	public List<AccountDetails> getAccountDetails()
	{
		return accountDetails;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" line="67">

---

&nbsp;

```java
	public void setAccountDetails(List<AccountDetails> accountDetailsIn)
	{
		accountDetails = accountDetailsIn;
	}
```

---

</SwmSnippet>

# ToString method

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" line="109">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/listaccounts/InqAccczJson.java" pos="110:5:5" line-data="	public String toString()">`toString`</SwmToken> method is overridden to provide a string representation of the object. This is useful for debugging and logging purposes.

```java
	@Override
	public String toString()
	{
		return "InqAccczJson [AccountDetails=" + accountDetails
				+ ", CommFailCode=" + commFailCode + ", CommPcbPointer="
				+ commPcbPointer + ", CommSuccess=" + commSuccess
				+ ", CustomerFound=" + customerFound + ", CustomerNumber="
				+ customerNumber + "]";
	}
}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
