---
title: DelaccJson Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="12:4:4" line-data="public class DelaccJson">`DelaccJson`</SwmToken> class definition in the <SwmPath>[src/…/deleteaccount/DelaccJson.java](src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="12:4:4" line-data="public class DelaccJson">`DelaccJson`</SwmToken> class is designed to represent the JSON structure for the delete account operation in the customer services interface. It uses Jackson annotations to map JSON properties to Java fields.

We will cover:

1. The class definition and its annotations.
2. The fields and their JSON mappings.
3. The getter and setter methods for some of the fields.
4. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="340:5:5" line-data="	public String toString()">`toString`</SwmToken> method for debugging and logging purposes.

# Class definition and annotations

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="11">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="12:4:4" line-data="public class DelaccJson">`DelaccJson`</SwmToken> class is defined with Jackson annotations to handle JSON property naming and mapping.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class DelaccJson
{



	@JsonProperty("DELACC_SUCCESS")
	private String delaccSuccess;
```

---

</SwmSnippet>

# Fields and JSON mappings

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="20">

---

The class contains several fields, each annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="20:1:2" line-data="	@JsonProperty(&quot;DELACC_LAST_STMT_DT&quot;)">`@JsonProperty`</SwmToken> to specify the corresponding JSON property name. Here are some of the key fields:

```java
	@JsonProperty("DELACC_LAST_STMT_DT")
	private String delaccLastStatementDate;

	@JsonProperty("DELACC_INT_RATE")
	private float delaccInterestRate;

	@JsonProperty("DELACC_DEL_FAIL_CD")
	private int delaccDelFailCode;

	@JsonProperty("DELACC_SCODE")
	private String delaccSortcode;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="38">

---

&nbsp;

```java
	@JsonProperty("DELACC_ACC_TYPE")
	private AccountType delaccAccType;

	@JsonProperty("DELACC_NEXT_STMT_DT")
	private String delaccNextStatementDate;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="50">

---

&nbsp;

```java
	@JsonProperty("DELACC_CUSTNO")
	private String delaccCustno;

	@JsonProperty("DELACC_DEL_PCB3")
	private String delaccDelPcb3;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="62">

---

&nbsp;

```java
	@JsonProperty("DELACC_OVERDRAFT")
	private int delaccOverdraft;
	
	@JsonProperty("DELACC_FAIL_CD")
	private int delaccFailCode;
```

---

</SwmSnippet>

# Getter and setter methods

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="84">

---

The class provides getter and setter methods for accessing and modifying the fields. These methods are essential for encapsulation and data manipulation. Here are some examples:

```java
	public String getDelaccSuccess()
	{
		return delaccSuccess;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="96">

---

&nbsp;

```java
	public String getDelaccLastStatementDate()
	{
		return delaccLastStatementDate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="108">

---

&nbsp;

```java
	public float getDelaccInterestRate()
	{
		return delaccInterestRate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="120">

---

&nbsp;

```java
	public int getDelaccDelFailCode()
	{
		return delaccDelFailCode;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="168">

---

&nbsp;

```java
	public AccountType getDelaccAccType()
	{
		return delaccAccType;
	}
```

---

</SwmSnippet>

# ToString method

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" line="339">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="340:5:5" line-data="	public String toString()">`toString`</SwmToken> method is overridden to provide a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="342:4:4" line-data="		return &quot;DelaccJson [DelAccAccno=&quot; + delaccAccno + &quot;, DelAccAccType=&quot;">`DelaccJson`</SwmToken> object. This is useful for debugging and logging purposes.

```java
	@Override
	public String toString()
	{
		return "DelaccJson [DelAccAccno=" + delaccAccno + ", DelAccAccType="
				+ delaccAccType + ", DelAccActualBal=" + delaccActualBalance
				+ ", DelAccAvailBal=" + delaccAvailableBalance
				+ ", DelAccCustno=" + delaccCustno + ", DelAccDelApplid="
				+ delaccDelApplid + ", DelAccDelFailCd=" + delaccDelFailCode
				+ ", DelAccDelPcb1=" + delaccDelPcb1 + ", DelAccDelPcb2="
				+ delaccDelPcb2 + ", DelAccDelPcb3=" + delaccDelPcb3
				+ ", DelAccDelSuccess=" + delaccDelSuccess + ", DelAccEye="
				+ delaccEye + ", DelAccDelFailCd=" + delaccFailCode
				+ ", DelAccIntRate=" + delaccInterestRate
				+ ", DelAccLastStmtDt=" + delaccLastStatementDate
				+ ", DelAccNextStmtDt=" + delaccNextStatementDate
				+ ", DelAccOpened=" + delaccOpened + ", DelAccOverdraft="
				+ delaccOverdraft + ", DelAccScode=" + delaccSortcode
				+ ", DelAccDelSuccess=" + delaccSuccess + "]";
	}

}
```

---

</SwmSnippet>

This document covered the main aspects of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deleteaccount/DelaccJson.java" pos="12:4:4" line-data="public class DelaccJson">`DelaccJson`</SwmToken> class, including its definition, fields, and key methods.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
