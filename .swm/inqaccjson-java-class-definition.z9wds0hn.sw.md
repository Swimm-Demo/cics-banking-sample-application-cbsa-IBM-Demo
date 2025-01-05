---
title: InqaccJson Java Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" pos="11:4:4" line-data="public class InqaccJson">`InqaccJson`</SwmToken> Java class definition.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" pos="11:4:4" line-data="public class InqaccJson">`InqaccJson`</SwmToken> class is designed to represent account inquiry data in JSON format. It uses Jackson annotations to map JSON properties to Java fields, ensuring proper serialization and deserialization.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" pos="11:4:4" line-data="public class InqaccJson">`InqaccJson`</SwmToken> class.
2. The use of Jackson annotations for JSON property mapping.
3. Key fields and their significance.
4. Important methods for accessing and modifying the data.

# Class definition and annotations

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" line="10">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" pos="11:4:4" line-data="public class InqaccJson">`InqaccJson`</SwmToken> class is defined with Jackson annotations to handle JSON property naming and mapping. This ensures that the JSON data is correctly serialized and deserialized.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqaccJson
{


	@JsonProperty("INQACC_PCB1_POINTER")
	private String inqaccPcb1Pointer;
```

---

</SwmSnippet>

# Key fields

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" line="18">

---

The class contains several fields representing different aspects of an account inquiry. Each field is annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" pos="18:1:2" line-data="	@JsonProperty(&quot;INQACC_OVERDRAFT&quot;)">`@JsonProperty`</SwmToken> to specify the corresponding JSON property name.

```java
	@JsonProperty("INQACC_OVERDRAFT")
	private int inqaccOverdraft;

	@JsonProperty("INQACC_LAST_STMT_DT")
	private int inqaccLastStatementDate;

	@JsonProperty("INQACC_SCODE")
	private int inqaccSortcode;

	@JsonProperty("INQACC_ACTUAL_BAL")
	private float inqaccActualBalance;

	@JsonProperty("INQACC_ACCNO")
	private int inqaccAccno;

	@JsonProperty("INQACC_OPENED")
	private int inqaccOpened;

	@JsonProperty("INQACC_CUSTNO")
	private int inqaccCustno;

	@JsonProperty("INQACC_ACC_TYPE")
	private String inqaccAccType;

	@JsonProperty("INQACC_NEXT_STMT_DT")
	private int inqaccNextStatementDate;

	@JsonProperty("INQACC_AVAIL_BAL")
	private float inqaccAvailableBalance;

	@JsonProperty("INQACC_EYE")
	private String inqaccEyecatcher;

	@JsonProperty("INQACC_SUCCESS")
	private String inqaccSuccess;

	@JsonProperty("INQACC_INT_RATE")
	private float inqaccInterestRate;
```

---

</SwmSnippet>

# Getter and setter methods

The class provides getter and setter methods for each field. These methods allow other parts of the application to access and modify the data.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" line="58">

---

For example, the getter and setter methods for the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" pos="60:3:3" line-data="		return inqaccPcb1Pointer;">`inqaccPcb1Pointer`</SwmToken> field:

```java
	public String getInqaccPcb1Pointer()
	{
		return inqaccPcb1Pointer;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" line="64">

---

&nbsp;

```java
	public void setInqaccPcb1Pointer(String inqaccPcb1PointerIn)
	{
		inqaccPcb1Pointer = inqaccPcb1PointerIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" line="70">

---

Similarly, the getter and setter methods for the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" pos="72:3:3" line-data="		return inqaccOverdraft;">`inqaccOverdraft`</SwmToken> field:

```java
	public int getInqaccOverdraft()
	{
		return inqaccOverdraft;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" line="76">

---

&nbsp;

```java
	public void setInqaccOverdraft(int inqaccOverdraftIn)
	{
		inqaccOverdraft = inqaccOverdraftIn;
	}
```

---

</SwmSnippet>

# <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" pos="227:5:5" line-data="	public String toString()">`toString`</SwmToken> method

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" line="226">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" pos="227:5:5" line-data="	public String toString()">`toString`</SwmToken> method is overridden to provide a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/InqaccJson.java" pos="229:4:4" line-data="		return &quot;InqaccJson [InqAccAccno=&quot; + inqaccAccno + &quot;, InqAccAccType=&quot;">`InqaccJson`</SwmToken> object. This is useful for debugging and logging purposes.

```java
	@Override
	public String toString()
	{
		return "InqaccJson [InqAccAccno=" + inqaccAccno + ", InqAccAccType="
				+ inqaccAccType + ", InqAccActualBal=" + inqaccActualBalance
				+ ", InqAccAvailBal=" + inqaccAvailableBalance
				+ ", InqAccCustno=" + inqaccCustno + ", InqAccEye="
				+ inqaccEyecatcher + ", InqAccIntRate=" + inqaccInterestRate
				+ ", InqAccLastStmtDt=" + inqaccLastStatementDate
				+ ", InqAccNextStmtDt=" + inqaccNextStatementDate
				+ ", InqAccOpened=" + inqaccOpened + ", InqAccOverdraft="
				+ inqaccOverdraft + ", InqAccPcb1Pointer=" + inqaccPcb1Pointer
				+ ", InqAccScode=" + inqaccSortcode + ", InqAccSuccess="
				+ inqaccSuccess + "]";
	}

}
```

---

</SwmSnippet>

This method concatenates the values of all fields into a single string, making it easy to inspect the object's state.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
