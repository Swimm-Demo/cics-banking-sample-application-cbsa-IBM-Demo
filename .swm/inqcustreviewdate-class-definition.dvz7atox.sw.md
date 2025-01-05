---
title: InqCustReviewDate Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="11:4:4" line-data="public class InqCustReviewDate">`InqCustReviewDate`</SwmToken> class definition in the <SwmPath>[src/…/customerenquiry/InqCustReviewDate.java](src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="11:4:4" line-data="public class InqCustReviewDate">`InqCustReviewDate`</SwmToken> class is designed to handle customer review date information, including year, month, and day. It uses Jackson annotations for JSON serialization and deserialization.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="11:4:4" line-data="public class InqCustReviewDate">`InqCustReviewDate`</SwmToken> class.
2. The use of Jackson annotations for JSON property mapping.
3. The getter and setter methods for the class properties.
4. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="69:5:5" line-data="	public String toString()">`toString`</SwmToken> method for string representation of the date.

# Class definition and JSON property mapping

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" line="10">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="11:4:4" line-data="public class InqCustReviewDate">`InqCustReviewDate`</SwmToken> class is defined with Jackson annotations to map JSON properties to Java fields. This is crucial for serializing and deserializing JSON data.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class InqCustReviewDate
{



	@JsonProperty("INQCUST_CS_REVIEW_YYYY")
	private int inqcustCsReviewYyyy;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" line="19">

---

The class has three properties: <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="17:5:5" line-data="	private int inqcustCsReviewYyyy;">`inqcustCsReviewYyyy`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="20:5:5" line-data="	private int inqcustCsReviewMm;">`inqcustCsReviewMm`</SwmToken>, and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="23:5:5" line-data="	private int inqcustCsReviewDd;">`inqcustCsReviewDd`</SwmToken>, each annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="19:1:2" line-data="	@JsonProperty(&quot;INQCUST_CS_REVIEW_MM&quot;)">`@JsonProperty`</SwmToken> to specify the JSON field names.

```java
	@JsonProperty("INQCUST_CS_REVIEW_MM")
	private int inqcustCsReviewMm;

	@JsonProperty("INQCUST_CS_REVIEW_DD")
	private int inqcustCsReviewDd;
```

---

</SwmSnippet>

# Constructor

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" line="26">

---

The class includes a default constructor. This is necessary for Jackson to create instances of the class during deserialization.

```java
	public InqCustReviewDate()
	{
		super();
	}
```

---

</SwmSnippet>

# Getter and setter methods

Getter and setter methods are provided for each property. These methods allow other parts of the application to access and modify the values of the properties.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" line="32">

---

For the year property:

```java
	public int getInqcustCsReviewYyyy()
	{
		return inqcustCsReviewYyyy;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" line="38">

---

&nbsp;

```java
	public void setInqcustCsReviewYyyy(int inqcustCsReviewYyyyIn)
	{
		inqcustCsReviewYyyy = inqcustCsReviewYyyyIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" line="44">

---

For the month property:

```java
	public int getInqcustCsReviewMm()
	{
		return inqcustCsReviewMm;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" line="50">

---

&nbsp;

```java
	public void setInqcustCsReviewMm(int inqcustCsReviewMmIn)
	{
		inqcustCsReviewMm = inqcustCsReviewMmIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" line="56">

---

For the day property:

```java
	public int getInqcustCsReviewDd()
	{
		return inqcustCsReviewDd;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" line="62">

---

&nbsp;

```java
	public void setInqcustCsReviewDd(int inqcustCsReviewDdIn)
	{
		inqcustCsReviewDd = inqcustCsReviewDdIn;
	}
```

---

</SwmSnippet>

# String representation

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" line="68">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="69:5:5" line-data="	public String toString()">`toString`</SwmToken> method is overridden to provide a string representation of the date in the format `DD/MM/YYYY`. This is useful for logging and debugging purposes.

```java
	@Override
	public String toString()
	{
		return inqcustCsReviewDd + "/" + inqcustCsReviewMm + "/"
				+ inqcustCsReviewYyyy;
	}

}
```

---

</SwmSnippet>

This document has covered the main aspects of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/InqCustReviewDate.java" pos="11:4:4" line-data="public class InqCustReviewDate">`InqCustReviewDate`</SwmToken> class, including its purpose, JSON property mapping, getter and setter methods, and string representation.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
