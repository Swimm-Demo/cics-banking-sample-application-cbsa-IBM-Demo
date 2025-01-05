---
title: OriginJson Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="11:4:4" line-data="public class OriginJson">`OriginJson`</SwmToken> class definition in the <SwmPath>[src/…/paymentinterface/OriginJson.java](src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="11:4:4" line-data="public class OriginJson">`OriginJson`</SwmToken> class is designed to map JSON properties to Java fields, facilitating the serialization and deserialization of JSON data. This is particularly useful for handling data exchange in the payment interface.

We will cover:

1. The class definition and its annotations.
2. The constructor and its purpose.
3. Key methods for setting and getting properties.
4. The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="125:5:5" line-data="	public String toString()">`toString`</SwmToken> method for debugging and logging.

# Class definition and annotations

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" line="10">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="11:4:4" line-data="public class OriginJson">`OriginJson`</SwmToken> class is annotated with <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="10:0:1" line-data="@JsonNaming(JsonPropertyNamingStrategy.class)">`@JsonNaming`</SwmToken> to specify a custom naming strategy for JSON properties. This ensures that the JSON properties are correctly mapped to the Java fields.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class OriginJson
{


	@JsonProperty("COMM_APPLID")
	private String commApplid;
```

---

</SwmSnippet>

# Constructor

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" line="40">

---

The class includes a default constructor and a parameterized constructor. The parameterized constructor is used to initialize the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="45:1:1" line-data="		commApplid = paddedOrg.substring(0, 8);">`commApplid`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="46:1:1" line-data="		commUserid = paddedOrg.substring(8);">`commUserid`</SwmToken> fields by splitting the input string.

```java
	public OriginJson(String organisation)
	{
		// APPLID and USERID are both used to contain the organisation data, so
		// it's split into two 8 char strings
		String paddedOrg = String.format("%-16s", organisation);
		commApplid = paddedOrg.substring(0, 8);
		commUserid = paddedOrg.substring(8);
	}
```

---

</SwmSnippet>

# Key methods

The class provides getter and setter methods for each field. These methods are essential for accessing and modifying the field values.

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" line="50">

---

For example, the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="50:5:5" line-data="	public void setOrganisation(String organisation)">`setOrganisation`</SwmToken> method sets the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="53:1:1" line-data="		commApplid = paddedOrg.substring(0, 8);">`commApplid`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="54:1:1" line-data="		commUserid = paddedOrg.substring(8);">`commUserid`</SwmToken> fields by splitting the input string.

```java
	public void setOrganisation(String organisation)
	{
		String paddedOrg = String.format("%-16s", organisation);
		commApplid = paddedOrg.substring(0, 8);
		commUserid = paddedOrg.substring(8);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" line="58">

---

Getter methods like <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="58:5:5" line-data="	public String getCommApplid()">`getCommApplid`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="70:5:5" line-data="	public String getCommUserid()">`getCommUserid`</SwmToken> are used to retrieve the values of the respective fields.

```java
	public String getCommApplid()
	{
		return commApplid;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" line="70">

---

&nbsp;

```java
	public String getCommUserid()
	{
		return commUserid;
	}
```

---

</SwmSnippet>

# <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="125:5:5" line-data="	public String toString()">`toString`</SwmToken> method

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" line="124">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="125:5:5" line-data="	public String toString()">`toString`</SwmToken> method is overridden to provide a string representation of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="127:4:4" line-data="		return &quot;OriginJson [CommApplid=&quot; + commApplid + &quot;, CommFacilityName=&quot;">`OriginJson`</SwmToken> object. This is useful for debugging and logging purposes.

```java
	@Override
	public String toString()
	{
		return "OriginJson [CommApplid=" + commApplid + ", CommFacilityName="
				+ commFacilityName + ", CommFaciltype=" + commFaciltype
				+ ", CommNetwrkId=" + commNetwrkId + ", CommUserid="
				+ commUserid + ", Fill0=" + fill0 + "]";
	}
}
```

---

</SwmSnippet>

This document has covered the main aspects of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="11:4:4" line-data="public class OriginJson">`OriginJson`</SwmToken> class, including its definition, constructors, key methods, and the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/OriginJson.java" pos="125:5:5" line-data="	public String toString()">`toString`</SwmToken> method.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
