---
title: UpdcustJson Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" pos="11:4:4" line-data="public class UpdcustJson">`UpdcustJson`</SwmToken> class definition in the <SwmPath>[src/…/updatecustomer/UpdcustJson.java](src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" pos="11:4:4" line-data="public class UpdcustJson">`UpdcustJson`</SwmToken> class is designed to handle JSON data for updating customer information. It uses Jackson annotations for JSON property mapping and includes various fields and methods to manage customer data.

We will cover:

1. Class definition and JSON property mapping
2. Constructor logic for initializing fields
3. Getter and setter methods for accessing and modifying fields
4. String representation of the object

# Class definition and JSON property mapping

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" line="10">

---

The class is defined with Jackson annotations to map JSON properties to Java fields. This ensures that JSON data can be easily converted to and from Java objects.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdcustJson
{



	@JsonProperty("COMM_EYE")
	private String commEye = "    ";
```

---

</SwmSnippet>

# Constructor logic for initializing fields

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" line="47">

---

The constructor initializes the fields with provided values, ensuring proper formatting and conversion where necessary. This is important for handling input data correctly.

```java
	public UpdcustJson(String commCustnoIn, String commNameIn,
			String commAddressIn, String commDateOfBirthIn,
			int commCreditScoreIn, String commCreditScoreReviewDateIn)
	{
		// Some values need to be padded out when not full
		commCustno = String.format("%10s", commCustnoIn).replace(" ", "0");
		if (!commNameIn.equals(" "))
			commName = String.format("%-60s", commNameIn);
		if (!commAddressIn.equals(" "))
			commAddress = String.format("%-160s", commAddressIn);
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" line="58">

---

The constructor also includes logic to convert string inputs to integers, preventing <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" pos="59:5:5" line-data="		// a NumberFormatException on an empty String, as 0 would be okay by">`NumberFormatException`</SwmToken> for empty strings.

```java
		// These convert strings to ints - they use ternary operators to prevent
		// a NumberFormatException on an empty String, as 0 would be okay by
		// default.
		commDateOfBirth = commDateOfBirthIn.equals("") ? 0
				: Integer.parseInt(commDateOfBirthIn);
		commCreditScoreReviewDate = commCreditScoreReviewDateIn.equals("") ? 0
				: Integer.parseInt(commCreditScoreReviewDateIn);
```

---

</SwmSnippet>

# Getter and setter methods for accessing and modifying fields

The class includes various getter and setter methods to access and modify the fields. These methods are essential for interacting with the customer data.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" line="77">

---

For example, the getter and setter for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" pos="79:3:3" line-data="		return commEye;">`commEye`</SwmToken>:

```java
	public String getCommEye()
	{
		return commEye;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" line="83">

---

&nbsp;

```java
	public void setCommEye(String commEyeIn)
	{
		commEye = commEyeIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" line="101">

---

Similarly, the getter and setter for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" pos="103:3:3" line-data="		return commCustno;">`commCustno`</SwmToken>:

```java
	public String getCommCustno()
	{
		return commCustno;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" line="107">

---

&nbsp;

```java
	public void setCommCustno(String commCustnoIn)
	{
		commCustno = commCustnoIn;
	}
```

---

</SwmSnippet>

# String representation of the object

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" line="197">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" pos="198:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdcustJson.java" pos="200:4:4" line-data="		return &quot;UpdcustJson [CommAddress=&quot; + commAddress + &quot;, CommCreditScore=&quot;">`UpdcustJson`</SwmToken> object. This is useful for debugging and logging purposes.

```java
	@Override
	public String toString()
	{
		return "UpdcustJson [CommAddress=" + commAddress + ", CommCreditScore="
				+ commCreditScore + ", CommCsReviewDate="
				+ commCreditScoreReviewDate + ", CommCustno=" + commCustno
				+ ", CommDob=" + commDateOfBirth + ", CommEye=" + commEye
				+ ", CommName=" + commName + ", CommScode=" + commSortcode
				+ ", CommUpdFailCd=" + commUpdateFailCode
				+ ", CommmUpdSuccess=" + commUpdateSuccess + "]";
	}

}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
