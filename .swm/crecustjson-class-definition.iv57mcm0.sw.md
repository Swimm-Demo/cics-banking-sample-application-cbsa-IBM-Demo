---
title: CrecustJson Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" pos="12:4:4" line-data="public class CrecustJson">`CrecustJson`</SwmToken> class definition.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" pos="12:4:4" line-data="public class CrecustJson">`CrecustJson`</SwmToken> class is designed to represent customer data in JSON format for the customer services interface. It uses Jackson annotations to map JSON properties to Java fields.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" pos="12:4:4" line-data="public class CrecustJson">`CrecustJson`</SwmToken> class.
2. Key fields and their initial values.
3. Constructors for creating instances.
4. Getter and setter methods for accessing and modifying fields.

# Class definition and annotations

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="11">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" pos="12:4:4" line-data="public class CrecustJson">`CrecustJson`</SwmToken> class is defined with Jackson annotations to specify JSON property naming and mapping.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class CrecustJson
{



	@JsonProperty("COMM_EYECATCHER")
	private String commEyecatcher = "    ";
```

---

</SwmSnippet>

# Key fields and initial values

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="20">

---

The class contains several fields representing customer data. Each field is annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" pos="20:1:2" line-data="	@JsonProperty(&quot;COMM_KEY&quot;)">`@JsonProperty`</SwmToken> to map it to a specific JSON property.

```java
	@JsonProperty("COMM_KEY")
	private CreaccKeyJson commKey = new CreaccKeyJson();

	@JsonProperty("COMM_NAME")
	private String commName;

	@JsonProperty("COMM_ADDRESS")
	private String commAddress;

	@JsonProperty("COMM_DATE_OF_BIRTH")
	private String commDateOfBirth;

	@JsonProperty("COMM_CREDIT_SCORE")
	private int commCreditScore = 0;

	@JsonProperty("COMM_CS_REVIEW_DATE")
	private String commCsReviewDate = "0";

	@JsonProperty("COMM_SUCCESS")
	private String commSuccess;

	@JsonProperty("COMM_FAIL_CODE")
	private String commFailCode;
```

---

</SwmSnippet>

# Constructors

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="47">

---

The class provides two constructors: a default constructor and a parameterized constructor for initializing key fields.

```java
	public CrecustJson(String custName, String custAddress, String custDob)
	{
		commName = custName;
		commAddress = custAddress;
		commDateOfBirth = custDob;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="55">

---

&nbsp;

```java
	public CrecustJson()
	{

	}
```

---

</SwmSnippet>

# Getter and setter methods

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="61">

---

Getter and setter methods are provided for each field to allow access and modification of the data.

```java
	public String getCommEyecatcher()
	{
		return commEyecatcher;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="67">

---

&nbsp;

```java
	public void setCommEyecatcher(String commEyecatcherIn)
	{
		commEyecatcher = commEyecatcherIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="73">

---

&nbsp;

```java
	public CreaccKeyJson getCommKey()
	{
		return commKey;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="79">

---

&nbsp;

```java
	public void setCommKey(CreaccKeyJson commKeyIn)
	{
		commKey = commKeyIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="85">

---

&nbsp;

```java
	public String getCommName()
	{
		return commName;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="91">

---

&nbsp;

```java
	public void setCommName(String commNameIn)
	{
		commName = commNameIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="97">

---

&nbsp;

```java
	public String getCommAddress()
	{
		return commAddress;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="103">

---

&nbsp;

```java
	public void setCommAddress(String commAddressIn)
	{
		commAddress = commAddressIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="109">

---

&nbsp;

```java
	public String getCommDateOfBirth()
	{
		return commDateOfBirth;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="115">

---

&nbsp;

```java
	public void setCommDateOfBirth(String commDateOfBirthIn)
	{
		commDateOfBirth = commDateOfBirthIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="121">

---

&nbsp;

```java
	public int getCommCreditScore()
	{
		return commCreditScore;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="127">

---

&nbsp;

```java
	public void setCommCreditScore(int commCreditScoreIn)
	{
		commCreditScore = commCreditScoreIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="133">

---

&nbsp;

```java
	public String getCommCsReviewDate()
	{
		return commCsReviewDate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="139">

---

&nbsp;

```java
	public void setCommCsReviewDate(String commCsReviewDateIn)
	{
		commCsReviewDate = commCsReviewDateIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="145">

---

&nbsp;

```java
	public String getCommSuccess()
	{
		return commSuccess;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="151">

---

&nbsp;

```java
	public void setCommSuccess(String commSuccessIn)
	{
		commSuccess = commSuccessIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="157">

---

&nbsp;

```java
	public String getCommFailCode()
	{
		return commFailCode;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" line="163">

---

&nbsp;

```java
	public void setCommFailCode(String commFailCodeIn)
	{
		commFailCode = commFailCodeIn;
	}
```

---

</SwmSnippet>

This structure ensures that the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CrecustJson.java" pos="12:4:4" line-data="public class CrecustJson">`CrecustJson`</SwmToken> class can be easily serialized and deserialized to and from JSON, making it suitable for use in RESTful APIs and other JSON-based communication.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
