---
title: CreaccJson Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="11:4:4" line-data="public class CreaccJson">`CreaccJson`</SwmToken> class definition in the <SwmPath>[src/…/createaccount/CreaccJson.java](src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="11:4:4" line-data="public class CreaccJson">`CreaccJson`</SwmToken> class is designed to represent JSON data for creating an account in the customer services interface. It uses Jackson annotations to map JSON properties to Java fields.

We will cover:

1. Class-level annotations and their purpose.
2. Key fields and their JSON mappings.
3. Constructors and their roles.
4. Getter and setter methods for accessing and modifying fields.

# Class-level annotations

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="10">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="11:4:4" line-data="public class CreaccJson">`CreaccJson`</SwmToken> class uses the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="10:0:1" line-data="@JsonNaming(JsonPropertyNamingStrategy.class)">`@JsonNaming`</SwmToken> annotation to specify a custom naming strategy for JSON properties. This ensures that the JSON property names follow a specific format.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreaccJson
{



	@JsonProperty("COMM_ACC_TYPE")
	private String commAccType;
```

---

</SwmSnippet>

# Key fields and their JSON mappings

The class defines several fields, each annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="16:1:2" line-data="	@JsonProperty(&quot;COMM_ACC_TYPE&quot;)">`@JsonProperty`</SwmToken> to map JSON properties to Java fields. Here are some key fields:

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="10">

---

- <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="17:5:5" line-data="	private String commAccType;">`commAccType`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="60:1:1" line-data="		commCustno = String.format(&quot;%8s&quot;, accountNumber).replace(&quot; &quot;, &quot;0&quot;);">`commCustno`</SwmToken> represent the account type and customer number.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class CreaccJson
{



	@JsonProperty("COMM_ACC_TYPE")
	private String commAccType;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="25">

---

- <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="26:5:5" line-data="	private CreaccKeyJson commKey;">`commKey`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="29:5:5" line-data="	private float commInterestRate;">`commInterestRate`</SwmToken> represent the account key and interest rate.

```java
	@JsonProperty("COMM_KEY")
	private CreaccKeyJson commKey;

	@JsonProperty("COMM_INT_RT")
	private float commInterestRate;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="31">

---

- <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="32:5:5" line-data="	private int commOpened;">`commOpened`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="35:5:5" line-data="	private float commOverdraftLimit;">`commOverdraftLimit`</SwmToken> represent the account opened status and overdraft limit.

```java
	@JsonProperty("COMM_OPENED")
	private int commOpened;

	@JsonProperty("COMM_OVERDR_LIM")
	private float commOverdraftLimit;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="43">

---

- <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="44:5:5" line-data="	private float commAvailableBalance;">`commAvailableBalance`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="47:5:5" line-data="	private float commActualBalance;">`commActualBalance`</SwmToken> represent the available and actual balances.

```java
	@JsonProperty("COMM_AVAIL_BAL")
	private float commAvailableBalance;

	@JsonProperty("COMM_ACT_BAL")
	private float commActualBalance;
```

---

</SwmSnippet>

# Constructors

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="56">

---

The class provides two constructors. The parameterized constructor initializes key fields with formatted values, ensuring proper alignment and zero-padding where necessary.

```java
	public CreaccJson(String accountType, String accountNumber,
			float overdraftLimit, float interestRate)
	{
		commAccType = String.format("%-8s", accountType);
		commCustno = String.format("%8s", accountNumber).replace(" ", "0");

		commOverdraftLimit = overdraftLimit;
		commInterestRate = interestRate;

		commEyecatcher = "    ";
		commKey = new CreaccKeyJson();

	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="71">

---

The default constructor is also provided for cases where no initial values are needed.

```java
	public CreaccJson()
	{

	}
```

---

</SwmSnippet>

# Getter and setter methods

The class includes getter and setter methods for accessing and modifying its fields. Here are some examples:

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="77">

---

- Getter and setter for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="65:1:1" line-data="		commEyecatcher = &quot;    &quot;;">`commEyecatcher`</SwmToken>.

```java
	public String getCommEyecatcher()
	{
		return commEyecatcher;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="83">

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

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="89">

---

- Getter and setter for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="60:1:1" line-data="		commCustno = String.format(&quot;%8s&quot;, accountNumber).replace(&quot; &quot;, &quot;0&quot;);">`commCustno`</SwmToken>.

```java
	public String getCommCustno()
	{
		return commCustno;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="95">

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

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="125">

---

- Getter and setter for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" pos="29:5:5" line-data="	private float commInterestRate;">`commInterestRate`</SwmToken>.

```java
	public float getCommInterestRate()
	{
		return commInterestRate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreaccJson.java" line="131">

---

&nbsp;

```java
	public void setCommInterestRate(float commInterestRateIn)
	{
		commInterestRate = commInterestRateIn;
	}
```

---

</SwmSnippet>

These methods are essential for encapsulating the fields and providing controlled access to them.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
