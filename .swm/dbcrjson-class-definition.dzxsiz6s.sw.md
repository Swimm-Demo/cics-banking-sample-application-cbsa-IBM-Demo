---
title: DbcrJson Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken> class definition in the <SwmPath>[src/…/paymentinterface/DbcrJson.java](src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken> class is designed to represent a JSON object for debit and credit transactions in a payment interface. It uses Jackson annotations for JSON serialization and deserialization.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken> class.
2. The initialization of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken> object.
3. The handling of specific fields within the class.
4. The conversion of <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="46:5:5" line-data="	public DbcrJson(TransferForm transferForm)">`TransferForm`</SwmToken> data into <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken>.

# Class definition and annotations

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="10">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken> class is defined with Jackson annotations to control the JSON property naming strategy and to map Java fields to JSON properties.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class DbcrJson
{


	@JsonProperty("COMM_ACCNO")
	private String commAccno;
```

---

</SwmSnippet>

# Field definitions

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="18">

---

The class contains several fields representing different aspects of a debit/credit transaction. Each field is annotated with <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="18:1:2" line-data="	@JsonProperty(&quot;COMM_AMT&quot;)">`@JsonProperty`</SwmToken> to specify the corresponding JSON property name.

```java
	@JsonProperty("COMM_AMT")
	private float commAmt;

	@JsonProperty("COMM_SORTC")
	private int commSortC = 0;

	@JsonProperty("COMM_AV_BAL")
	private int commAvBal = 0;

	@JsonProperty("COMM_ACT_BAL")
	private int commActBal = 0;

	@JsonProperty("COMM_ORIGIN")
	private OriginJson commOrigin;

	@JsonProperty("COMM_SUCCESS")
	private String commSuccess = " ";

	@JsonProperty("COMM_FAIL_CODE")
	private String commFailCode = " ";
```

---

</SwmSnippet>

# Default constructor

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="40">

---

A default constructor is provided to allow the creation of an empty <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="40:3:3" line-data="	public DbcrJson()">`DbcrJson`</SwmToken> object. This is necessary for frameworks that require a no-argument constructor.

```java
	public DbcrJson()
	{

	}
```

---

</SwmSnippet>

# Constructor with <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="46:5:5" line-data="	public DbcrJson(TransferForm transferForm)">`TransferForm`</SwmToken> parameter

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="46">

---

This constructor initializes the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="46:3:3" line-data="	public DbcrJson(TransferForm transferForm)">`DbcrJson`</SwmToken> object using data from a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="46:5:5" line-data="	public DbcrJson(TransferForm transferForm)">`TransferForm`</SwmToken> object. It sets the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="48:1:1" line-data="		commOrigin = new OriginJson(transferForm.getOrganisation());">`commOrigin`</SwmToken> field and formats the account number.

```java
	public DbcrJson(TransferForm transferForm)
	{
		commOrigin = new OriginJson(transferForm.getOrganisation());

		// accno
		// Pads out with zeroes to the length specified
		commAccno = String.format("%8s", transferForm.getAcctNumber())
				.replace(" ", "0");

		// Make the amount positive or negative based on wether debit or credit
		// is selected
		commAmt = transferForm.isDebit() ? (transferForm.getAmount() * -1)
				: transferForm.getAmount();
	}
```

---

</SwmSnippet>

# Getter and setter methods

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="62">

---

The class includes getter and setter methods for each field. These methods allow other parts of the application to access and modify the fields of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken> object.

```java
	public String getCommAccno()
	{
		return commAccno;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="74">

---

&nbsp;

```java
	public float getCommAmt()
	{
		return commAmt;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="86">

---

&nbsp;

```java
	public int getCommSortC()
	{
		return commSortC;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="98">

---

&nbsp;

```java
	public int getCommAvBal()
	{
		return commAvBal;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="110">

---

&nbsp;

```java
	public int getCommActBal()
	{
		return commActBal;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="122">

---

&nbsp;

```java
	public OriginJson getCommOrigin()
	{
		return commOrigin;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="134">

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

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="146">

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

# <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="159:5:5" line-data="	public String toString()">`toString`</SwmToken> method

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="158">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="159:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a string representation of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="161:4:4" line-data="		return &quot;DbcrJson [CommAccno=&quot; + commAccno + &quot;, CommActBal=&quot;">`DbcrJson`</SwmToken> object, which is useful for logging and debugging.

```java
	@Override
	public String toString()
	{
		return "DbcrJson [CommAccno=" + commAccno + ", CommActBal="
				+ commActBal + ", CommAmt=" + commAmt + ", CommAvBal="
				+ commAvBal + ", CommFailCode=" + commFailCode
				+ ", CommOrigin=" + commOrigin.toString() + ", CommSortC="
				+ commSortC + ", CommSuccess=" + commSuccess + "]";
	}

}
```

---

</SwmSnippet>

This document has covered the main ideas of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken> class, including its purpose, initialization, field handling, and conversion from <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="46:5:5" line-data="	public DbcrJson(TransferForm transferForm)">`TransferForm`</SwmToken> data.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
