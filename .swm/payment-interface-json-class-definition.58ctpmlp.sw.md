---
title: Payment Interface JSON Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="11:4:4" line-data="public class PaymentInterfaceJson">`PaymentInterfaceJson`</SwmToken> class definition in the <SwmPath>[src/…/paymentinterface/PaymentInterfaceJson.java](src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="11:4:4" line-data="public class PaymentInterfaceJson">`PaymentInterfaceJson`</SwmToken> class is designed to handle JSON serialization and deserialization for payment interface data. It uses Jackson annotations to map JSON properties to Java fields.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="11:4:4" line-data="public class PaymentInterfaceJson">`PaymentInterfaceJson`</SwmToken> class.
2. The constructor and its role in initializing the class.
3. The getter and setter methods for accessing and modifying the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="17:5:5" line-data="	private DbcrJson payDbCr;">`payDbCr`</SwmToken> field.
4. The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="45:5:5" line-data="	public String toString()">`toString`</SwmToken> method for debugging and logging purposes.

# Class definition and JSON naming strategy

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" line="10">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="11:4:4" line-data="public class PaymentInterfaceJson">`PaymentInterfaceJson`</SwmToken> class is annotated with <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="10:0:1" line-data="@JsonNaming(JsonPropertyNamingStrategy.class)">`@JsonNaming`</SwmToken> to specify a custom naming strategy for JSON properties. This ensures that the JSON properties follow a specific naming convention.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class PaymentInterfaceJson
{



	@JsonProperty("PAYDBCR")
	private DbcrJson payDbCr;
```

---

</SwmSnippet>

# Constructor for initialization

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" line="20">

---

The class provides two constructors: a default constructor and one that initializes the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="17:5:5" line-data="	private DbcrJson payDbCr;">`payDbCr`</SwmToken> field using a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="26:5:5" line-data="	public PaymentInterfaceJson(TransferForm transferForm)">`TransferForm`</SwmToken> object. The latter is useful for creating an instance of <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="20:3:3" line-data="	public PaymentInterfaceJson()">`PaymentInterfaceJson`</SwmToken> with data from a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="26:5:5" line-data="	public PaymentInterfaceJson(TransferForm transferForm)">`TransferForm`</SwmToken>.

```java
	public PaymentInterfaceJson()
	{

	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" line="26">

---

&nbsp;

```java
	public PaymentInterfaceJson(TransferForm transferForm)
	{
		payDbCr = new DbcrJson(transferForm);
	}
```

---

</SwmSnippet>

# Getter and setter methods

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" line="32">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="32:5:5" line-data="	public DbcrJson getPAYDBCR()">`getPAYDBCR`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="38:5:5" line-data="	public void setPAYDBCR(DbcrJson payDbCrIn)">`setPAYDBCR`</SwmToken> methods are used to access and modify the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="34:3:3" line-data="		return payDbCr;">`payDbCr`</SwmToken> field. These methods are essential for interacting with the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="34:3:3" line-data="		return payDbCr;">`payDbCr`</SwmToken> data.

```java
	public DbcrJson getPAYDBCR()
	{
		return payDbCr;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" line="38">

---

&nbsp;

```java
	public void setPAYDBCR(DbcrJson payDbCrIn)
	{
		this.payDbCr = payDbCrIn;
	}
```

---

</SwmSnippet>

# <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="45:5:5" line-data="	public String toString()">`toString`</SwmToken> method for debugging

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" line="44">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="45:5:5" line-data="	public String toString()">`toString`</SwmToken> method is overridden to provide a string representation of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="47:4:4" line-data="		return &quot;PaymentInterfaceJson [PAYDBCR=&quot; + payDbCr.toString() + &quot;]&quot;;">`PaymentInterfaceJson`</SwmToken> object. This is useful for debugging and logging purposes, as it allows developers to easily inspect the contents of the object.

```java
	@Override
	public String toString()
	{
		return "PaymentInterfaceJson [PAYDBCR=" + payDbCr.toString() + "]";
	}
}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
