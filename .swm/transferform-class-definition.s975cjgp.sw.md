---
title: TransferForm Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="9:4:4" line-data="public class TransferForm">`TransferForm`</SwmToken> class definition in the <SwmPath>[src/…/paymentinterface/TransferForm.java](src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="9:4:4" line-data="public class TransferForm">`TransferForm`</SwmToken> class is designed to handle the data structure for transferring funds, ensuring that all necessary fields are validated and accessible.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="9:4:4" line-data="public class TransferForm">`TransferForm`</SwmToken> class.
2. The fields and their validations.
3. The constructors and their roles.
4. The getter and setter methods.

# Class definition and fields

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="9">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="9:4:4" line-data="public class TransferForm">`TransferForm`</SwmToken> class is defined to encapsulate the data required for a transfer operation. It includes fields for account number, debit status, amount, and organisation, each with specific validation constraints.

```java
public class TransferForm
{



	// accno
	@NotNull
	@Size(max = 8)
	private String acctNumber;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="19">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="17:5:5" line-data="	private String acctNumber;">`acctNumber`</SwmToken> field is annotated with <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="19:1:2" line-data="	@NotNull">`@NotNull`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="16:1:9" line-data="	@Size(max = 8)">`@Size(max = 8)`</SwmToken> to ensure it is not null and does not exceed 8 characters.

```java
	@NotNull
	private boolean debit = true;

	@NotNull
	private Float amount;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="25">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="20:5:5" line-data="	private boolean debit = true;">`debit`</SwmToken> field is a boolean with a default value of `true`, and the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="23:5:5" line-data="	private Float amount;">`amount`</SwmToken> field is a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="23:3:3" line-data="	private Float amount;">`Float`</SwmToken> that must not be null.

```java
	@NotNull
	@Size(max = 16)
	private String organisation;
```

---

</SwmSnippet>

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="27:5:5" line-data="	private String organisation;">`organisation`</SwmToken> field is also annotated with <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="15:1:2" line-data="	@NotNull">`@NotNull`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="26:1:9" line-data="	@Size(max = 16)">`@Size(max = 16)`</SwmToken> to ensure it is not null and does not exceed 16 characters.

# Constructors

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="30">

---

The class includes a default constructor and a parameterized constructor. The default constructor is necessary for frameworks that require a no-argument constructor.

```java
	public TransferForm()
	{

	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="36">

---

The parameterized constructor allows for the initialization of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="36:20:20" line-data="	public TransferForm(@NotNull @Size(max = 8) String acctNumber,">`acctNumber`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="37:6:6" line-data="			@NotNull Float amount, @NotNull @Size(max = 16) String organisation)">`amount`</SwmToken>, and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="37:24:24" line-data="			@NotNull Float amount, @NotNull @Size(max = 16) String organisation)">`organisation`</SwmToken> fields upon object creation.

```java
	public TransferForm(@NotNull @Size(max = 8) String acctNumber,
			@NotNull Float amount, @NotNull @Size(max = 16) String organisation)
	{
		this.acctNumber = acctNumber;
		this.amount = amount;
		this.organisation = organisation;
	}
```

---

</SwmSnippet>

# Getter and setter methods

The class provides getter and setter methods for each field to allow for encapsulation and controlled access.

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="45">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="45:5:5" line-data="	public String getAcctNumber()">`getAcctNumber`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="51:5:5" line-data="	public void setAcctNumber(String acctNumber)">`setAcctNumber`</SwmToken> methods manage the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="47:3:3" line-data="		return acctNumber;">`acctNumber`</SwmToken> field.

```java
	public String getAcctNumber()
	{
		return acctNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="51">

---

&nbsp;

```java
	public void setAcctNumber(String acctNumber)
	{
		this.acctNumber = acctNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="57">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="57:5:5" line-data="	public boolean isDebit()">`isDebit`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="69:5:5" line-data="	public void setDebit(String type)">`setDebit`</SwmToken> methods manage the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="59:3:3" line-data="		return debit;">`debit`</SwmToken> field. Note that there is an overloaded <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="69:5:5" line-data="	public void setDebit(String type)">`setDebit`</SwmToken> method that accepts a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="17:3:3" line-data="	private String acctNumber;">`String`</SwmToken> and sets the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="59:3:3" line-data="		return debit;">`debit`</SwmToken> field based on whether the string equals "Debit".

```java
	public boolean isDebit()
	{
		return debit;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="69">

---

&nbsp;

```java
	public void setDebit(String type)
	{
		this.debit = type.equals("Debit");
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="75">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="75:5:5" line-data="	public Float getAmount()">`getAmount`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="81:5:5" line-data="	public void setAmount(Float amount)">`setAmount`</SwmToken> methods manage the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="77:3:3" line-data="		return amount;">`amount`</SwmToken> field.

```java
	public Float getAmount()
	{
		return amount;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="81">

---

&nbsp;

```java
	public void setAmount(Float amount)
	{
		this.amount = amount;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="87">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="87:5:5" line-data="	public String getOrganisation()">`getOrganisation`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="93:5:5" line-data="	public void setOrganisation(String organisation)">`setOrganisation`</SwmToken> methods manage the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="89:3:3" line-data="		return organisation;">`organisation`</SwmToken> field.

```java
	public String getOrganisation()
	{
		return organisation;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="93">

---

&nbsp;

```java
	public void setOrganisation(String organisation)
	{
		this.organisation = organisation;
	}
```

---

</SwmSnippet>

# String representation

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" line="99">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="100:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a string representation of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="102:4:4" line-data="		return &quot;TransferForm [acctNumber=&quot; + acctNumber + &quot;, amount=&quot; + amount">`TransferForm`</SwmToken> object, which is useful for logging and debugging.

```java
	@Override
	public String toString()
	{
		return "TransferForm [acctNumber=" + acctNumber + ", amount=" + amount
				+ ", organisation=" + organisation + "]";
	}
}
```

---

</SwmSnippet>

This covers the main aspects of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/TransferForm.java" pos="9:4:4" line-data="public class TransferForm">`TransferForm`</SwmToken> class, focusing on its fields, constructors, and methods.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
