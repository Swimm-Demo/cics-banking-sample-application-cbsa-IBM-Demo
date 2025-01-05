---
title: CreateAccountForm Java Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="48:3:3" line-data="	public CreateAccountForm()">`CreateAccountForm`</SwmToken> Java class definition.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="48:3:3" line-data="	public CreateAccountForm()">`CreateAccountForm`</SwmToken> class is designed to handle the data required for creating a new account in the banking system. It includes fields for customer number, account type, overdraft limit, and interest rate, along with their respective getters and setters.

We will cover:

1. The purpose of the class constructor.
2. The validation constraints on the fields.
3. The getters and setters for the fields.
4. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="79:5:5" line-data="	public String toString()">`toString`</SwmToken> method for debugging and logging.

# Class constructor

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" line="48">

---

The class constructor initializes the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="48:3:3" line-data="	public CreateAccountForm()">`CreateAccountForm`</SwmToken> object. This is essential for creating instances of the class.

```java
	public CreateAccountForm()
	{
		super();
	}
```

---

</SwmSnippet>

# Validation constraints

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" line="12">

---

Validation constraints ensure that the data provided for creating an account is valid. For example, the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="14:5:5" line-data="	private String custNumber;">`custNumber`</SwmToken> field must not be null and must have a maximum size of 8 characters.

```java
	@NotNull
	@Size(max = 8)
	private String custNumber;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" line="16">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="17:5:5" line-data="	private AccountType accountType;">`accountType`</SwmToken> field must not be null, ensuring that an account type is always specified.

```java
	@NotNull
	private AccountType accountType;

	private int overdraftLimit;
```

---

</SwmSnippet>

# Getters and setters

Getters and setters are used to access and modify the fields of the class. They are crucial for encapsulation and data integrity.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" line="54">

---

The getter and setter for the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="56:3:3" line-data="		return custNumber;">`custNumber`</SwmToken> field:

```java
	public String getCustNumber()
	{
		return custNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" line="60">

---

&nbsp;

```java
	public void setCustNumber(String custNumber)
	{
		this.custNumber = custNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" line="21">

---

The getter and setter for the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="26:3:3" line-data="		return overdraftLimit;">`overdraftLimit`</SwmToken> field:

```java
	private float interestRate;


	public int getOverdraftLimit()
	{
		return overdraftLimit;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" line="30">

---

&nbsp;

```java
	public void setOverdraftLimit(int overdraftLimit)
	{
		this.overdraftLimit = overdraftLimit;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" line="36">

---

The getter and setter for the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="38:3:3" line-data="		return interestRate;">`interestRate`</SwmToken> field:

```java
	public float getInterestRate()
	{
		return interestRate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" line="42">

---

&nbsp;

```java
	public void setInterestRate(float interestRate)
	{
		this.interestRate = interestRate;
	}
```

---

</SwmSnippet>

# ToString method

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" line="78">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="79:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="81:4:4" line-data="		return &quot;CreateAccountForm [accountType=&quot; + accountType + &quot;, custNumber=&quot;">`CreateAccountForm`</SwmToken> object. This is useful for debugging and logging purposes.

```java
	@Override
	public String toString()
	{
		return "CreateAccountForm [accountType=" + accountType + ", custNumber="
				+ custNumber + ", interestRate=" + interestRate
				+ ", overdraftLimit=" + overdraftLimit + "]";
	}

}
```

---

</SwmSnippet>

This document has covered the main design decisions and implementation details of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountForm.java" pos="48:3:3" line-data="	public CreateAccountForm()">`CreateAccountForm`</SwmToken> class.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
