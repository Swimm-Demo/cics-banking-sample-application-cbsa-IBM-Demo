---
title: CreateCustomerForm Java Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="9:4:4" line-data="public class CreateCustomerForm">`CreateCustomerForm`</SwmToken> Java class definition.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="9:4:4" line-data="public class CreateCustomerForm">`CreateCustomerForm`</SwmToken> class is designed to handle customer data input for creating new customer records. It includes fields for customer name, address, and date of birth, along with validation constraints and utility methods.

We will cover:

1. The purpose of the class constructor.
2. The validation constraints on the fields.
3. The getter and setter methods for the fields.
4. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="71:5:5" line-data="	public String toString()">`toString`</SwmToken> method for debugging and logging.
5. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="78:5:5" line-data="	public boolean isValidTitle()">`isValidTitle`</SwmToken> method for validating the customer's title.

# Class constructor

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="26">

---

The constructor initializes the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="26:3:3" line-data="	public CreateCustomerForm()">`CreateCustomerForm`</SwmToken> object. It calls the superclass constructor to ensure proper initialization.

```java
	public CreateCustomerForm()
	{
		super();
	}
```

---

</SwmSnippet>

# Field validation constraints

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="9">

---

Each field in the class has validation constraints to ensure data integrity. For example, the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="15:5:5" line-data="	private String custName;">`custName`</SwmToken> field must not be null and must have a maximum length of 61 characters.

```java
public class CreateCustomerForm
{


	@NotNull
	@Size(max = 61)
	private String custName;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="17">

---

Similarly, the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="19:5:5" line-data="	private String custAddress;">`custAddress`</SwmToken> field must not be null and must have a maximum length of 161 characters.

```java
	@NotNull
	@Size(max = 161)
	private String custAddress;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="21">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="23:5:5" line-data="	private String custDob;">`custDob`</SwmToken> field must not be null and must have exactly 8 characters.

```java
	@NotNull
	@Size(min = 8, max = 8)
	private String custDob;
```

---

</SwmSnippet>

# Getter and setter methods

The class provides getter and setter methods for each field. These methods ensure encapsulation and allow controlled access to the fields.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="32">

---

For example, the getter and setter methods for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="34:3:3" line-data="		return custName;">`custName`</SwmToken>:

```java
	public String getCustName()
	{
		return custName;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="38">

---

&nbsp;

```java
	public void setCustName(@NotNull String custName)
	{
		this.custName = custName.equals("") ? "" : custName;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="44">

---

The getter and setter methods for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="46:3:3" line-data="		return custAddress;">`custAddress`</SwmToken>:

```java
	public String getCustAddress()
	{
		return custAddress;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="50">

---

&nbsp;

```java
	public void setCustAddress(@NotNull String custAddress)
	{
		this.custAddress = custAddress.equals("") ? "" : custAddress;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="56">

---

The getter and setter methods for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="58:3:3" line-data="		return custDob;">`custDob`</SwmToken>:

```java
	public String getCustDob()
	{
		return custDob;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="62">

---

&nbsp;

```java
	public void setCustDob(String custDob)
	{
		this.custDob = "";
		this.custDob += custDob.substring(8, 10) + custDob.substring(5, 7)
				+ custDob.substring(0, 4);
	}
```

---

</SwmSnippet>

# <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="71:5:5" line-data="	public String toString()">`toString`</SwmToken> method

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="70">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="71:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="73:4:4" line-data="		return &quot;CreateCustomerForm [custAddress=&quot; + custAddress + &quot;, custDob=&quot;">`CreateCustomerForm`</SwmToken> object. This is useful for debugging and logging purposes.

```java
	@Override
	public String toString()
	{
		return "CreateCustomerForm [custAddress=" + custAddress + ", custDob="
				+ custDob + ", custName=" + custName + "]";
	}
```

---

</SwmSnippet>

# <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="78:5:5" line-data="	public boolean isValidTitle()">`isValidTitle`</SwmToken> method

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="78">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="78:5:5" line-data="	public boolean isValidTitle()">`isValidTitle`</SwmToken> method checks if the customer's name starts with a valid title. This method ensures that the name follows a specific format.

```java
	public boolean isValidTitle()
	{
		if (this.custName == null)
			return false;
		String[] elements = custName.split(" ");
		if (elements == null || elements.length < 3)
			return false;
		if (elements[0].contentEquals("Mr"))
			return true;
		if (elements[0].contentEquals("Mrs"))
			return true;
		if (elements[0].contentEquals("Miss"))
			return true;
		if (elements[0].contentEquals("Ms"))
			return true;
		if (elements[0].contentEquals("Dr"))
			return true;
		if (elements[0].contentEquals("Professor"))
			return true;
		if (elements[0].contentEquals("Drs"))
			return true;
		if (elements[0].contentEquals("Lord"))
			return true;
		if (elements[0].contentEquals("Sir"))
			return true;
		return (elements[0].contentEquals("Lady"));
	}

}
```

---

</SwmSnippet>

This document has covered the main design decisions and implementation details of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="9:4:4" line-data="public class CreateCustomerForm">`CreateCustomerForm`</SwmToken> class.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
