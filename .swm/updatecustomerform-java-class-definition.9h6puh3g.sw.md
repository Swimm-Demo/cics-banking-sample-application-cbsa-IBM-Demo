---
title: UpdateCustomerForm Java Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="9:4:4" line-data="public class UpdateCustomerForm">`UpdateCustomerForm`</SwmToken> Java class definition.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="9:4:4" line-data="public class UpdateCustomerForm">`UpdateCustomerForm`</SwmToken> class is designed to handle customer data updates. It includes fields for customer details and provides getter and setter methods for these fields. The class also includes validation constraints to ensure data integrity.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="9:4:4" line-data="public class UpdateCustomerForm">`UpdateCustomerForm`</SwmToken> class.
2. The validation constraints applied to the fields.
3. The implementation of getter and setter methods.
4. The custom logic in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="73:5:5" line-data="	public void setCustDoB(String custDoB)">`setCustDoB`</SwmToken> method.
5. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="110:5:5" line-data="	public String toString()">`toString`</SwmToken> method for debugging and logging.

# Class definition and fields

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="9">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="9:4:4" line-data="public class UpdateCustomerForm">`UpdateCustomerForm`</SwmToken> class is defined to hold customer data that needs to be updated. It includes fields for customer number, name, address, date of birth, credit score, and review date.

```java
public class UpdateCustomerForm
{



	@NotNull
	@Size(max = 10)
	String custNumber;
```

---

</SwmSnippet>

# Validation constraints

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="9">

---

Validation constraints are applied to ensure that the data meets specific criteria. For example, the customer number must not be null and must have a maximum size of 10 characters.

```java
public class UpdateCustomerForm
{



	@NotNull
	@Size(max = 10)
	String custNumber;
```

---

</SwmSnippet>

# Getter and setter methods

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="28">

---

The class provides getter and setter methods for each field. These methods are essential for accessing and modifying the field values.

```java
	String custReviewDate = "";


	public String getCustNumber()
	{
		return custNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="37">

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

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="43">

---

&nbsp;

```java
	public String getCustName()
	{
		return custName;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="49">

---

&nbsp;

```java
	public void setCustName(String custName)
	{
		this.custName = custName;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="55">

---

&nbsp;

```java
	public String getCustAddress()
	{
		return custAddress;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="61">

---

&nbsp;

```java
	public void setCustAddress(String custAddress)
	{
		this.custAddress = custAddress;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="67">

---

&nbsp;

```java
	public String getCustDoB()
	{
		return custDoB;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="73">

---

&nbsp;

```java
	public void setCustDoB(String custDoB)
	{
		if (custDoB.equals(""))
		{
			return;
		}
		this.custDoB = "";
		this.custDoB += custDoB.substring(8, 10) + custDoB.substring(5, 7)
				+ custDoB.substring(0, 4);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="85">

---

&nbsp;

```java
	public int getCustCreditScore()
	{
		return custCreditScore;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="91">

---

&nbsp;

```java
	public void setCustCreditScore(int custCreditScore)
	{
		this.custCreditScore = custCreditScore;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="97">

---

&nbsp;

```java
	public String getCustReviewDate()
	{
		return custReviewDate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="103">

---

&nbsp;

```java
	public void setCustReviewDate(String custReviewDate)
	{
		this.custReviewDate = custReviewDate;
	}
```

---

</SwmSnippet>

# Custom logic in <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="73:5:5" line-data="	public void setCustDoB(String custDoB)">`setCustDoB`</SwmToken> method

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="73">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="73:5:5" line-data="	public void setCustDoB(String custDoB)">`setCustDoB`</SwmToken> method includes custom logic to format the date of birth. If the input date is not empty, it rearranges the date format to `DDMMYYYY`.

```java
	public void setCustDoB(String custDoB)
	{
		if (custDoB.equals(""))
		{
			return;
		}
		this.custDoB = "";
		this.custDoB += custDoB.substring(8, 10) + custDoB.substring(5, 7)
				+ custDoB.substring(0, 4);
	}
```

---

</SwmSnippet>

# <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="110:5:5" line-data="	public String toString()">`toString`</SwmToken> method

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" line="109">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="110:5:5" line-data="	public String toString()">`toString`</SwmToken> method is overridden to provide a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="112:4:4" line-data="		return &quot;UpdateCustomerForm [custAddress=&quot; + custAddress">`UpdateCustomerForm`</SwmToken> object. This is useful for debugging and logging purposes.

```java
	@Override
	public String toString()
	{
		return "UpdateCustomerForm [custAddress=" + custAddress
				+ ", custCreditScore=" + custCreditScore + ", custDoB="
				+ custDoB + ", custName=" + custName + ", custNumber="
				+ custNumber + ", custReviewDate=" + custReviewDate + "]";
	}

}
```

---

</SwmSnippet>

This document has covered the main aspects of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="9:4:4" line-data="public class UpdateCustomerForm">`UpdateCustomerForm`</SwmToken> class, including its fields, validation constraints, getter and setter methods, custom logic in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="73:5:5" line-data="	public void setCustDoB(String custDoB)">`setCustDoB`</SwmToken> method, and the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updatecustomer/UpdateCustomerForm.java" pos="110:5:5" line-data="	public String toString()">`toString`</SwmToken> method.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
