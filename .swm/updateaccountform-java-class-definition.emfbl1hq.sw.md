---
title: UpdateAccountForm Java Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" pos="12:4:4" line-data="public class UpdateAccountForm">`UpdateAccountForm`</SwmToken> Java class definition.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" pos="12:4:4" line-data="public class UpdateAccountForm">`UpdateAccountForm`</SwmToken> class is designed to handle the data structure for updating account information in the customer services interface. It includes various fields with validation constraints and methods to get and set these fields.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" pos="12:4:4" line-data="public class UpdateAccountForm">`UpdateAccountForm`</SwmToken> class.
2. Key fields and their validation constraints.
3. Important methods for accessing and modifying the fields.

# Class definition

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="10">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" pos="12:4:4" line-data="public class UpdateAccountForm">`UpdateAccountForm`</SwmToken> class is defined to encapsulate the data required for updating an account. It includes fields for customer number, account number, account type, interest rate, overdraft, and balance details.

```java
import org.hibernate.validator.constraints.Range;

public class UpdateAccountForm
{



	private String custNumber;
```

---

</SwmSnippet>

# Account number validation

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="19">

---

The account number field is validated to ensure it is not null and falls within a specific range. This prevents invalid account numbers from being processed.

```java
	// Limiting length to 8
	@NotNull
	@Range(min = 1, max = 99999999)
	private int acctNumber;
```

---

</SwmSnippet>

# Account type and interest rate

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="24">

---

The account type field is validated to ensure it is not null and a valid account type is chosen. The interest rate field is also validated to ensure it is not null.

```java
	@NotNull(message = "You must choose an account type")
	private AccountType acctType;

	@NotNull
	private String acctInterestRate;
```

---

</SwmSnippet>

# Overdraft and dates

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="30">

---

The overdraft field is validated to ensure it is not null. The class also includes fields for account opened date, last statement date, and next statement date.

```java
	@NotNull
	private String acctOverdraft;

	private String acctOpenedDate;
```

---

</SwmSnippet>

# Getter and setter methods

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="44">

---

The class includes getter and setter methods for each field. These methods are essential for accessing and modifying the field values.

```java
	public String getCustNumber()
	{
		return custNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="50">

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

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="56">

---

&nbsp;

```java
	public int getAcctNumber()
	{
		return acctNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="62">

---

&nbsp;

```java
	public void setAcctNumber(int acctNumber)
	{
		this.acctNumber = acctNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="68">

---

&nbsp;

```java
	public AccountType getAcctType()
	{
		return acctType;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="74">

---

&nbsp;

```java
	public void setAcctType(AccountType acctType)
	{
		this.acctType = acctType;
	}
```

---

</SwmSnippet>

# Interest rate handling

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="80">

---

The class provides methods to get the interest rate as a string and as a float. It also includes a setter method that handles empty string values by setting a default interest rate.

```java
	// Interest Rate
	public String getAcctInterestRate()
	{
		return acctInterestRate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="87">

---

&nbsp;

```java
	public float getAcctInterestRateFloat()
	{
		return Float.parseFloat(acctInterestRate);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="93">

---

&nbsp;

```java
	// Counting "" as null here, as an actual value needs to be entered
	public void setAcctInterestRate(String acctInterestRate)
	{
		this.acctInterestRate = acctInterestRate.equals("") ? "0.00"
				: acctInterestRate;
	}
```

---

</SwmSnippet>

# Overdraft handling

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="101">

---

Similar to the interest rate, the class provides methods to get the overdraft as a string and as an integer. The setter method handles empty string values by setting a default overdraft value.

```java
	// Overdraft
	public String getAcctOverdraft()
	{
		return acctOverdraft;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="108">

---

&nbsp;

```java
	public int getAcctOverdraftInt()
	{
		return Integer.parseInt(acctOverdraft);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="114">

---

&nbsp;

```java
	// Counting "" as null here, as an actual value needs to be entered
	public void setAcctOverdraft(String acctOverdraft)
	{
		this.acctOverdraft = acctOverdraft.equals("") ? "0"
				: String.valueOf(Integer.parseInt(acctOverdraft));
	}
```

---

</SwmSnippet>

# Date handling

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="122">

---

The class includes methods to get and set the account opened date, last statement date, and next statement date. The setter methods handle empty string values and format the dates correctly.

```java
	public String getAcctOpenedDate()
	{
		return acctOpenedDate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="128">

---

&nbsp;

```java
	public void setAcctOpenedDate(String acctOpenedDate)
	{
		if (acctOpenedDate.equals(""))
		{
			return;
		}
		this.acctOpenedDate = "";
		this.acctOpenedDate += acctOpenedDate.substring(8, 10)
				+ acctOpenedDate.substring(5, 7)
				+ acctOpenedDate.substring(0, 4);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="141">

---

&nbsp;

```java
	public String getAcctLastStatementDate()
	{
		return acctLastStatementDate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="147">

---

&nbsp;

```java
	public void setAcctLastStatementDate(String acctLastStatementDate)
	{
		if (acctLastStatementDate.equals(""))
		{
			return;
		}
		this.acctLastStatementDate = "";
		this.acctLastStatementDate += acctLastStatementDate.substring(8, 10)
				+ acctLastStatementDate.substring(5, 7)
				+ acctLastStatementDate.substring(0, 4);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="160">

---

&nbsp;

```java
	public String getAcctNextStatementDate()
	{
		return acctNextStatementDate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="166">

---

&nbsp;

```java
	public void setAcctNextStatementDate(String acctNextStatementDate)
	{
		if (acctNextStatementDate.equals(""))
		{
			return;
		}
		this.acctNextStatementDate = "";
		this.acctNextStatementDate += acctNextStatementDate.substring(8, 10)
				+ acctNextStatementDate.substring(5, 7)
				+ acctNextStatementDate.substring(0, 4);
	}
```

---

</SwmSnippet>

# Balance handling

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="179">

---

The class includes methods to get and set the available balance and actual balance.

```java
	public float getAcctAvailableBalance()
	{
		return acctAvailableBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="185">

---

&nbsp;

```java
	public void setAcctAvailableBalance(float acctAvailableBalance)
	{
		this.acctAvailableBalance = acctAvailableBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="191">

---

&nbsp;

```java
	public float getAcctActualBalance()
	{
		return acctActualBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="197">

---

&nbsp;

```java
	public void setAcctActualBalance(float acctActualBalance)
	{
		this.acctActualBalance = acctActualBalance;
	}
```

---

</SwmSnippet>

# String representation

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" line="203">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" pos="204:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" pos="206:4:4" line-data="		return &quot;UpdateAccountForm [acctActualBalance=&quot; + acctActualBalance">`UpdateAccountForm`</SwmToken> object, which is useful for debugging and logging.

```java
	@Override
	public String toString()
	{
		return "UpdateAccountForm [acctActualBalance=" + acctActualBalance
				+ ", acctAvailableBalance=" + acctAvailableBalance
				+ ", acctInterestRate=" + acctInterestRate
				+ ", acctLastStatementDate=" + acctLastStatementDate
				+ ", acctNextStatementDate=" + acctNextStatementDate
				+ ", acctNumber=" + acctNumber + ", acctOpenedDate="
				+ acctOpenedDate + ", acctOverdraft=" + acctOverdraft
				+ ", acctType=" + acctType + ", custNumber=" + custNumber + "]";
	}

}
```

---

</SwmSnippet>

This document has covered the main ideas and important aspects of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdateAccountForm.java" pos="12:4:4" line-data="public class UpdateAccountForm">`UpdateAccountForm`</SwmToken> class, including its fields, validation constraints, and key methods.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
