---
title: UpdaccJson Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" pos="12:4:4" line-data="public class UpdaccJson">`UpdaccJson`</SwmToken> class definition in the <SwmPath>[src/…/updateaccount/UpdaccJson.java](src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" pos="12:4:4" line-data="public class UpdaccJson">`UpdaccJson`</SwmToken> class is designed to handle JSON serialization and deserialization for updating account information. It uses Jackson annotations to map JSON properties to Java fields.

We will cover:

1. Class-level annotations and their purpose.
2. Field definitions and their JSON mappings.
3. Constructor logic for initializing the class.
4. Key getter and setter methods for accessing and modifying fields.
5. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" pos="81:3:3" line-data="					commAccountTypeIn.toString());">`toString`</SwmToken> method for debugging and logging.

# Class-level annotations

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="11">

---

The class uses the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" pos="11:0:1" line-data="@JsonNaming(JsonPropertyNamingStrategy.class)">`@JsonNaming`</SwmToken> annotation to specify a custom naming strategy for JSON properties. This ensures that the JSON keys match the expected format.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class UpdaccJson
{



	@JsonProperty("COMM_EYE")
	private String commEye;
```

---

</SwmSnippet>

# Field definitions

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="20">

---

The class defines several fields, each annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" pos="20:1:2" line-data="	@JsonProperty(&quot;COMM_CUSTNO&quot;)">`@JsonProperty`</SwmToken> to map JSON keys to Java fields. This is crucial for correctly parsing and generating JSON data.

```java
	@JsonProperty("COMM_CUSTNO")
	private String commCustno;

	@JsonProperty("COMM_SCODE")
	private String commSortcode;

	@JsonProperty("COMM_ACCNO")
	private int commAccno;

	@JsonProperty("COMM_INT_RATE")
	private float commInterestRate;

	@JsonProperty("COMM_OPENED")
	private String commOpened;

	@JsonProperty("COMM_OVERDRAFT")
	private int commOverdraft;

	@JsonProperty("COMM_LAST_STMT_DT")
	private String commLastStatementDate;

	@JsonProperty("COMM_NEXT_STMT_DT")
	private String commNextStatementDate;

	@JsonProperty("COMM_AVAIL_BAL")
	private float commAvailableBalance;

	@JsonProperty("COMM_ACTUAL_BAL")
	private float commActualBalance;

	@JsonProperty("COMM_SUCCESS")
	private String commSuccess;

	private static final String SPACES = "        ";

	@JsonProperty("COMM_ACC_TYPE")
	private String commAccountType = SPACES;
```

---

</SwmSnippet>

# Constructor logic

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="59">

---

The class provides a parameterized constructor to initialize all fields. This constructor ensures that all necessary data is provided when creating an instance of <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" pos="59:3:3" line-data="	public UpdaccJson(String commCustNoIn, int commAccNoIn,">`UpdaccJson`</SwmToken>.

```java
	public UpdaccJson(String commCustNoIn, int commAccNoIn,
			AccountType commAccountTypeIn, float commInterestRateIn,
			String commOpenedIn, int commOverdraftIn,
			String commLastStatementDateIn, String commNextStatementDateIn,
			float commAvailableBalanceIn, float commActualBalanceIn)
	{
		commCustno = commCustNoIn;
		commAccno = commAccNoIn;
		commInterestRate = commInterestRateIn;
		commOpened = commOpenedIn;
		commOverdraft = commOverdraftIn;
		commLastStatementDate = commLastStatementDateIn;
		commNextStatementDate = commNextStatementDateIn;
		commAvailableBalance = commAvailableBalanceIn;
		commActualBalance = commActualBalanceIn;
		if (commAccountTypeIn == null)
		{
			commAccountType = SPACES;
		}
		else
		{
			commAccountType = String.format("%-8s",
					commAccountTypeIn.toString());
		}
	}
```

---

</SwmSnippet>

# Getter and setter methods

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="92">

---

The class includes getter and setter methods for each field. These methods allow other parts of the application to access and modify the fields as needed.

```java
	public String getCommEye()
	{
		return commEye;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="104">

---

&nbsp;

```java
	public String getCommCustNo()
	{
		return commCustno;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="116">

---

&nbsp;

```java
	public String getCommSortcode()
	{
		return commSortcode;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="128">

---

&nbsp;

```java
	public int getCommAccno()
	{
		return commAccno;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="140">

---

&nbsp;

```java
	public String getCommAccountType()
	{
		return commAccountType;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="165">

---

&nbsp;

```java
	public float getCommInterestRate()
	{
		return commInterestRate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="177">

---

&nbsp;

```java
	public String getCommOpened()
	{
		return commOpened;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="189">

---

&nbsp;

```java
	public int getCommOverdraft()
	{
		return commOverdraft;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="201">

---

&nbsp;

```java
	public String getCommLastStatementDate()
	{
		return commLastStatementDate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="213">

---

&nbsp;

```java
	public String getCommNextStatementDate()
	{
		return commNextStatementDate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="225">

---

&nbsp;

```java
	public float getCommAvailableBalance()
	{
		return commAvailableBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="237">

---

&nbsp;

```java
	public float getCommActualBalance()
	{
		return commActualBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="249">

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

# <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" pos="81:3:3" line-data="					commAccountTypeIn.toString());">`toString`</SwmToken> method

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" line="261">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" pos="262:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/updateaccount/UpdaccJson.java" pos="264:4:4" line-data="		return &quot;UpdaccJson [CommAccno=&quot; + commAccno + &quot;, CommAccType=&quot;">`UpdaccJson`</SwmToken> object. This is useful for debugging and logging purposes.

```java
	@Override
	public String toString()
	{
		return "UpdaccJson [CommAccno=" + commAccno + ", CommAccType="
				+ commAccountType + ", CommActualBal=" + commActualBalance
				+ ", CommAvailBal=" + commAvailableBalance + ", CommCustno="
				+ commCustno + ", CommEye=" + commEye + ", CommIntRate="
				+ commInterestRate + ", CommLastStmtDt="
				+ commLastStatementDate + ", CommNextStmtDt="
				+ commNextStatementDate + ", CommOpened=" + commOpened
				+ ", CommOverdraft=" + commOverdraft + ", CommScode="
				+ commSortcode + ", CommSuccess=" + commSuccess + "]";
	}

}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
