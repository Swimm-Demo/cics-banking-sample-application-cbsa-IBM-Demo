---
title: OutputFormatUtils Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" pos="12:3:3" line-data="	private OutputFormatUtils()">`OutputFormatUtils`</SwmToken> class definition in the <SwmPath>[src/…/paymentinterface/OutputFormatUtils.java](src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" pos="12:3:3" line-data="	private OutputFormatUtils()">`OutputFormatUtils`</SwmToken> class provides utility methods for formatting output, specifically dates and strings with leading zeroes.

We will cover:

1. The purpose of making the class static-only.
2. The design decision behind the date formatting methods.
3. The rationale for having multiple <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" pos="40:7:7" line-data="	public static String leadingZeroes(int amount, String input)">`leadingZeroes`</SwmToken> methods.

# Static-only class

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" line="12">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" pos="12:3:3" line-data="	private OutputFormatUtils()">`OutputFormatUtils`</SwmToken> class is designed to be a utility class, meaning it should not be instantiated. To enforce this, the constructor is private and throws an exception if called.

```java
	private OutputFormatUtils()
	{
		throw new IllegalStateException("Static only");
	}
```

---

</SwmSnippet>

# Date formatting methods

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" line="18">

---

The class provides two methods for formatting dates. The first method takes a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" pos="18:5:5" line-data="	public static String date(String date)">`String`</SwmToken> input and formats it into a date string with slashes.

```java
	public static String date(String date)
	{

		String unSlashedString = String.format("%8s", date).replace(" ", "0");
		return unSlashedString.substring(0, 2) + "/"
				+ unSlashedString.substring(2, 4) + "/"
				+ unSlashedString.substring(4, 8);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" line="28">

---

The second method is an overload that takes an `int` input, converts it to a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" pos="28:5:5" line-data="	public static String date(int date)">`String`</SwmToken>, and then formats it using the first method.

```java
	public static String date(int date)
	{
		return date(String.valueOf(date));
	}
```

---

</SwmSnippet>

# Leading zeroes methods

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" line="34">

---

The class also includes methods for adding leading zeroes to strings. The first method takes an `int` and a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" pos="34:7:7" line-data="	// Although using String.format directly might work sometimes, you still">`String`</SwmToken> as inputs and formats the string to the specified length with leading zeroes.

```java
	// Although using String.format directly might work sometimes, you still
	// need different formatting depending
	// on whether the input is a string or an int - this makes it slightly
	// quicker for account and customer numbers,
	// because the type returned can vary. Not as necessary for monetary values
	// since they're always a float or an int.
	public static String leadingZeroes(int amount, String input)
	{
		String formatString = "%" + amount + "s";
		return String.format(formatString, input).replace(" ", "0");
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" line="47">

---

The second method is an overload that takes two `int` inputs, converts the second input to a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/OutputFormatUtils.java" pos="47:5:5" line-data="	public static String leadingZeroes(int amount, int input)">`String`</SwmToken>, and then formats it using the first method.

```java
	public static String leadingZeroes(int amount, int input)
	{
		return leadingZeroes(amount, String.valueOf(input));
	}
}
```

---

</SwmSnippet>

These methods are useful for ensuring consistent formatting of account and customer numbers, which can vary in type.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
