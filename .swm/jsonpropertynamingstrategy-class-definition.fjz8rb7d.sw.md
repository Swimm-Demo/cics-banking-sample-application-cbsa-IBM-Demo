---
title: JsonPropertyNamingStrategy Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="11:4:4" line-data="public class JsonPropertyNamingStrategy extends PropertyNamingStrategy">`JsonPropertyNamingStrategy`</SwmToken> class definition.

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="11:4:4" line-data="public class JsonPropertyNamingStrategy extends PropertyNamingStrategy">`JsonPropertyNamingStrategy`</SwmToken> class customizes the naming strategy for JSON properties in the `Z-OS-Connect-Payment-Interface` project.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="11:4:4" line-data="public class JsonPropertyNamingStrategy extends PropertyNamingStrategy">`JsonPropertyNamingStrategy`</SwmToken> class.
2. How the class customizes field names.
3. How the class customizes getter and setter method names.

# Class definition

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" line="11">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="11:4:4" line-data="public class JsonPropertyNamingStrategy extends PropertyNamingStrategy">`JsonPropertyNamingStrategy`</SwmToken> class extends <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="11:8:8" line-data="public class JsonPropertyNamingStrategy extends PropertyNamingStrategy">`PropertyNamingStrategy`</SwmToken> to provide custom naming for JSON properties.

```java
public class JsonPropertyNamingStrategy extends PropertyNamingStrategy
{



	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
```

---

</SwmSnippet>

# Customizing field names

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" line="22">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="23:5:5" line-data="	public String nameForField(MapperConfig&lt;?&gt; config, AnnotatedField field,">`nameForField`</SwmToken> method is overridden to customize the naming of fields. It uses the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="26:3:3" line-data="		return convert(field.getName());">`convert`</SwmToken> method to transform the field name.

```java
	@Override
	public String nameForField(MapperConfig<?> config, AnnotatedField field,
			String defaultName)
	{
		return convert(field.getName());
	}
```

---

</SwmSnippet>

# Customizing getter method names

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" line="30">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="31:5:5" line-data="	public String nameForGetterMethod(MapperConfig&lt;?&gt; config,">`nameForGetterMethod`</SwmToken> method is overridden to customize the naming of getter methods. It also uses the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="34:3:3" line-data="		return convert(method.getName());">`convert`</SwmToken> method to transform the method name.

```java
	@Override
	public String nameForGetterMethod(MapperConfig<?> config,
			AnnotatedMethod method, String defaultName)
	{
		return convert(method.getName());
	}
```

---

</SwmSnippet>

# Customizing setter method names

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" line="38">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="39:5:5" line-data="	public String nameForSetterMethod(MapperConfig&lt;?&gt; config,">`nameForSetterMethod`</SwmToken> method is overridden to customize the naming of setter methods. Like the other methods, it uses the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="42:3:3" line-data="		return convert(method.getName());">`convert`</SwmToken> method to transform the method name.

```java
	@Override
	public String nameForSetterMethod(MapperConfig<?> config,
			AnnotatedMethod method, String defaultName)
	{
		return convert(method.getName());
	}
```

---

</SwmSnippet>

# Conversion logic

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" line="46">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="46:5:5" line-data="	private String convert(String input)">`convert`</SwmToken> method is a private method that performs the actual transformation of the names. It removes the first three characters from the input string.

```java
	private String convert(String input)
	{
		return input.substring(3);
	}
}
```

---

</SwmSnippet>

This approach ensures that all field, getter, and setter method names are consistently transformed according to the custom logic defined in the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/JsonPropertyNamingStrategy.java" pos="26:3:3" line-data="		return convert(field.getName());">`convert`</SwmToken> method.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
