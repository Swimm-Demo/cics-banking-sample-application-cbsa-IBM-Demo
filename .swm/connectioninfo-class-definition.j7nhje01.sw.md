---
title: ConnectionInfo Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" pos="26:3:3" line-data="	private ConnectionInfo()">`ConnectionInfo`</SwmToken> class definition in the <SwmPath>[src/…/customerservices/ConnectionInfo.java](src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" pos="26:3:3" line-data="	private ConnectionInfo()">`ConnectionInfo`</SwmToken> class is designed to manage connection parameters for a service. It uses static fields and methods to store and retrieve connection details such as scheme, port, and address.

We will cover:

1. Why the class uses static fields and methods.
2. How the class enforces its static-only nature.
3. The purpose of the parameter annotations.
4. Key methods for getting and setting connection details.

# Static fields and methods

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" line="13">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" pos="26:3:3" line-data="	private ConnectionInfo()">`ConnectionInfo`</SwmToken> class uses static fields and methods to ensure that connection parameters are globally accessible and modifiable without needing to instantiate the class. This design choice simplifies access to connection details across the application.

```java
	@Parameter(names =
	{ "--scheme", "-s" }, description = "Scheme/protocol to connect with")
	private static String scheme = "http";

	@Parameter(names =
	{ "--port", "-p" }, description = "Port to connect with")
	private static int port = 38417;

	@Parameter(names =
	{ "--address", "--url", "-a", "-u" }, description = "Address to use")
	private static String address = "localhost";
```

---

</SwmSnippet>

# Enforcing static-only nature

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" line="26">

---

To enforce that the class is not instantiated, a private constructor is defined that throws an <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" pos="28:5:5" line-data="		throw new IllegalStateException(&quot;Static only&quot;);">`IllegalStateException`</SwmToken>. This ensures that all interaction with the class is through its static methods and fields.

```java
	private ConnectionInfo()
	{
		throw new IllegalStateException("Static only");
	}
```

---

</SwmSnippet>

# Parameter annotations

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" line="13">

---

The class uses the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" pos="13:1:2" line-data="	@Parameter(names =">`@Parameter`</SwmToken> annotation from the JCommander library to define command-line parameters for the connection details. This allows the application to be configured via command-line arguments.

```java
	@Parameter(names =
	{ "--scheme", "-s" }, description = "Scheme/protocol to connect with")
	private static String scheme = "http";

	@Parameter(names =
	{ "--port", "-p" }, description = "Port to connect with")
	private static int port = 38417;

	@Parameter(names =
	{ "--address", "--url", "-a", "-u" }, description = "Address to use")
	private static String address = "localhost";
```

---

</SwmSnippet>

# Key methods for connection details

The class provides several static methods to get and set the connection details. These methods ensure that the connection parameters can be easily accessed and modified.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" line="32">

---

To get the full address and port as a single string:

```java
	public static String getAddressAndPort()
	{
		return scheme + "://" + address + ":" + port;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" line="50">

---

To set the port:

```java
	public static void setPort(int port)
	{
		ConnectionInfo.port = port;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" line="56">

---

To get the address:

```java
	public static String getAddress()
	{
		return address;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" line="62">

---

To set the address:

```java
	public static void setAddress(String address)
	{
		ConnectionInfo.address = address;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" line="67">

---

To get the scheme:

```java
	public static String getScheme()
	{
		return scheme;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/ConnectionInfo.java" line="73">

---

To set the scheme:

```java
	public static void setScheme(String scheme)
	{
		ConnectionInfo.scheme = scheme;
	}

}
```

---

</SwmSnippet>

This structure ensures that connection details are managed in a centralized and consistent manner.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
