---
title: Connection Information Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" pos="24:3:3" line-data="	private ConnectionInfo()">`ConnectionInfo`</SwmToken> class definition in the <SwmPath>[src/…/paymentinterface/ConnectionInfo.java](src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" pos="24:3:3" line-data="	private ConnectionInfo()">`ConnectionInfo`</SwmToken> class is designed to manage connection parameters for a payment interface. It uses static fields and methods to store and retrieve connection details such as scheme, port, and address.

We will cover:

1. Why the class is designed to be static-only.
2. How connection parameters are defined and retrieved.
3. How connection parameters can be modified.

# Static-only class design

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" line="24">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" pos="24:3:3" line-data="	private ConnectionInfo()">`ConnectionInfo`</SwmToken> class is designed to be static-only. This is enforced by making the constructor private and throwing an exception if it is called. This ensures that the class cannot be instantiated and is used only for its static members.

```java
	private ConnectionInfo()
	{
		throw new IllegalStateException("Static only");
	}
```

---

</SwmSnippet>

# Defining connection parameters

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" line="11">

---

Connection parameters such as scheme, port, and address are defined as static fields with default values. These fields are annotated with <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" pos="11:0:1" line-data="@Parameter(names =">`@Parameter`</SwmToken> to allow them to be set via command-line arguments.

```java
@Parameter(names =
	{ "--scheme", "-s" }, description = "Scheme/protocol to connect with")
	private static String scheme = "http";

	@Parameter(names =
	{ "--port", "-p" }, description = "Port to connect with")
	private static int port = 38417;

	@Parameter(names =
	{ "--address", "--url", "-a", "-u" }, description = "Address to use")
	private static String address = "127.0.0.1";
```

---

</SwmSnippet>

# Retrieving connection parameters

The class provides static methods to retrieve the connection parameters. These methods return the current values of the parameters.

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" line="30">

---

To get the full address including the scheme and port:

```java
	public static String getAddressAndPort()
	{
		return scheme + "://" + address + ":" + port;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" line="36">

---

To get the port as an integer:

```java
	public static int getPort()
	{
		return port;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" line="54">

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

# Modifying connection parameters

The class also provides static methods to modify the connection parameters. These methods allow the parameters to be updated at runtime.

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" line="48">

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

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" line="60">

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

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/ConnectionInfo.java" line="71">

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

This design ensures that connection parameters are centrally managed and easily accessible throughout the application.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
