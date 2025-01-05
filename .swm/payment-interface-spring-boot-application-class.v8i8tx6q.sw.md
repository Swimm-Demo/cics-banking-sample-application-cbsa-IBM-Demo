---
title: Payment Interface Spring Boot Application Class
---
# Introduction

This document will walk you through the Payment Interface Spring Boot Application Class.

The Payment Interface class is the entry point for the Spring Boot application. It initializes the application and sets up the necessary configurations.

We will cover:

1. Why we use the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" pos="14:0:1" line-data="@SpringBootApplication(scanBasePackages =">`@SpringBootApplication`</SwmToken> annotation.
2. The purpose of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" pos="22:7:7" line-data="	public static void main(String[] args)">`main`</SwmToken> method.
3. The role of <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" pos="26:1:1" line-data="		JCommander.newBuilder().build().parse(args);">`JCommander`</SwmToken> and logging.
4. How the application is run.

# Spring Boot application setup

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" line="11">

---

We use the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" pos="14:0:1" line-data="@SpringBootApplication(scanBasePackages =">`@SpringBootApplication`</SwmToken> annotation to mark the main class of our Spring Boot application. This annotation enables auto-configuration and component scanning in the specified package.

```java
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication(scanBasePackages =
{ "com.ibm.cics.cip.bank.springboot.paymentinterface.controllers" })
public class PaymentInterface
{
```

---

</SwmSnippet>

# Main method

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" line="22">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" pos="22:7:7" line-data="	public static void main(String[] args)">`main`</SwmToken> method is the entry point of the application. It initializes the logger and sets up command-line argument parsing.

```java
	public static void main(String[] args)
	{
		final Logger log = LoggerFactory.getLogger(PaymentInterface.class);
```

---

</SwmSnippet>

# Command-line argument parsing and logging

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" line="26">

---

We use <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" pos="26:1:1" line-data="		JCommander.newBuilder().build().parse(args);">`JCommander`</SwmToken> to parse command-line arguments. This allows us to handle different configurations and options passed to the application. The logger is used to log the address and port information.

```java
		JCommander.newBuilder().build().parse(args);

		log.info("Running with address: {}",
				ConnectionInfo.getAddressAndPort());
```

---

</SwmSnippet>

# Running the application

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" line="31">

---

Finally, we run the Spring Boot application using `SpringApplication.run()`. This starts the embedded web server and initializes the application context.

```java
		// Run the application. From here out, only the WebController and
		// ParamsController classes really matter.
		SpringApplication.run(PaymentInterface.class, args);
	}

}
```

---

</SwmSnippet>

This structure ensures that the application is properly configured and ready to handle incoming requests.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
