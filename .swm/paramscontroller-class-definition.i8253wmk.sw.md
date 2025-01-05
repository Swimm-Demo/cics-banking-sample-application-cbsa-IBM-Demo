---
title: ParamsController Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="23:4:4" line-data="public class ParamsController">`ParamsController`</SwmToken> class definition in the <SwmPath>[src/…/controllers/ParamsController.java](src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java)</SwmPath> file.

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="23:4:4" line-data="public class ParamsController">`ParamsController`</SwmToken> class is designed to handle HTTP POST requests for submitting payment details. It processes the input parameters, converts them to JSON, and sends them to a specified endpoint.

We will cover:

1. The purpose of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="23:4:4" line-data="public class ParamsController">`ParamsController`</SwmToken> class.
2. How the input parameters are processed.
3. How the JSON conversion and HTTP request are handled.

# Class definition and logging

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" line="22">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="23:4:4" line-data="public class ParamsController">`ParamsController`</SwmToken> class is annotated with <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="22:0:1" line-data="@RestController">`@RestController`</SwmToken>, indicating that it is a Spring MVC controller where every method returns a domain object instead of a view.

```java
@RestController
public class ParamsController
{



	private static final Logger log = LoggerFactory
			.getLogger(ParamsController.class);
```

---

</SwmSnippet>

# Handling POST requests

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" line="32">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="35:6:6" line-data="	@PostMapping(&quot;/submit&quot;)">`submit`</SwmToken> method is mapped to the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="35:5:6" line-data="	@PostMapping(&quot;/submit&quot;)">`/submit`</SwmToken> endpoint and handles POST requests. It takes three required parameters: <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="37:9:9" line-data="			@RequestParam(name = &quot;acctnum&quot;, required = true) String acctNumber,">`acctnum`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="38:9:9" line-data="			@RequestParam(name = &quot;amount&quot;, required = true) float amount,">`amount`</SwmToken>, and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="39:9:9" line-data="			@RequestParam(name = &quot;organisation&quot;, required = true) String organisation)">`organisation`</SwmToken>.

```java
	// This follows a very similar format to the form submitting equivalents in
	// WebController.java
	// Instead of a form object, parameters required in the url
	@PostMapping("/submit")
	public PaymentInterfaceJson submit(
			@RequestParam(name = "acctnum", required = true) String acctNumber,
			@RequestParam(name = "amount", required = true) float amount,
			@RequestParam(name = "organisation", required = true) String organisation)
			throws JsonProcessingException
	{
		log.info("AcctNumber: {}, Amount {}, Organisation {}", acctNumber,
				amount, organisation);
		TransferForm transferForm = new TransferForm(acctNumber, amount,
				organisation);
```

---

</SwmSnippet>

# JSON conversion

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" line="47">

---

The input parameters are used to create a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="44:1:1" line-data="		TransferForm transferForm = new TransferForm(acctNumber, amount,">`TransferForm`</SwmToken> object, which is then converted to a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="47:1:1" line-data="		PaymentInterfaceJson transferJson = new PaymentInterfaceJson(">`PaymentInterfaceJson`</SwmToken> object. This object is serialized to a JSON string.

```java
		PaymentInterfaceJson transferJson = new PaymentInterfaceJson(
				transferForm);

		String jsonString = new ObjectMapper().writeValueAsString(transferJson);
		log.info(jsonString);
```

---

</SwmSnippet>

# Sending the HTTP request

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" line="53">

---

A <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="53:1:1" line-data="		WebClient client = WebClient.create(">`WebClient`</SwmToken> instance is created to send the JSON string to the specified endpoint. The response is logged and converted back to a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="55:1:1" line-data="		PaymentInterfaceJson responseObj;">`PaymentInterfaceJson`</SwmToken> object.

```java
		WebClient client = WebClient.create(
				ConnectionInfo.getAddressAndPort() + "/makepayment/dbcr");
		PaymentInterfaceJson responseObj;
```

---

</SwmSnippet>

# Handling the response

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" line="57">

---

The response from the HTTP request is processed, logged, and returned. If an exception occurs, it is logged, and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="75:3:3" line-data="		return null;">`null`</SwmToken> is returned.

```java
		try
		{
			ResponseSpec response = client.put()
					.header("content-type", "application/json")
					.accept(MediaType.APPLICATION_JSON)
					.body(BodyInserters.fromValue(jsonString)).retrieve();
			String responseBody = response.bodyToMono(String.class).block();
			log.info(responseBody);
			responseObj = new ObjectMapper().readValue(responseBody,
					PaymentInterfaceJson.class);
			log.info("{}", responseObj);
			return responseObj;
		}
		catch (Exception e)
		{
			log.info(e.toString());
		}

		return null;
	}
}
```

---

</SwmSnippet>

This structure ensures that the input parameters are correctly processed, converted to JSON, and sent to the endpoint, with appropriate logging at each step.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
