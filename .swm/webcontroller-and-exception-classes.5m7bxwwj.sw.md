---
title: WebController and Exception Classes
---
# Introduction

This document will walk you through the implementation of the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="34:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> and associated exception classes in the payment interface module.

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="34:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> handles the payment form submission and processes the payment request. It also manages the response and error handling.

We will cover:

1. The main controller setup and form handling.
2. The process of serializing the form data and sending the payment request.
3. The handling of the response and error management.
4. The custom exceptions used for specific error cases.

# Controller setup and form handling

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="33">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="34:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> is annotated with <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="33:0:1" line-data="@Controller">`@Controller`</SwmToken> and implements <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="34:8:8" line-data="public class WebController implements WebMvcConfigurer">`WebMvcConfigurer`</SwmToken>. This sets up the controller to handle web requests.

```java
@Controller
public class WebController implements WebMvcConfigurer
{


	private static final Logger log = LoggerFactory
			.getLogger(WebController.class);
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="41">

---

We define constants for form names and error messages to be used throughout the controller.

```java
	private static final String FORM_NAME = "paymentInterfaceForm";

	private static final String LARGE_TEXT = "largeText";

	private static final String SMALL_TEXT = "smallText";

	private static final String PAYMENT_ERROR = "Payment Error";
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="50">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="52:5:5" line-data="	public String showForm(TransferForm personForm)">`showForm`</SwmToken> method handles GET requests to display the payment form.

```java
	// Payment Interface
	@GetMapping(value={"/",""})
	public String showForm(TransferForm personForm)
	{
		return FORM_NAME;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="58">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="59:5:5" line-data="	public String checkPersonInfo(@Valid TransferForm transferForm,">`checkPersonInfo`</SwmToken> method handles POST requests for form submission. It validates the form data and processes the payment.

```java
	@PostMapping("/paydbcr")
	public String checkPersonInfo(@Valid TransferForm transferForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="64">

---

If there are validation errors, the same form is returned with error messages.

```java
		// The same page is returned, this time with the errors given as an
		// object.
		if (bindingResult.hasErrors())
		{
			return FORM_NAME;
		}
```

---

</SwmSnippet>

# Serializing form data and sending the payment request

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="71">

---

We create a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="71:1:1" line-data="		PaymentInterfaceJson transferjson = new PaymentInterfaceJson(">`PaymentInterfaceJson`</SwmToken> object from the form data and serialize it to JSON.

```java
		PaymentInterfaceJson transferjson = new PaymentInterfaceJson(
				transferForm);

		// Serialise the object to JSON
		log.info("{}", transferjson);
		String jsonString = new ObjectMapper().writeValueAsString(transferjson);
		log.info(jsonString);
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="79">

---

A <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="80:1:1" line-data="		WebClient client = WebClient.create(">`WebClient`</SwmToken> is used to send the payment request to the server. The server address and port are dynamically set.

```java
		// The port is set elsewhere as it changes frequently
		WebClient client = WebClient.create(
				ConnectionInfo.getAddressAndPort() + "/makepayment/dbcr");
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="83">

---

The request is sent using a PUT method, and the response is logged.

```java
		try
		{
			// Create a response object - body of json, accept json back, and
			// insert the
			// request body created a couple lines up
			ResponseSpec response = client.put()
					.header("content-type", "application/json")
					.accept(MediaType.APPLICATION_JSON)
					.body(BodyInserters.fromValue(jsonString)).retrieve();
			String responseBody = response.bodyToMono(String.class).block();
			log.info(responseBody);
```

---

</SwmSnippet>

# Handling the response and error management

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="95">

---

The response is deserialized into a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="96:1:1" line-data="			PaymentInterfaceJson responseObj = new ObjectMapper()">`PaymentInterfaceJson`</SwmToken> object.

```java
			// Deserialise into a POJO
			PaymentInterfaceJson responseObj = new ObjectMapper()
					.readValue(responseBody, PaymentInterfaceJson.class);
			log.info("{}", responseObj);
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="100">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="101:1:1" line-data="			checkIfResponseValidDbcr(responseObj);">`checkIfResponseValidDbcr`</SwmToken> method is called to validate the response. It throws specific exceptions based on the response code.

```java
			// Throws out different exceptions depending on the contents
			checkIfResponseValidDbcr(responseObj);

			// If successful...
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="147">

---

&nbsp;

```java
	public static void checkIfResponseValidDbcr(PaymentInterfaceJson response)
			throws InsufficientFundsException, InvalidAccountTypeException, AccountNotFoundException
	{
		switch (Integer.parseInt(response.getPAYDBCR().getCommFailCode()))
		{
		case 1:
			throw new AccountNotFoundException();
		case 3:
			throw new InsufficientFundsException();
		case 4:
			throw new InvalidAccountTypeException();
		default:
			break;
		}
	}
}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="105">

---

If the payment is successful, appropriate messages are added to the model.

```java
			if (transferForm.isDebit())
			{
				model.addAttribute(LARGE_TEXT, "Payment Successful");
			}
			else
			{
				model.addAttribute(LARGE_TEXT, "Credit Successful");
			}
			model.addAttribute(SMALL_TEXT,
					("Value: " + responseObj.getPAYDBCR().getCommAmt()));
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="116">

---

If an exception is thrown, it is caught and handled by logging the error and setting error messages in the model.

```java
			// Otherwise...
		}
		catch (AccountNotFoundException | InsufficientFundsException | InvalidAccountTypeException e)
		{
			log.info(e.toString());
			model.addAttribute(LARGE_TEXT, PAYMENT_ERROR);
			model.addAttribute(SMALL_TEXT, e.getMessage());
		}
		catch (WebClientRequestException e)
		{
			log.info(e.toString());
			model.addAttribute(LARGE_TEXT, PAYMENT_ERROR);
			model.addAttribute(SMALL_TEXT,
					"Connection refused or failed to resolve; Are you using the right address and port? Is the server running?");
		}
		catch (Exception e)
		{
			log.info(e.toString());
			model.addAttribute(LARGE_TEXT, PAYMENT_ERROR);
			model.addAttribute(SMALL_TEXT,
					"There was an error processing the request; Please try again later or check logs for more info.");
		}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="139">

---

The results are added to the model to be displayed in the HTML template.

```java
		// The HTML template includes a clause to show the box for the results
		// if this is set to true(the page is otherwise the same)
		model.addAttribute("results", true);

		return FORM_NAME;
	}
```

---

</SwmSnippet>

# Custom exceptions

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="164">

---

Several custom exceptions are defined to handle specific error cases. These exceptions extend <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="164:6:6" line-data="class InsufficientFundsException extends Exception">`Exception`</SwmToken> and provide meaningful error messages.

```java
class InsufficientFundsException extends Exception
{



	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="175">

---

&nbsp;

```java
	public InsufficientFundsException()
	{
		super("Payment rejected: Insufficient funds.");
	}
}

class InvalidAccountTypeException extends Exception
{



	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="192">

---

&nbsp;

```java
	public InvalidAccountTypeException()
	{
		super("Payment rejected: Invalid account type.");
	}
}

class AccountNotFoundException extends Exception
{



	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="209">

---

&nbsp;

```java
	public AccountNotFoundException()
	{
		super("Payment rejected: Account does not exist.");
	}
}

class TooManyAccountsException extends Exception
{



	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="226">

---

&nbsp;

```java
	public TooManyAccountsException(int customerNumber)
	{
		super("Too many accounts for customer number " + customerNumber
				+ "; Try deleting an account first.");
	}
}

class ItemNotFoundException extends Exception
{


	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="243">

---

&nbsp;

```java
	public ItemNotFoundException(String item)
	{
		super("The " + item
				+ " you searched for could not be found; Try a different "
				+ item + " number.");
	}
}
```

---

</SwmSnippet>

This setup ensures that the controller can handle various error scenarios and provide clear feedback to the user.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
