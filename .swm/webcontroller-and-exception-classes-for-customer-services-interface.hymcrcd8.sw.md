---
title: WebController and Exception Classes for Customer Services Interface
---
# Introduction

This document will walk you through the implementation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="533:9:9" line-data="			model.addAttribute(LARGE_TEXT, &quot;Customer creation successful&quot;);">`Customer`</SwmToken>` Services Interface`. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> handles various customer and account-related operations such as enquiries, creation, updates, and deletions.

We will cover:

1. Why the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class was implemented in a repetitive manner.
2. How the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> handles account enquiries.
3. How the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> handles customer creation.
4. How the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> handles account updates.
5. How the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> handles customer deletions.

# Design decision: Repetitive implementation

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="49">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class contains repetitive code for handling different customer and account operations. This was done to avoid <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="49:39:41" line-data="// The code in this file is quite repetitive, however a case/swich block would&#39;ve required too much over-engineering to do">`over-engineering`</SwmToken> with complex case/switch blocks, as each operation returns slightly different formats and fields.

```java
// The code in this file is quite repetitive, however a case/swich block would've required too much over-engineering to do
// Ideally I'd only need to send off one class and I'd only get either an account or customer object back to deserialise,
// but all of the objects returned have slightly different formats and/or fields.

@Controller
public class WebController implements WebMvcConfigurer
{
```

---

</SwmSnippet>

# Handling account enquiries

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> provides a method to handle account enquiries. It first displays the account enquiry form and then processes the form submission to fetch account details.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="123">

---

To display the account enquiry form:

```java
	// Get request for when first navigating to the page
	@GetMapping("/enqacct")
	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)
	{
		// String relates to the page template found in
		// /src/main/resources/templates
		return ACCOUNT_ENQUIRY_FORM;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="133">

---

To process the form submission:

```java
	// When the Submit button is pressed, a Post request to the same location is
	// made
	// This function gets its arguments created using magic and the form
	// submitted
	@PostMapping("/enqacct")
	public String returnAcct(@Valid AccountEnquiryForm accountEnquiryForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="153">

---

The form submission processing involves creating a <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="153:7:7" line-data="			// Instantiating a WebClient at either the specified address or the">`WebClient`</SwmToken> to send a request to the backend service:

```java
			// Instantiating a WebClient at either the specified address or the
			// default one
			WebClient client = WebClient.create(ConnectionInfo.getAddressAndPort() + "/inqaccz/enquiry/"
							+ accountEnquiryForm.getAcctNumber());
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="176">

---

The response is then deserialized and checked for validity:

```java
				AccountEnquiryJson responseObj = myObjectMapper.readValue(responseBody, AccountEnquiryJson.class);
				log.info("{}", responseObj);

				// Run through the checks on error codes in the method shown
				// directly below this and every other response method
				// The method throws exceptions based on the error type
				checkIfResponseValidListAcc(responseObj);
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="184">

---

If the response is valid, the account details are added to the model:

```java
				// Set the fields that will be shown in the template. Either the
				// details of the response, or the details of the error.
				model.addAttribute(LARGE_TEXT, "Account Details:");
				model.addAttribute(SMALL_TEXT, responseObj.toPrettyString());
				model.addAttribute("success", true);
			}
```

---

</SwmSnippet>

# Handling customer creation

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> also handles the creation of new customers. It first displays the customer creation form and then processes the form submission to create a new customer.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="481">

---

To display the customer creation form:

```java
	// 5. Create a customer
	@GetMapping("/createcust")
	public String showCreateCustForm(CreateCustomerForm createCustForm,
			Model model)
	{
		return CREATE_CUSTOMER_FORM;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="490">

---

To process the form submission:

```java
	@PostMapping("/createcust")
	public String processCreateCust(@Valid CreateCustomerForm createCustForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
		if (bindingResult.hasErrors())
		{
			return CREATE_CUSTOMER_FORM;
		}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="500">

---

The form submission processing involves serializing the form data to JSON and sending it to the backend service:

```java
		CreateCustomerJson transferjson = new CreateCustomerJson(
				createCustForm);

		// Serialise the object to JSON
		log.info("{}", transferjson);
		String jsonString = new ObjectMapper().writeValueAsString(transferjson);
		log.info("Json to be sent:\n{}", jsonString);
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="524">

---

The response is then deserialized and checked for validity:

```java
			// Deserialise into a POJO
			CreateCustomerJson responseObj = new ObjectMapper()
					.readValue(responseBody, CreateCustomerJson.class);
			log.info("Response Json:\n{}", responseObj);
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="529">

---

If the response is valid, a success message is added to the model:

```java
			// Throws out different exceptions depending on the contents
			checkIfResponseValidCreateCust(responseObj);

			// If successful...
			model.addAttribute(LARGE_TEXT, "Customer creation successful");
			model.addAttribute(SMALL_TEXT, (responseObj.toPrettyString()));
```

---

</SwmSnippet>

# Handling account updates

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> handles account updates by displaying the update form and processing the form submission to update account details.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="575">

---

To display the account update form:

```java
	// 6. Update an account
	@GetMapping("/updateacc")
	public String showUpdateAccountForm(UpdateAccountForm updateAccForm,
			Model model)
	{
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="587">

---

To process the form submission:

```java
	@PostMapping("/updateacc")
	public String processCreateAcc(@Valid UpdateAccountForm updateAccountForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
		if (bindingResult.hasErrors())
		{
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="601">

---

The form submission processing involves serializing the form data to JSON and sending it to the backend service:

```java
		UpdateAccountJson transferjson = new UpdateAccountJson(
				updateAccountForm);

		// Serialise the object to JSON
		log.info("{}", transferjson);
		String jsonString = new ObjectMapper().writeValueAsString(transferjson);
		log.info("{}", jsonString);
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="625">

---

The response is then deserialized and checked for validity:

```java
			// Deserialise into a POJO
			UpdateAccountJson responseObj = new ObjectMapper()
					.readValue(responseBody, UpdateAccountJson.class);
			log.info("{}", responseObj);
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="630">

---

If the response is valid, the updated account details are added to the model:

```java
			// Throws out different exceptions depending on the contents
			checkIfResponseValidUpdateAcc(responseObj);

			// If successful...
			model.addAttribute(LARGE_TEXT, "");
			model.addAttribute(SMALL_TEXT, responseObj.toPrettyString());
```

---

</SwmSnippet>

# Handling customer deletions

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> handles customer deletions by displaying the deletion form and processing the form submission to delete a customer.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="848">

---

To display the customer deletion form:

```java
	// 9. Delete a customer
	@GetMapping("/delcust")
	public String showDelCustForm(CustomerEnquiryForm customerEnquiryForm)
	{
		return DELETE_CUSTOMER_FORM;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="856">

---

To process the form submission:

```java
	@PostMapping("/delcust")
	public String deleteCust(@Valid CustomerEnquiryForm customerEnquiryForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
		if (!bindingResult.hasErrors())
		{
			WebClient client = WebClient
					.create(ConnectionInfo.getAddressAndPort()
							+ "/delcus/remove/" + String
									.format(String
											.format("%10s",
													customerEnquiryForm
															.getCustNumber())
											.replace(" ", "0")));
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="872">

---

The form submission processing involves sending a delete request to the backend service:

```java
			try
			{
				ResponseSpec response = client.delete().retrieve();
				String responseBody = response.bodyToMono(String.class).block();
				log.info(responseBody);
				DeleteCustomerJson responseObj = new ObjectMapper()
						.readValue(responseBody, DeleteCustomerJson.class);
				log.info("{}", responseObj);
				checkIfResponseValidDeleteCust(responseObj);
				model.addAttribute(LARGE_TEXT,
						"Customer and associated accounts Deleted");
				model.addAttribute(SMALL_TEXT, responseObj.toPrettyString());
			}
			catch (ItemNotFoundException e)
			{
				log.info(e.toString());
				model.addAttribute(LARGE_TEXT, REQUEST_ERROR);
				model.addAttribute(SMALL_TEXT, e.getMessage());
			}
			catch (WebClientRequestException e)
			{
				log.info(e.toString());
				model.addAttribute(LARGE_TEXT, REQUEST_ERROR);
				model.addAttribute(SMALL_TEXT, CONNECTION_ERROR_MSG);
			}
			catch (Exception e)
			{
				log.info(e.toString());
				model.addAttribute(LARGE_TEXT, REQUEST_ERROR);
				model.addAttribute(SMALL_TEXT, ERROR_MSG);
			}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="872">

---

The response is then deserialized and checked for validity:

```java
			try
			{
				ResponseSpec response = client.delete().retrieve();
				String responseBody = response.bodyToMono(String.class).block();
				log.info(responseBody);
				DeleteCustomerJson responseObj = new ObjectMapper()
						.readValue(responseBody, DeleteCustomerJson.class);
				log.info("{}", responseObj);
				checkIfResponseValidDeleteCust(responseObj);
				model.addAttribute(LARGE_TEXT,
						"Customer and associated accounts Deleted");
				model.addAttribute(SMALL_TEXT, responseObj.toPrettyString());
			}
			catch (ItemNotFoundException e)
			{
				log.info(e.toString());
				model.addAttribute(LARGE_TEXT, REQUEST_ERROR);
				model.addAttribute(SMALL_TEXT, e.getMessage());
			}
			catch (WebClientRequestException e)
			{
				log.info(e.toString());
				model.addAttribute(LARGE_TEXT, REQUEST_ERROR);
				model.addAttribute(SMALL_TEXT, CONNECTION_ERROR_MSG);
			}
			catch (Exception e)
			{
				log.info(e.toString());
				model.addAttribute(LARGE_TEXT, REQUEST_ERROR);
				model.addAttribute(SMALL_TEXT, ERROR_MSG);
			}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="872">

---

If the response is valid, a success message is added to the model:

```java
			try
			{
				ResponseSpec response = client.delete().retrieve();
				String responseBody = response.bodyToMono(String.class).block();
				log.info(responseBody);
				DeleteCustomerJson responseObj = new ObjectMapper()
						.readValue(responseBody, DeleteCustomerJson.class);
				log.info("{}", responseObj);
				checkIfResponseValidDeleteCust(responseObj);
				model.addAttribute(LARGE_TEXT,
						"Customer and associated accounts Deleted");
				model.addAttribute(SMALL_TEXT, responseObj.toPrettyString());
			}
			catch (ItemNotFoundException e)
			{
				log.info(e.toString());
				model.addAttribute(LARGE_TEXT, REQUEST_ERROR);
				model.addAttribute(SMALL_TEXT, e.getMessage());
			}
			catch (WebClientRequestException e)
			{
				log.info(e.toString());
				model.addAttribute(LARGE_TEXT, REQUEST_ERROR);
				model.addAttribute(SMALL_TEXT, CONNECTION_ERROR_MSG);
			}
			catch (Exception e)
			{
				log.info(e.toString());
				model.addAttribute(LARGE_TEXT, REQUEST_ERROR);
				model.addAttribute(SMALL_TEXT, ERROR_MSG);
			}
```

---

</SwmSnippet>

This document has covered the main design decisions and implementation details of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="533:9:9" line-data="			model.addAttribute(LARGE_TEXT, &quot;Customer creation successful&quot;);">`Customer`</SwmToken>` Services Interface`.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
