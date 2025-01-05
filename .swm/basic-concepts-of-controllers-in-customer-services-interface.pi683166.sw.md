---
title: Basic Concepts of Controllers in Customer Services Interface
---
# <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> Responsibilities

# Overview of Controllers

Controllers in the Customer Services Interface manage the flow of data between the user interface and the backend services. They handle HTTP requests, process input data, and return the appropriate responses.

# Defining Endpoints

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class is responsible for handling various customer and account-related operations such as creating, updating, and deleting accounts and customers.

# Example: Creating an Account

It uses methods annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="124:1:2" line-data="	@GetMapping(&quot;/enqacct&quot;)">`@GetMapping`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="381:1:2" line-data="	@PostMapping(&quot;/createacc&quot;)">`@PostMapping`</SwmToken> to define endpoints for different operations, ensuring that the correct forms are displayed and processed.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="371">

---

For instance, the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class handles the creation of an account using the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="372:1:2" line-data="	@GetMapping(&quot;/createacc&quot;)">`@GetMapping`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="381:1:2" line-data="	@PostMapping(&quot;/createacc&quot;)">`@PostMapping`</SwmToken> annotations to manage the form display and submission process.

```java
	// 4. Create an account
	@GetMapping("/createacc")
	public String showCreateAccForm(CreateAccountForm createAccForm,
			Model model)
	{
		model.addAttribute(ACCOUNT_TYPES, AccountType.values());
		return CREATE_ACCOUNT_FORM;
	}


	@PostMapping("/createacc")
	public String processCreateAcc(@Valid CreateAccountForm createAccForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
		if (bindingResult.hasErrors())
		{
			model.addAttribute(ACCOUNT_TYPES, AccountType.values());
			return CREATE_ACCOUNT_FORM;
		}
		CreateAccountJson transferjson = new CreateAccountJson(createAccForm);
```

---

</SwmSnippet>

## <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="125:5:5" line-data="	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)">`showAcctForm`</SwmToken>

# Endpoints of Controllers

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class defines several endpoints to handle different operations.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="124">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="125:5:5" line-data="	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)">`showAcctForm`</SwmToken> function is used to display the account enquiry form. It handles GET requests to the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="124:5:6" line-data="	@GetMapping(&quot;/enqacct&quot;)">`/enqacct`</SwmToken> endpoint.

```java
	@GetMapping("/enqacct")
	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)
	{
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="234">

---

## <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="235:5:5" line-data="	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)">`showCustForm`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="235:5:5" line-data="	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)">`showCustForm`</SwmToken> function is used to display the customer enquiry form. It handles GET requests to the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="234:5:6" line-data="	@GetMapping(&quot;/enqcust&quot;)">`/enqcust`</SwmToken> endpoint.

```java
	@GetMapping("/enqcust")
	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)
	{
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
