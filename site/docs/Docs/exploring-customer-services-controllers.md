---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: fgvjk81r
---
# Exploring Customer Services Controllers

## Overview of Customer Services Controllers

Controllers in the Customer Services Interface are responsible for handling HTTP requests and mapping them to the appropriate service methods. They manage the flow of data between the model and the view, ensuring that user inputs are processed and the correct responses are generated.

## <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> Class

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class is a central component that defines various endpoints for customer and account-related operations. It includes methods for handling GET and POST requests for actions such as account and customer inquiries, listing accounts, creating, updating, and deleting accounts and customers.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="371">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="373:5:5" line-data="	public String showCreateAccForm(CreateAccountForm createAccForm,">`showCreateAccForm`</SwmToken> method handles GET requests to display the account creation form. This method sets up the form for user input to create a new account.

```java
	// 4. Create an account
	@GetMapping("/createacc")
	public String showCreateAccForm(CreateAccountForm createAccForm,
			Model model)
	{
		model.addAttribute(ACCOUNT_TYPES, AccountType.values());
		return CREATE_ACCOUNT_FORM;
	}
```

---

</SwmSnippet>

## Handling Form Submissions

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="382:5:5" line-data="	public String processCreateAcc(@Valid CreateAccountForm createAccForm,">`processCreateAcc`</SwmToken> method processes the form submission via a POST request, validates the input, and interacts with the service layer.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="381">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="382:5:5" line-data="	public String processCreateAcc(@Valid CreateAccountForm createAccForm,">`processCreateAcc`</SwmToken> method handles the form submission for creating a new account. It validates the input, serializes the data to JSON, and sends it to the service layer.

```java
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

		// Serialise the object to JSON
		log.info("{}", transferjson);
		String jsonString = new ObjectMapper().writeValueAsString(transferjson);
		log.info(jsonString);

		WebClient client = WebClient
				.create(ConnectionInfo.getAddressAndPort() + "/creacc/insert");

		try
```

---

</SwmSnippet>

## Customer Services Interface Endpoints

The Customer Services Interface includes several endpoints for handling different operations related to customer and account management.

#### /enqacct

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="124:5:6" line-data="	@GetMapping(&quot;/enqacct&quot;)">`/enqacct`</SwmToken> endpoint handles GET requests to display the account inquiry form. This method initializes the form and prepares it for user input.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="124">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="125:5:5" line-data="	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)">`showAcctForm`</SwmToken> method handles GET requests to display the account inquiry form.

```java
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

#### /createacc

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="372:5:6" line-data="	@GetMapping(&quot;/createacc&quot;)">`/createacc`</SwmToken> endpoint handles GET requests to display the form for creating a new account. This method sets up the form for user input to create a new account.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
