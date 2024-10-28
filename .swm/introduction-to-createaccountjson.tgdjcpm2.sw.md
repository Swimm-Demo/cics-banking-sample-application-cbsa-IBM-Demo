---
title: Introduction to CreateAccountJson
---
# Introduction to <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="22:3:3" line-data="	public CreateAccountJson(CreateAccountForm createAccountForm)">`CreateAccountJson`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="22:3:3" line-data="	public CreateAccountJson(CreateAccountForm createAccountForm)">`CreateAccountJson`</SwmToken> class represents the JSON structure required for creating a new account. It encapsulates the account details within a field named <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="24:1:1" line-data="		creAcc = new CreaccJson(createAccountForm.getAccountType().toString(),">`creAcc`</SwmToken>, which is an instance of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="24:7:7" line-data="		creAcc = new CreaccJson(createAccountForm.getAccountType().toString(),">`CreaccJson`</SwmToken> class.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" line="22">

---

The constructor of <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="22:3:3" line-data="	public CreateAccountJson(CreateAccountForm createAccountForm)">`CreateAccountJson`</SwmToken> initializes the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="24:1:1" line-data="		creAcc = new CreaccJson(createAccountForm.getAccountType().toString(),">`creAcc`</SwmToken> field using data from a <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="22:5:5" line-data="	public CreateAccountJson(CreateAccountForm createAccountForm)">`CreateAccountForm`</SwmToken> object. This example shows how the account type, customer number, overdraft limit, and interest rate are extracted from the form and used to create a <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="24:7:7" line-data="		creAcc = new CreaccJson(createAccountForm.getAccountType().toString(),">`CreaccJson`</SwmToken> object.

```java
	public CreateAccountJson(CreateAccountForm createAccountForm)
	{
		creAcc = new CreaccJson(createAccountForm.getAccountType().toString(),
				createAccountForm.getCustNumber(),
				createAccountForm.getOverdraftLimit(),
				createAccountForm.getInterestRate());

	}
```

---

</SwmSnippet>

# Methods

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="22:3:3" line-data="	public CreateAccountJson(CreateAccountForm createAccountForm)">`CreateAccountJson`</SwmToken> class provides several methods to interact with the account details:

1. <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="38:5:5" line-data="	public CreaccJson getCreAcc()">`getCreAcc`</SwmToken>: This method retrieves the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="24:1:1" line-data="		creAcc = new CreaccJson(createAccountForm.getAccountType().toString(),">`creAcc`</SwmToken> field, allowing access to the account details.

2. <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="57:5:5" line-data="	public String toPrettyString()">`toPrettyString`</SwmToken>: This method formats the account details into a readable string, making it easier to display or log the information.

3. <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="24:15:15" line-data="		creAcc = new CreaccJson(createAccountForm.getAccountType().toString(),">`toString`</SwmToken>: This method provides a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="22:3:3" line-data="	public CreateAccountJson(CreateAccountForm createAccountForm)">`CreateAccountJson`</SwmToken> object, which can be useful for debugging or logging purposes.

# Usage in <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="22:3:3" line-data="	public CreateAccountJson(CreateAccountForm createAccountForm)">`CreateAccountJson`</SwmToken> class is utilized in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class to handle account creation requests. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="373:5:5" line-data="	public String showCreateAccForm(CreateAccountForm createAccForm,">`showCreateAccForm`</SwmToken> method displays the account creation form, while the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="382:5:5" line-data="	public String processCreateAcc(@Valid CreateAccountForm createAccForm,">`processCreateAcc`</SwmToken> method processes the form data and creates a new account using <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="22:3:3" line-data="	public CreateAccountJson(CreateAccountForm createAccountForm)">`CreateAccountJson`</SwmToken>.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="372">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="373:5:5" line-data="	public String showCreateAccForm(CreateAccountForm createAccForm,">`showCreateAccForm`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class displays the account creation form.

```java
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

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="381">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="382:5:5" line-data="	public String processCreateAcc(@Valid CreateAccountForm createAccForm,">`processCreateAcc`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class processes the form data and creates a new account using <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createaccount/CreateAccountJson.java" pos="22:3:3" line-data="	public CreateAccountJson(CreateAccountForm createAccountForm)">`CreateAccountJson`</SwmToken>.

```java
	@PostMapping("/createacc")
	public String processCreateAcc(@Valid CreateAccountForm createAccForm,
			BindingResult bindingResult, Model model)
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
