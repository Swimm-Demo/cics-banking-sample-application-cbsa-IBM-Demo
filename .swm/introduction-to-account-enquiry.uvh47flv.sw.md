---
title: Introduction to Account Enquiry
---
# Introduction to Account Enquiry

The Account Enquiry feature in the CICS Bank Sample Application allows users to view detailed information about a specific bank account. This feature is accessible through the Customer Services Interface and involves several components including forms, JSON classes, and controllers.

# Account Enquiries in Customer Services Interface

To enquire about an account, navigate to the landing page and click on 'View account details'. This action will direct you to the account enquiry form where you can input the account number.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryForm.java" line="9">

---

# <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryForm.java" pos="9:4:4" line-data="public class AccountEnquiryForm">`AccountEnquiryForm`</SwmToken> Class

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryForm.java" pos="9:4:4" line-data="public class AccountEnquiryForm">`AccountEnquiryForm`</SwmToken> class captures the account number for which details are being requested. It includes methods to get and set the account number, and to convert the form data to a string representation.

```java
public class AccountEnquiryForm
{



	@NotNull
	@Size(max = 8)
	private String acctNumber;


	public AccountEnquiryForm()
	{

	}


	public AccountEnquiryForm(@NotNull @Size(max = 8) String acctNumber)
	{
		this.acctNumber = acctNumber;
	}
```

---

</SwmSnippet>

# <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="23:3:3" line-data="	public AccountEnquiryJson()">`AccountEnquiryJson`</SwmToken> Class

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="23:3:3" line-data="	public AccountEnquiryJson()">`AccountEnquiryJson`</SwmToken> class structures the JSON response for an account enquiry. It includes a nested <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="20:3:3" line-data="	private InqaccJson inqaccCommarea;">`InqaccJson`</SwmToken> object that holds detailed account information.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" line="19">

---

The field <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="20:5:5" line-data="	private InqaccJson inqaccCommarea;">`inqaccCommarea`</SwmToken> is an instance of <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="20:3:3" line-data="	private InqaccJson inqaccCommarea;">`InqaccJson`</SwmToken> and is annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="19:1:2" line-data="	@JsonProperty(&quot;INQACC_COMMAREA&quot;)">`@JsonProperty`</SwmToken> to map the JSON property <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="19:5:5" line-data="	@JsonProperty(&quot;INQACC_COMMAREA&quot;)">`INQACC_COMMAREA`</SwmToken>.

```java
	@JsonProperty("INQACC_COMMAREA")
	private InqaccJson inqaccCommarea;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" line="23">

---

The constructor initializes <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="25:1:1" line-data="		inqaccCommarea = new InqaccJson();">`inqaccCommarea`</SwmToken> with a new instance of <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="25:7:7" line-data="		inqaccCommarea = new InqaccJson();">`InqaccJson`</SwmToken>.

```java
	public AccountEnquiryJson()
	{
		inqaccCommarea = new InqaccJson();
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" line="41">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="42:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="44:4:4" line-data="		return &quot;AccountEnquiryJson [INQACC_COMMAREA=&quot;">`AccountEnquiryJson`</SwmToken> object, including the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="45:3:3" line-data="				+ inqaccCommarea.toString() + &quot;]&quot;;">`inqaccCommarea`</SwmToken> field.

```java
	@Override
	public String toString()
	{
		return "AccountEnquiryJson [INQACC_COMMAREA="
				+ inqaccCommarea.toString() + "]";
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" line="49">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryJson.java" pos="49:5:5" line-data="	public String toPrettyString()">`toPrettyString`</SwmToken> method formats the account enquiry details into a human-readable string, using various utility methods for formatting.

```java
	public String toPrettyString()
	{
		InqaccJson acctInfo = inqaccCommarea;
		String output = "";
		output += "Account Number: "
				+ OutputFormatUtils.leadingZeroes(8, acctInfo.getInqaccAccno())
				+ "\n" + "Customer Number: "
				+ OutputFormatUtils.leadingZeroes(10,
						acctInfo.getInqaccCustno())
				+ "\n" + "Account Type: " + acctInfo.getInqaccAccType() + "\n"
				+ "Available Balance: "
				+ String.format(FLOAT_FORMAT,
						acctInfo.getInqaccAvailableBalance())
				+ "\n" + "Actual Balance: "
				+ String.format(FLOAT_FORMAT, acctInfo.getInqaccActualBalance())
				+ "\n" + "Interest Rate: "
				+ String.format(FLOAT_FORMAT, acctInfo.getInqaccInterestRate())
				+ "\n" + "Overdraft: " + acctInfo.getInqaccOverdraft() + "\n"
				+ "Account Opened: "
				+ OutputFormatUtils.date(acctInfo.getInqaccOpened()) + "\n"
				+ "Next Statement Date: "
```

---

</SwmSnippet>

# <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> Usage

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class handles the HTTP GET and POST requests for account enquiries. It uses the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/accountenquiry/AccountEnquiryForm.java" pos="9:4:4" line-data="public class AccountEnquiryForm">`AccountEnquiryForm`</SwmToken> to capture user input and process the enquiry.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="124">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="125:5:5" line-data="	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)">`showAcctForm`</SwmToken> method handles the HTTP GET request to display the account enquiry form.

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

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="137">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="138:5:5" line-data="	public String returnAcct(@Valid AccountEnquiryForm accountEnquiryForm,">`returnAcct`</SwmToken> method handles the HTTP POST request to process the submitted account enquiry form.

```java
	@PostMapping("/enqacct")
	public String returnAcct(@Valid AccountEnquiryForm accountEnquiryForm,
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
