---
title: Introduction to AccountEnquiryJson
---
# Introduction to <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="33:20:20" line-data="import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry.AccountEnquiryJson;">`AccountEnquiryJson`</SwmToken>

<SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="33:20:20" line-data="import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry.AccountEnquiryJson;">`AccountEnquiryJson`</SwmToken> is a class that represents the structure of an account enquiry response in JSON format.

This class contains a field named `inqaccCommarea`, which is an instance of the `InqaccJson` class.

The constructor of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="33:20:20" line-data="import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry.AccountEnquiryJson;">`AccountEnquiryJson`</SwmToken> class initializes the `inqaccCommarea` field.

Additionally, the class provides getter and setter methods for the `inqaccCommarea` field.

## Usage in <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="192:7:7" line-data="				log.info(e.toString());">`toString`</SwmToken> method returns a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="33:20:20" line-data="import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry.AccountEnquiryJson;">`AccountEnquiryJson`</SwmToken> object, including the `inqaccCommarea` field.

## AccountEnquiry Overview

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="33:20:20" line-data="import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry.AccountEnquiryJson;">`AccountEnquiryJson`</SwmToken> class is used in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class to handle HTTP GET and POST requests related to account enquiries.

## Example Usage

AccountEnquiry is used to handle the process of querying account details. It involves creating instances of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="125:7:7" line-data="	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)">`AccountEnquiryForm`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="33:20:20" line-data="import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry.AccountEnquiryJson;">`AccountEnquiryJson`</SwmToken> classes, which manage the input and output data for account enquiries.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="124">

---

An example of `AccountEnquiry` usage is in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class, where it processes account enquiry forms and returns account details in JSON format.

```java
	@GetMapping("/enqacct")
	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)
	{
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
