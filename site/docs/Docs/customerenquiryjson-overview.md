---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: fs19wny0
---
# CustomerEnquiryJson Overview

## Overview

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="40:20:20" line-data="import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry.CustomerEnquiryJson;">`CustomerEnquiryJson`</SwmToken> class is a part of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="32:16:16" line-data="import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.accountenquiry.AccountEnquiryForm;">`jsonclasses`</SwmToken> package and is used to handle customer enquiry data in the application. It plays a crucial role in managing and formatting customer enquiry details.

## Fields and Methods

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="40:20:20" line-data="import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry.CustomerEnquiryJson;">`CustomerEnquiryJson`</SwmToken> class contains a field `INQCUSTZ`, which is an instance of the `InqCustZJson` class. This field holds the customer enquiry details. The class provides a default constructor to initialize the object and getter and setter methods for the `INQCUSTZ` field.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="234">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="235:5:5" line-data="	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)">`showCustForm`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class is used to display the customer enquiry form when the 'View customer details' button is clicked.

```java
	@GetMapping("/enqcust")
	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)
	{
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="241">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="242:5:5" line-data="	public String returnCust(@Valid CustomerEnquiryForm customerEnquiryForm,">`returnCust`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class processes the customer enquiry form and returns the customer details.

```java
	@PostMapping("/enqcust")
	public String returnCust(@Valid CustomerEnquiryForm customerEnquiryForm,
			BindingResult bindingResult, Model model)
```

---

</SwmSnippet>

## String Representation

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="192:7:7" line-data="				log.info(e.toString());">`toString`</SwmToken> method is overridden to provide a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="40:20:20" line-data="import com.ibm.cics.cip.bank.springboot.customerservices.jsonclasses.customerenquiry.CustomerEnquiryJson;">`CustomerEnquiryJson`</SwmToken> object, including the `INQCUSTZ` field.

## Pretty String Format

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="187:10:10" line-data="				model.addAttribute(SMALL_TEXT, responseObj.toPrettyString());">`toPrettyString`</SwmToken> method formats the customer enquiry details into a more readable string format, including customer number, name, address, date of birth, credit score, review date, and sort code.

## How to Use CustomerEnquiry

CustomerEnquiry is accessed through the Customer Services interface. To see customer details, click the 'View customer details' button on the landing page.

## Where CustomerEnquiry is Used

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="235:5:5" line-data="	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)">`showCustForm`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class is used to display the customer enquiry form when the 'View customer details' button is clicked. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="242:5:5" line-data="	public String returnCust(@Valid CustomerEnquiryForm customerEnquiryForm,">`returnCust`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class processes the customer enquiry form and returns the customer details.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
