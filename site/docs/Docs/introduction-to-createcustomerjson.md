---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: bwgb2gy8
---
# Introduction to CreateCustomerJson

## Introduction to CreateCustomerJson

The `CreateCustomerJson` class represents the JSON structure for creating a customer. It is designed to facilitate the conversion of customer details into a JSON format for further processing.

## Structure of CreateCustomerJson

The class contains a field `creCust`, which is an instance of `CrecustJson`. This field is annotated with `@JsonProperty` to map the JSON property `CRECUST`. The class has two constructors: a default constructor and another that takes a <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="9:4:4" line-data="public class CreateCustomerForm">`CreateCustomerForm`</SwmToken> object to initialize the `creCust` field.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" line="9">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="9:4:4" line-data="public class CreateCustomerForm">`CreateCustomerForm`</SwmToken> class includes methods for setting and validating customer details such as name, address, and date of birth. It ensures that these details meet certain criteria before creating the customer record.

```java
public class CreateCustomerForm
{


	@NotNull
	@Size(max = 61)
	private String custName;

	@NotNull
	@Size(max = 161)
	private String custAddress;

	@NotNull
	@Size(min = 8, max = 8)
	private String custDob;


	public CreateCustomerForm()
	{
		super();
	}
```

---

</SwmSnippet>

## Methods in CreateCustomerJson

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="71:5:5" line-data="	public String toString()">`toString`</SwmToken> method provides a string representation of the `CreateCustomerJson` object, including the `creCust` field. The `toPrettyString` method formats the customer details into a human-readable string, using utility methods for formatting.

## Usage of CreateCustomerJson

The `CreateCustomerJson` class is used to handle the creation of new customer records. This involves setting customer details such as name, address, and date of birth, and then validating these details before creating the customer record. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerForm.java" pos="9:4:4" line-data="public class CreateCustomerForm">`CreateCustomerForm`</SwmToken> class provides methods for setting these details and ensuring they meet certain criteria, such as not being null and having a specific length.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
