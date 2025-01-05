---
title: Understanding DeleteCustomerJson
---
# Field Mapping

# Overview of <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="42:4:4" line-data="		return &quot;DeleteCustomerJson [DelCus=&quot; + delcus.toString() + &quot;]&quot;;">`DeleteCustomerJson`</SwmToken>

<SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="42:4:4" line-data="		return &quot;DeleteCustomerJson [DelCus=&quot; + delcus.toString() + &quot;]&quot;;">`DeleteCustomerJson`</SwmToken> is a class that represents the JSON structure for deleting a customer. It contains a field named <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="29:3:3" line-data="		return delcus;">`delcus`</SwmToken>, which is an instance of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="27:3:3" line-data="	public DelcusJson getDelcus()">`DelcusJson`</SwmToken> class.

# Getter and Setter Methods

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="29:3:3" line-data="		return delcus;">`delcus`</SwmToken> field is annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="17:1:2" line-data="	@JsonProperty(&quot;DELCUS&quot;)">`@JsonProperty`</SwmToken> to map it to the corresponding JSON property 'DELCUS'. This annotation ensures that the field is correctly serialized and deserialized when converting between JSON and Java objects.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" line="27">

---

The class provides getter and setter methods for the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="29:3:3" line-data="		return delcus;">`delcus`</SwmToken> field, allowing access and modification of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="27:3:3" line-data="	public DelcusJson getDelcus()">`DelcusJson`</SwmToken> object.

```java
	public DelcusJson getDelcus()
	{
		return delcus;
	}


	public void setDelcus(DelcusJson delcusIn)
	{
		delcus = delcusIn;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" line="39">

---

# String Representation

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="40:5:5" line-data="	public String toString()">`toString`</SwmToken> method in <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="42:4:4" line-data="		return &quot;DeleteCustomerJson [DelCus=&quot; + delcus.toString() + &quot;]&quot;;">`DeleteCustomerJson`</SwmToken> returns a string representation of the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/deletecustomer/DeleteCustomerJson.java" pos="42:13:13" line-data="		return &quot;DeleteCustomerJson [DelCus=&quot; + delcus.toString() + &quot;]&quot;;">`delcus`</SwmToken> object. This is useful for logging and debugging purposes.

```java
	@Override
	public String toString()
	{
		return "DeleteCustomerJson [DelCus=" + delcus.toString() + "]";
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="912">

---

# Usage Example

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="913:6:6" line-data="		if (responseObj.getDelcus().getCommDelFailCode() == 1)">`getDelcus`</SwmToken> method is used within <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="880:1:1" line-data="				checkIfResponseValidDeleteCust(responseObj);">`checkIfResponseValidDeleteCust`</SwmToken> in <SwmPath>[src/…/controllers/WebController.java](src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java)</SwmPath> to validate the response. This ensures that the deletion request has been processed correctly.

```java
	{
		if (responseObj.getDelcus().getCommDelFailCode() == 1)
		{
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
