---
title: Customer Services Interface Overview
---
## Implementation

# Customer Services Interface Overview

The Customer Services Interface is a key component that facilitates various customer-related operations within the application.

It is implemented as a Spring Boot application, which is initialized in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/CustomerServices.java" pos="24:13:13" line-data="		final Logger log = LoggerFactory.getLogger(CustomerServices.class);">`CustomerServices`</SwmToken> class.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/CustomerServices.java" line="22">

---

The main method in this class sets up the application context and starts the Spring application.

```java
	public static void main(String[] args)
	{
		final Logger log = LoggerFactory.getLogger(CustomerServices.class);
		JCommander.newBuilder().build().parse(args);

		log.info("Running with address: {}",
				ConnectionInfo.getAddressAndPort());

		// Run the application. From here out, only the WebController and
		// ParamsController classes really matter.
		SpringApplication.run(CustomerServices.class, args);
	}
```

---

</SwmSnippet>

## User Interface

The interface provides a web-based user interface defined in the <SwmPath>[src/…/templates/customerServices.html](src/Z-OS-Connect-Customer-Services-Interface/src/main/resources/templates/customerServices.html)</SwmPath> template, allowing users to perform actions such as creating, viewing, updating, and deleting customer and account details.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/resources/templates/customerServices.html" line="1">

---

This template is the main entry point for users interacting with the Customer Services Interface.

```html
<!DOCTYPE html>
<!--                                                                 -->
<!--  Copyright IBM Corp. 2023                                       -->
<!--                                                                 -->
<!--                                                                 -->
<html lang="en">

<head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="stylesheet" href="https://unpkg.com/carbon-components/css/carbon-components.min.css">
    <link rel="stylesheet" href="/customerservices-1.0/styles/styles.css">
    <title>Account/Customer management utility</title>
</head>

<body>
    <div class="main-body">
        <div class="centre">
            <h1 class="homepage-title">Customer Services</h1>
            <div class="options-box">
```

---

</SwmSnippet>

## Request Handling

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/CustomerServices.java" pos="30:21:21" line-data="		// Run the application. From here out, only the WebController and">`WebController`</SwmToken> class handles the routing and processing of these customer and account service requests.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="105">

---

It maps various endpoints to their respective handler methods, which process the requests and return the appropriate views.

```java
	// Customer and account services screen
	@GetMapping(value =
	{ "","/services", "/" })
	public String showCustServices(Model model)
	{

		model.addAttribute("contextPath", "");
		return "customerServices";
	}
```

---

</SwmSnippet>

## Build and Run Configuration

## Form Templates

The interface also includes several form templates for different operations, such as <SwmPath>[src/…/templates/customerEnquiryForm.html](src/Z-OS-Connect-Customer-Services-Interface/src/main/resources/templates/customerEnquiryForm.html)</SwmPath> for customer inquiries and <SwmPath>[src/…/templates/createAccountForm.html](src/Z-OS-Connect-Customer-Services-Interface/src/main/resources/templates/createAccountForm.html)</SwmPath> for account creation.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="1">

---

To build the Customer Services Interface, you need to use Maven. Navigate to the <SwmPath>[src/Z-OS-Connect-Customer-Services-Interface/](src/Z-OS-Connect-Customer-Services-Interface/)</SwmPath> directory and run the `mvn clean install` command. This will compile the project and package it into a WAR file.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright IBM Corp. 2023 -->
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd">
	<modelVersion>4.0.0</modelVersion>
	<parent>
		<groupId>org.springframework.boot</groupId>
		<artifactId>spring-boot-starter-parent</artifactId>
		<version>3.2.5</version>
		<relativePath /> <!-- lookup parent from repository -->
	</parent>


	<groupId>com.ibm.cics.cip.bank.springboot</groupId>
	<artifactId>customerservices</artifactId>
	<version>1.0</version>
	<packaging>war</packaging>
	<name>customerservices</name>
	<description>Springboot project utilising Z/OS Connect</description>
	<properties>
		<java.version>17</java.version>
```

---

</SwmSnippet>

# Customer Services Endpoints

To run the Customer Services Interface, you can use the Spring Boot Maven plugin. Execute the `mvn `<SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="8:4:6" line-data="		&lt;artifactId&gt;spring-boot-starter-parent&lt;/artifactId&gt;">`spring-boot`</SwmToken>`:`<SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/CustomerServices.java" pos="32:3:3" line-data="		SpringApplication.run(CustomerServices.class, args);">`run`</SwmToken> command in the <SwmPath>[src/Z-OS-Connect-Customer-Services-Interface/](src/Z-OS-Connect-Customer-Services-Interface/)</SwmPath> directory. This will start the Spring Boot application, and you can access the Customer Services pages at the URL <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/resources/templates/customerServices.html" pos="13:13:19" line-data="    &lt;link rel=&quot;stylesheet&quot; href=&quot;/customerservices-1.0/styles/styles.css&quot;&gt;">`/customerservices-1.0/`</SwmToken>.

## <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="235:5:5" line-data="	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)">`showCustForm`</SwmToken>

The Customer Services Interface includes several endpoints for handling different customer-related operations.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="234">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="235:5:5" line-data="	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)">`showCustForm`</SwmToken> endpoint is mapped to the URL <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="234:5:6" line-data="	@GetMapping(&quot;/enqcust&quot;)">`/enqcust`</SwmToken> and is responsible for displaying the customer enquiry form. This endpoint handles GET requests and initializes the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="235:7:7" line-data="	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)">`CustomerEnquiryForm`</SwmToken> object.

```java
	@GetMapping("/enqcust")
	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)
	{
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="491">

---

## <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="491:5:5" line-data="	public String processCreateCust(@Valid CreateCustomerForm createCustForm,">`processCreateCust`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="491:5:5" line-data="	public String processCreateCust(@Valid CreateCustomerForm createCustForm,">`processCreateCust`</SwmToken> endpoint is mapped to the URL <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/resources/templates/customerServices.html" pos="24:11:12" line-data="                    &lt;a th:href=&quot;@{/createcust}&quot;&gt;">`/createcust`</SwmToken> and processes the creation of a new customer. This endpoint handles POST requests and validates the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="491:10:10" line-data="	public String processCreateCust(@Valid CreateCustomerForm createCustForm,">`CreateCustomerForm`</SwmToken> object before processing the customer creation.

```java
	public String processCreateCust(@Valid CreateCustomerForm createCustForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
