---
title: Payment Interface Overview
---
# Payment Interface Overview

The Payment Interface is a key component that facilitates the processing of payment transactions within the application.

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" line="14">

---

It is implemented as a Spring Boot application, which is initialized and run by the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" pos="16:4:4" line-data="public class PaymentInterface">`PaymentInterface`</SwmToken> class.

```java
@SpringBootApplication(scanBasePackages =
{ "com.ibm.cics.cip.bank.springboot.paymentinterface.controllers" })
public class PaymentInterface
{




	public static void main(String[] args)
	{
		final Logger log = LoggerFactory.getLogger(PaymentInterface.class);

		JCommander.newBuilder().build().parse(args);

		log.info("Running with address: {}",
				ConnectionInfo.getAddressAndPort());

		// Run the application. From here out, only the WebController and
		// ParamsController classes really matter.
		SpringApplication.run(PaymentInterface.class, args);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="33">

---

# HTTP Request Handling

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="34:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class handles the HTTP requests related to payment operations, including displaying the payment form and processing payment submissions.

```java
@Controller
public class WebController implements WebMvcConfigurer
{


	private static final Logger log = LoggerFactory
			.getLogger(WebController.class);

	private static final String FORM_NAME = "paymentInterfaceForm";

	private static final String LARGE_TEXT = "largeText";

	private static final String SMALL_TEXT = "smallText";

	private static final String PAYMENT_ERROR = "Payment Error";


	// Payment Interface
	@GetMapping(value={"/",""})
	public String showForm(TransferForm personForm)
	{
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="58">

---

# Form Submission

When a payment form is submitted, the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="59:5:5" line-data="	public String checkPersonInfo(@Valid TransferForm transferForm,">`checkPersonInfo`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" pos="31:21:21" line-data="		// Run the application. From here out, only the WebController and">`WebController`</SwmToken> class validates the input, serializes the form data into a JSON object, and sends it to the payment processing endpoint.

```java
	@PostMapping("/paydbcr")
	public String checkPersonInfo(@Valid TransferForm transferForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{

		// The same page is returned, this time with the errors given as an
		// object.
		if (bindingResult.hasErrors())
		{
			return FORM_NAME;
		}

		PaymentInterfaceJson transferjson = new PaymentInterfaceJson(
				transferForm);

		// Serialise the object to JSON
		log.info("{}", transferjson);
		String jsonString = new ObjectMapper().writeValueAsString(transferjson);
		log.info(jsonString);

```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" line="95">

---

# Response Handling

The response from the payment processing endpoint is then deserialized back into a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="96:1:1" line-data="			PaymentInterfaceJson responseObj = new ObjectMapper()">`PaymentInterfaceJson`</SwmToken> object, which contains the details of the transaction.

```java
			// Deserialise into a POJO
			PaymentInterfaceJson responseObj = new ObjectMapper()
					.readValue(responseBody, PaymentInterfaceJson.class);
			log.info("{}", responseObj);

```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" line="10">

---

# JSON Mapping

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/PaymentInterfaceJson.java" pos="11:4:4" line-data="public class PaymentInterfaceJson">`PaymentInterfaceJson`</SwmToken> class is used to map the JSON data to Java objects, facilitating the handling of payment data within the application.

```java
@JsonNaming(JsonPropertyNamingStrategy.class)
public class PaymentInterfaceJson
{



	@JsonProperty("PAYDBCR")
	private DbcrJson payDbCr;


	public PaymentInterfaceJson()
	{

	}


	public PaymentInterfaceJson(TransferForm transferForm)
	{
		payDbCr = new DbcrJson(transferForm);
	}

```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/pom.xml" line="178">

---

# Build and Run Configuration

## Building the Payment Interface

To build the Payment Interface, you need to use Maven. Navigate to the <SwmPath>[src/Z-OS-Connect-Payment-Interface/](src/Z-OS-Connect-Payment-Interface/)</SwmPath> directory and run the `mvn `<SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="188:4:4" line-data="						&lt;phase&gt;package&lt;/phase&gt;">`package`</SwmToken> command. This will compile the project and package it into a WAR file.

```xml
	<build>
		<plugins>
			<plugin>
				<groupId>org.apache.maven.plugins</groupId>
				<artifactId>maven-war-plugin</artifactId>
				<executions>
					<execution>
						<goals>
							<goal>war</goal>
						</goals>
						<phase>package</phase>
					</execution>
				</executions>
			</plugin>
			<plugin>
				<groupId>org.springframework.boot</groupId>
				<artifactId>spring-boot-maven-plugin</artifactId>
				<executions>
					<execution>
						<id>repackage</id>
						<goals>
```

---

</SwmSnippet>

# Payment Interface Endpoints

## Running the Payment Interface

To run the Payment Interface, you can use the Spring Boot Maven plugin. After building the project, execute the `mvn `<SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="194:4:6" line-data="				&lt;artifactId&gt;spring-boot-maven-plugin&lt;/artifactId&gt;">`spring-boot`</SwmToken>`:`<SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" pos="33:3:3" line-data="		SpringApplication.run(PaymentInterface.class, args);">`run`</SwmToken> command in the <SwmPath>[src/Z-OS-Connect-Payment-Interface/](src/Z-OS-Connect-Payment-Interface/)</SwmPath> directory. This will start the Spring Boot application, and you can access the Payment Interface via the URL `/paymentinterface-1.1/`.

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" line="35">

---

## submit

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="35:6:6" line-data="	@PostMapping(&quot;/submit&quot;)">`submit`</SwmToken> endpoint is used to process payment transactions. It is defined in the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/PaymentInterface.java" pos="32:3:3" line-data="		// ParamsController classes really matter.">`ParamsController`</SwmToken> class and is mapped to the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="35:5:6" line-data="	@PostMapping(&quot;/submit&quot;)">`/submit`</SwmToken> URL. This endpoint accepts three parameters: <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="37:9:9" line-data="			@RequestParam(name = &quot;acctnum&quot;, required = true) String acctNumber,">`acctnum`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="38:9:9" line-data="			@RequestParam(name = &quot;amount&quot;, required = true) float amount,">`amount`</SwmToken>, and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="39:9:9" line-data="			@RequestParam(name = &quot;organisation&quot;, required = true) String organisation)">`organisation`</SwmToken>. These parameters are required to initiate a payment transaction.

```java
	@PostMapping("/submit")
	public PaymentInterfaceJson submit(
			@RequestParam(name = "acctnum", required = true) String acctNumber,
			@RequestParam(name = "amount", required = true) float amount,
			@RequestParam(name = "organisation", required = true) String organisation)
			throws JsonProcessingException
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" line="42">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="35:6:6" line-data="	@PostMapping(&quot;/submit&quot;)">`submit`</SwmToken> method logs the received parameters and creates a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="44:1:1" line-data="		TransferForm transferForm = new TransferForm(acctNumber, amount,">`TransferForm`</SwmToken> object with these values. This form is then serialized into a JSON string using the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="50:9:9" line-data="		String jsonString = new ObjectMapper().writeValueAsString(transferJson);">`ObjectMapper`</SwmToken> class.

```java
		log.info("AcctNumber: {}, Amount {}, Organisation {}", acctNumber,
				amount, organisation);
		TransferForm transferForm = new TransferForm(acctNumber, amount,
				organisation);

		PaymentInterfaceJson transferJson = new PaymentInterfaceJson(
				transferForm);

		String jsonString = new ObjectMapper().writeValueAsString(transferJson);
		log.info(jsonString);
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" line="53">

---

The JSON string is sent to the payment processing endpoint <SwmPath>[src/…/api/dbcr/](src/zosconnect_artefacts/apis/makepayment/api/dbcr/)</SwmPath> using the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="53:1:1" line-data="		WebClient client = WebClient.create(">`WebClient`</SwmToken> class. The response from this endpoint is then deserialized back into a <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/ParamsController.java" pos="55:1:1" line-data="		PaymentInterfaceJson responseObj;">`PaymentInterfaceJson`</SwmToken> object, which contains the details of the transaction.

```java
		WebClient client = WebClient.create(
				ConnectionInfo.getAddressAndPort() + "/makepayment/dbcr");
		PaymentInterfaceJson responseObj;

		try
		{
			ResponseSpec response = client.put()
					.header("content-type", "application/json")
					.accept(MediaType.APPLICATION_JSON)
					.body(BodyInserters.fromValue(jsonString)).retrieve();
			String responseBody = response.bodyToMono(String.class).block();
			log.info(responseBody);
			responseObj = new ObjectMapper().readValue(responseBody,
					PaymentInterfaceJson.class);
			log.info("{}", responseObj);
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
