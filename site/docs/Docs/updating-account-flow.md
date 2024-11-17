---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: 6bbe0w0g
---
# Updating account flow

## Introduction

This document will walk you through the implementation of the updating account feature.

The feature allows users to update account details through a web form, processes the input, and updates the account information in the backend system.

We will cover:

1. The structure of the update account form.
2. Handling form submission and validation.
3. Serializing form data to JSON.
4. Sending the update request to the backend.
5. Processing the backend response.
6. Backend service mapping and properties.
7. COBOL program updates.

## Update account form

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/resources/templates/updateAccountForm.html" line="25">

---

The form for updating account details is defined in `src/Z-OS-Connect-Customer-Services-Interface/src/main/resources/templates/updateAccountForm.html`. It includes fields for account number, account type, interest rate, and overdraft limit. The form uses Thymeleaf for data binding and error handling.

```
            <form class="bx--form-item form" action="/updateacc" th:action="@{/updateacc}"
                th:object="${updateAccountForm}" method="post">
                <table>
                    <tr>
                        <td>
                            <h1>Update Account</h1>
                        </td>
                    </tr>
                    <tr>
                        <td>Account number:</td>
                        <td><input class="bx--text-input" type="number" th:field="*{acctNumber}" /></td>
                        <td class="danger" th:if="${#fields.hasErrors('acctNumber')}" th:errors="*{custNumber}">Customer
                            Number Error</td>
                    </tr>
                    <tr>
                        <td>Account Type:</td>
                        <td>
                            <div th:each="type : ${accountTypes}">
                                <div class="">
                                    <!-- Would love to have Carbon Components radio buttons! but they disappear when you add the class for them -->
                                    <input type="radio" th:field="*{acctType}" th:value="${type}">
                                    <label class="" th:for="${#ids.prev('acctType')}" th:text="${type}">model</label>
                                </div>
                            </div>
                        </td>
                        <td class="danger" th:if="${#fields.hasErrors('acctType')}" th:errors="*{acctType}">Interest
                            Rate Error</td>
                    </tr>
                    <tr>
                        <td>Interest Rate:</td>
                        <td><input class="bx--text-input" type="number" th:field="*{acctInterestRate}" /></td>
                        <td class="danger" th:if="${#fields.hasErrors('acctInterestRate')}"
                            th:errors="*{acctInterestRate}">Interest Rate Error</td>
                    </tr>
                    <tr>
                        <td>Overdraft Limit:</td>
                        <td><input class="bx--text-input" type="number" th:field="*{acctOverdraft}" th:value="9" /></td>
                        <td class="danger" th:if="${#fields.hasErrors('acctOverdraft')}" th:errors="*{acctOverdraft}">
                            Overdraft Limit Error</td>
                    </tr>
                    <!-- These can't be updated at the moment but writing the code took effort so it's still here just in case they can be updated directly in the future -->
                    <!-- <tr>
                    <td>Available Balance:</td>
                    <td><input class="bx--text-input" type="number" th:field="*{acctAvailableBalance}" /></td>
                    <td class="danger" th:if="${#fields.hasErrors('acctAvailableBalance')}" th:errors="*{acctAvailableBalance}">Interest Rate Error</td>
                </tr>
                <tr>
                    <td>Actual Balance:</td>
                    <td><input class="bx--text-input" type="number" th:field="*{acctActualBalance}" /></td>
                    <td class="danger" th:if="${#fields.hasErrors('acctActualBalance')}" th:errors="*{acctActualBalance}">Interest Rate Error</td>
                </tr> -->
                    <tr>
                        <td><button class="bx--btn bx--btn--primary" type="submit">Submit</button></td>
                    </tr>
                </table>
            </form>
```

---

</SwmSnippet>

## Handling form submission and validation

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="587">

---

When the form is submitted, the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="588:5:5" line-data="	public String processCreateAcc(@Valid UpdateAccountForm updateAccountForm,">`processCreateAcc`</SwmToken> method in <SwmToken path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/controllers/WebController.java" pos="34:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> handles the POST request to <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="587:5:6" line-data="	@PostMapping(&quot;/updateacc&quot;)">`/updateacc`</SwmToken>. It validates the form data and checks for errors. If there are errors, it reloads the form with error messages and retains the account types for the radio buttons.

```
	@PostMapping("/updateacc")
	public String processCreateAcc(@Valid UpdateAccountForm updateAccountForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
		if (bindingResult.hasErrors())
		{

			// Must add the accountTypes enum here as well, otherwise the radio
			// buttons disappear on error
			model.addAttribute(ACCOUNT_TYPES, AccountType.values());
			return UPDATE_ACCOUNT_FORM;
		}
```

---

</SwmSnippet>

## Serializing form data to JSON

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="601">

---

If the form data is valid, it is converted into a <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="601:1:1" line-data="		UpdateAccountJson transferjson = new UpdateAccountJson(">`UpdateAccountJson`</SwmToken> object. This object is then serialized to a JSON string for transmission to the backend service.

```
		UpdateAccountJson transferjson = new UpdateAccountJson(
				updateAccountForm);

		// Serialise the object to JSON
		log.info("{}", transferjson);
		String jsonString = new ObjectMapper().writeValueAsString(transferjson);
		log.info("{}", jsonString);
```

---

</SwmSnippet>

## Sending the update request to the backend

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="608">

---

The JSON string is sent to the backend service using a <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="610:1:1" line-data="		WebClient client = WebClient">`WebClient`</SwmToken>. The request is a PUT request to the `src/zosconnect_artefacts/apis/updacc/api/update/` endpoint, and it expects a JSON response.

```

		// The port is set elsewhere as it changes frequently
		WebClient client = WebClient
				.create(ConnectionInfo.getAddressAndPort() + "/updacc/update");

		try
		{
			// Create a response object - body of json, accept json back, and
			// insert the
			// request body created a couple lines up
			ResponseSpec response = client.put()
					.header(CONTENT_TYPE, APPLICATION_JSON)
					.accept(MediaType.APPLICATION_JSON)
					.body(BodyInserters.fromValue(jsonString)).retrieve();
			String responseBody = response.bodyToMono(String.class).block();
			log.info(responseBody);
```

---

</SwmSnippet>

## Processing the backend response

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="624">

---

The response from the backend is deserialized into an <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="626:1:1" line-data="			UpdateAccountJson responseObj = new ObjectMapper()">`UpdateAccountJson`</SwmToken> object. The response is then validated, and if successful, relevant attributes are added to the model for display.

```

			// Deserialise into a POJO
			UpdateAccountJson responseObj = new ObjectMapper()
					.readValue(responseBody, UpdateAccountJson.class);
			log.info("{}", responseObj);

			// Throws out different exceptions depending on the contents
			checkIfResponseValidUpdateAcc(responseObj);

			// If successful...
			model.addAttribute(LARGE_TEXT, "");
			model.addAttribute(SMALL_TEXT, responseObj.toPrettyString());
```

---

</SwmSnippet>

## Backend service mapping and properties

<SwmSnippet path="/src/zosconnect_artefacts/apis/updacc/api/update/PUT/mapping.xml" line="1">

---

The backend service mapping is defined in `src/zosconnect_artefacts/apis/updacc/api/update/PUT/mapping.xml`. It specifies the endpoint, HTTP method, and response code.

```
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<!--  Copyright IBM Corp. 2023   -->
<ns2:ZosConnectServiceMapping xmlns:ns2="http://www.ibm.com/zosConnect/2.0/zosConnectServiceMapping" mappingSpecVersion="1.0">
    <mappingContext basePath="/updacc" relativePath="/update" method="PUT" serviceName="CSaccupd" defaultResponseCode="200"/>
    <responseMessages code="200" description="OK"/>
</ns2:ZosConnectServiceMapping>
```

---

</SwmSnippet>

<SwmSnippet path="/src/zosconnect_artefacts/services/CSaccupd/service.properties" line="8">

---

The service properties are defined in `src/zosconnect_artefacts/services/CSaccupd/service.properties`. These properties configure the service interface, executable name, and other settings.

```
responseSIName=UPDACC.si
bidiConfigRef=
servicetype=cicsCommarea
initializeInputFields=false
requestSIName=UPDACC.si
executableName=UPDACC
trimOutputTrailingWhitespace=true
```

---

</SwmSnippet>

## COBOL program updates

<SwmSnippet path="/src/base/cobol_src/UPDACC.cbl" line="158">

---

The COBOL program `src/base/cobol_src/UPDACC.cbl` is responsible for updating the account information in the database. It includes sections for moving data, performing the update, and finalizing the process.

```
       PREMIERE SECTION.
       A010.

           MOVE SORTCODE TO COMM-SCODE.
           MOVE SORTCODE TO DESIRED-SORT-CODE.

      *
      *           Update the account information
      *
           PERFORM UPDATE-ACCOUNT-DB2

      *
      *    The COMMAREA values have now been set so all we need to do
      *    is finish
      *

           PERFORM GET-ME-OUT-OF-HERE.
```

---

</SwmSnippet>

This concludes the walkthrough of the updating account flow feature. Each snippet plays a crucial role in ensuring the form data is correctly processed and the account information is updated in the backend system.

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
