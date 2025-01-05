---
title: Web UI Overview
---
# Web UI Overview

The Web UI refers to the web-based user interface of the banking sample application. It is designed to provide an interactive and user-friendly experience for bank tellers to perform various banking operations.

Built using modern web technologies and frameworks, the Web UI ensures a responsive and efficient interface. This design choice enhances the user experience by providing a seamless and intuitive interaction with the application.

The Web UI includes various static assets such as images and icons, which contribute to the visual appeal and usability of the interface. Additionally, essential configuration files like <SwmPath>[src/…/public/manifest.json](src/bank-application-frontend/public/manifest.json)</SwmPath> and <SwmPath>[src/…/WebContent/asset-manifest.json](src/webui/WebContent/asset-manifest.json)</SwmPath> are included to manage the application's metadata and asset caching.

<SwmSnippet path="/src/webui/WebContent/index.html" line="1">

---

The main entry point for the Web UI is the <SwmPath>[src/…/WebContent/index.html](src/webui/WebContent/index.html)</SwmPath> file. This file is responsible for loading the necessary resources and initializing the application, setting the stage for the user interface to function correctly.

```html
<!doctype html><html lang="en"><head><meta charset="utf-8"/><link rel="shortcut icon" href="/webui-1.0/favicon.ico"/><meta name="viewport" content="width=device-width,initial-scale=1,shrink-to-fit=no"/><meta name="theme-color" content="#000000"/><link rel="manifest" href="/webui-1.0/manifest.json"/><base href="/webui-1.0/"><title>CBSA</title><script defer="defer" src="/webui-1.0/static/js/main.43ee80df.js"></script><link href="/webui-1.0/static/css/main.27ea650d.css" rel="stylesheet"></head><body><noscript>You need to enable JavaScript to run this app.</noscript><div id="root"></div></body></html>
```

---

</SwmSnippet>

## Usage

The Web UI is located in the <SwmPath>[src/webui/WebContent/](src/webui/WebContent/)</SwmPath> directory. This directory contains various static assets and configuration files that are essential for the operation of the Web UI.

<SwmSnippet path="/src/webui/WebContent/index.html" line="1">

---

The <SwmPath>[src/…/WebContent/index.html](src/webui/WebContent/index.html)</SwmPath> file within this directory plays a crucial role in initializing the application by loading the necessary resources.

```html
<!doctype html><html lang="en"><head><meta charset="utf-8"/><link rel="shortcut icon" href="/webui-1.0/favicon.ico"/><meta name="viewport" content="width=device-width,initial-scale=1,shrink-to-fit=no"/><meta name="theme-color" content="#000000"/><link rel="manifest" href="/webui-1.0/manifest.json"/><base href="/webui-1.0/"><title>CBSA</title><script defer="defer" src="/webui-1.0/static/js/main.43ee80df.js"></script><link href="/webui-1.0/static/css/main.27ea650d.css" rel="stylesheet"></head><body><noscript>You need to enable JavaScript to run this app.</noscript><div id="root"></div></body></html>
```

---

</SwmSnippet>

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="87:5:5" line-data="	public Response getAccountCounter()">`getAccountCounter`</SwmToken>

# Web UI Endpoints

The Web UI interacts with various endpoints to perform its operations. These endpoints are defined in the backend and provide the necessary data and functionality for the Web UI.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="84">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="87:5:5" line-data="	public Response getAccountCounter()">`getAccountCounter`</SwmToken> endpoint is a GET request that retrieves the current account counter. This endpoint is defined in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="35:4:4" line-data="public class CounterResource extends HBankDataAccess">`CounterResource`</SwmToken> class and is mapped to the `/counter/account` path. It returns a JSON response containing the account counter value.

```java
	@GET
	@Path("/account")
	@Produces("application/json")
	public Response getAccountCounter()
	{
		logger.entering(this.getClass().getName(), GET_ACCOUNT_COUNTER);
		JSONObject response = new JSONObject();
		Response myResponse = null;

		Program newaccnoProgam = new Program();
		newaccnoProgam.setName(NEWACCNO);

		NewAccountNumber myNEWACCNO = new NewAccountNumber();

		myNEWACCNO.setNewaccnoFunction("C");
		byte[] data = myNEWACCNO.getByteBuffer();
		try
		{
			newaccnoProgam.link(data);
			myNEWACCNO = new NewAccountNumber(data);
			logger.fine(NEW_ACCNO_PREFIX + myNEWACCNO.getAccountNumber());
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="130">

---

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="133:5:5" line-data="	public Response getCustomerCounter()">`getCustomerCounter`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="133:5:5" line-data="	public Response getCustomerCounter()">`getCustomerCounter`</SwmToken> endpoint is a GET request that retrieves the current customer counter. This endpoint is defined in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="35:4:4" line-data="public class CounterResource extends HBankDataAccess">`CounterResource`</SwmToken> class and is mapped to the `/counter/customer` path. It returns a JSON response containing the customer counter value.

```java
	@GET
	@Path("/customer")
	@Produces("application/json")
	public Response getCustomerCounter()
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMER_COUNTER);

		JSONObject response = new JSONObject();
		Response myResponse = null;

		Program newcusnoProgram = new Program();
		newcusnoProgram.setName(NEWCUSNO);

		NewCustomerNumber myNEWCUSNO = new NewCustomerNumber();

		myNEWCUSNO.setNewcusnoFunction("C");
		byte[] data = myNEWCUSNO.getByteBuffer();
		try
		{
			newcusnoProgram.link(data);
			myNEWCUSNO = new NewCustomerNumber(data);
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
