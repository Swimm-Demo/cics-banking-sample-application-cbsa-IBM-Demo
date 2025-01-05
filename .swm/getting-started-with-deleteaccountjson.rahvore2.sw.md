---
title: Getting Started with DeleteAccountJson
---
# delaccCommarea Field

# Introduction

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:1:1" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`DeleteAccountJson`</SwmToken> class is designed to represent the JSON structure required for deleting a bank account. This class is a crucial part of the application, enabling the deletion of account details in a structured and consistent manner.

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:1:1" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`DeleteAccountJson`</SwmToken> class contains a field named `delaccCommarea`, which is an instance of the `DelaccJson` class. This field holds various details about the account that needs to be deleted.

# Getter and Setter Methods

The `delaccCommarea` field includes information such as the account number, sort code, account type, customer number, interest rate, overdraft limit, available balance, actual balance, account opened date, last statement date, and next statement date.

# <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="192:7:7" line-data="				log.info(e.toString());">`toString`</SwmToken> Method

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:1:1" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`DeleteAccountJson`</SwmToken> class provides getter and setter methods for the `delaccCommarea` field. These methods allow for accessing and modifying the details of the account to be deleted.

# Usage in <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="192:7:7" line-data="				log.info(e.toString());">`toString`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:1:1" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`DeleteAccountJson`</SwmToken> class returns a string representation of the object. This representation includes the details of the `delaccCommarea` field, making it easier to log and debug the account deletion process.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="805">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="806:1:1" line-data="				DeleteAccountJson responseObj = new ObjectMapper()">`DeleteAccountJson`</SwmToken> class is utilized in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class to read and log the response body. This ensures that the account deletion process is properly tracked and recorded.

```java
				log.info(responseBody);
				DeleteAccountJson responseObj = new ObjectMapper()
						.readValue(responseBody, DeleteAccountJson.class);
				log.info("{}", responseObj);
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
