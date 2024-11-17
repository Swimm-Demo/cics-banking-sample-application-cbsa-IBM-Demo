---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: 4ctk97hz
---
# Overview of Jsonclasses in Payment Interface

## What are Jsonclasses

Jsonclasses are a set of classes used to handle JSON data within the Payment Interface. They use Jackson annotations such as <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="30:1:2" line-data="	@JsonProperty(&quot;COMM_ORIGIN&quot;)">`@JsonProperty`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="10:0:1" line-data="@JsonNaming(JsonPropertyNamingStrategy.class)">`@JsonNaming`</SwmToken> to map JSON properties to Java fields, making it easier to serialize and deserialize JSON data.

## Where Jsonclasses are used

Jsonclasses are used in various parts of the Payment Interface, including the `PaymentInterfaceJson` class and its associated classes like <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="31:3:3" line-data="	private OriginJson commOrigin;">`OriginJson`</SwmToken>. These classes are used in controllers like `WebController` and `ParamsController` to handle payment-related requests and responses.

## Example of Jsonclasses usage

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken> class uses the <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="31:3:3" line-data="	private OriginJson commOrigin;">`OriginJson`</SwmToken> class to represent the origin of a transaction, demonstrating how Jsonclasses can be nested to represent complex JSON structures.

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" line="30">

---

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="11:4:4" line-data="public class DbcrJson">`DbcrJson`</SwmToken> class includes a field <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="31:5:5" line-data="	private OriginJson commOrigin;">`commOrigin`</SwmToken> which is an instance of <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="31:3:3" line-data="	private OriginJson commOrigin;">`OriginJson`</SwmToken>, annotated with <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="30:1:2" line-data="	@JsonProperty(&quot;COMM_ORIGIN&quot;)">`@JsonProperty`</SwmToken> to map the JSON property <SwmToken path="src/Z-OS-Connect-Payment-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/paymentinterface/jsonclasses/paymentinterface/DbcrJson.java" pos="30:5:5" line-data="	@JsonProperty(&quot;COMM_ORIGIN&quot;)">`COMM_ORIGIN`</SwmToken> to the Java field.

```java
	@JsonProperty("COMM_ORIGIN")
	private OriginJson commOrigin;
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
