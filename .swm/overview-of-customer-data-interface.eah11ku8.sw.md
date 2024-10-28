---
title: Overview of Customer Data Interface
---
# What is Customer Data Interface

The Customer Data Interface in the CICS Bank Sample Application (CBSA) is represented by the `Crecust` class. This class is responsible for handling customer data within the application. It includes fields for various customer attributes such as name, address, date of birth, and credit score.

The `Crecust` class uses a <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="14:5:5" line-data="	protected static CobolDatatypeFactory factory = new CobolDatatypeFactory();">`CobolDatatypeFactory`</SwmToken> to manage COBOL data types and handle the byte buffer that stores the customer data. This allows for efficient storage and retrieval of customer information.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CreditScoreCICS540.java" line="89">

---

## Usage of Crecust in <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CreditScoreCICS540.java" pos="43:4:4" line-data="public class CreditScoreCICS540">`CreditScoreCICS540`</SwmToken>

The `Crecust` class is instantiated and the customer address is set using the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CreditScoreCICS540.java" pos="90:3:3" line-data="				myCRECUST.setCommAddress(customer.getCustomerAddress());">`setCommAddress`</SwmToken> method.

```java
				CRECUST myCRECUST = new CRECUST();
				myCRECUST.setCommAddress(customer.getCustomerAddress());
```

---

</SwmSnippet>

## Main Functions

The `Crecust` class provides several main functions to interact with customer data. These include <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="192:5:5" line-data="	public String getCommName() {">`getCommName`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="199:5:5" line-data="	public void setCommName(String commName) {">`setCommName`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="207:5:5" line-data="	public String getCommAddress() {">`getCommAddress`</SwmToken>, and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CreditScoreCICS540.java" pos="90:3:3" line-data="				myCRECUST.setCommAddress(customer.getCustomerAddress());">`setCommAddress`</SwmToken>. Each function is designed to retrieve or set specific customer attributes within the byte buffer.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" line="192">

---

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="192:5:5" line-data="	public String getCommName() {">`getCommName`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="192:5:5" line-data="	public String getCommName() {">`getCommName`</SwmToken> function retrieves the customer's name from the byte buffer. If the name is not already cached, it extracts the name from the buffer and caches it for future use.

```java
	public String getCommName() {
		if (commName == null) {
			commName = COMM_NAME.getString(byteBuffer);
		}
		return commName;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" line="199">

---

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="199:5:5" line-data="	public void setCommName(String commName) {">`setCommName`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="199:5:5" line-data="	public void setCommName(String commName) {">`setCommName`</SwmToken> function sets the customer's name in the byte buffer. It first checks if the new name is different from the current cached name. If it is, it updates the byte buffer and caches the new name.

```java
	public void setCommName(String commName) {
		if (COMM_NAME.equals(this.commName, commName)) {
			return;
		}
		COMM_NAME.putString(commName, byteBuffer);
		this.commName = commName;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" line="207">

---

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="207:5:5" line-data="	public String getCommAddress() {">`getCommAddress`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="207:5:5" line-data="	public String getCommAddress() {">`getCommAddress`</SwmToken> function retrieves the customer's address from the byte buffer. Similar to <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="192:5:5" line-data="	public String getCommName() {">`getCommName`</SwmToken>, it caches the address if it is not already cached.

```java
	public String getCommAddress() {
		if (commAddress == null) {
			commAddress = COMM_ADDRESS.getString(byteBuffer);
		}
		return commAddress;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" line="214">

---

### <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="214:5:5" line-data="	public void setCommAddress(String commAddress) {">`setCommAddress`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CRECUST.java" pos="214:5:5" line-data="	public void setCommAddress(String commAddress) {">`setCommAddress`</SwmToken> function sets the customer's address in the byte buffer. It updates the byte buffer and caches the new address if it is different from the current cached address.

```java
	public void setCommAddress(String commAddress) {
		if (COMM_ADDRESS.equals(this.commAddress, commAddress)) {
			return;
		}
		COMM_ADDRESS.putString(commAddress, byteBuffer);
		this.commAddress = commAddress;
	}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
