---
title: The CUSTOMER class
---
This document will cover the class <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="139:3:3" line-data="	public CUSTOMER (byte[] buffer) {">`CUSTOMER`</SwmToken> in detail. We will cover:

1. What <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="139:3:3" line-data="	public CUSTOMER (byte[] buffer) {">`CUSTOMER`</SwmToken> is and its purpose.
2. The variables and functions defined in <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="139:3:3" line-data="	public CUSTOMER (byte[] buffer) {">`CUSTOMER`</SwmToken>.

# Variables and functions

# What is CUSTOMER

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="139:3:3" line-data="	public CUSTOMER (byte[] buffer) {">`CUSTOMER`</SwmToken> class in <SwmPath>[src/…/datainterfaces/CUSTOMER.java](src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java)</SwmPath> is a data interface class used to represent customer data in the banking application. It is designed to handle various customer-related information such as customer name, address, date of birth, credit score, and other relevant details. This class is essential for managing and manipulating customer data within the application.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="139">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="139:3:3" line-data="	public CUSTOMER (byte[] buffer) {">`CUSTOMER`</SwmToken> constructor initializes the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="140:3:3" line-data="		this.byteBuffer = buffer;">`byteBuffer`</SwmToken> with the provided buffer.

```java
	public CUSTOMER (byte[] buffer) {
		this.byteBuffer = buffer;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="143">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="143:3:3" line-data="	public CUSTOMER () {">`CUSTOMER`</SwmToken> default constructor initializes the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="144:3:3" line-data="		this.byteBuffer = new byte[OUTPUT_DATA_LEN];">`byteBuffer`</SwmToken> with a new byte array of length <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="144:11:11" line-data="		this.byteBuffer = new byte[OUTPUT_DATA_LEN];">`OUTPUT_DATA_LEN`</SwmToken>.

```java
	public CUSTOMER () {
		this.byteBuffer = new byte[OUTPUT_DATA_LEN];
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="147">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="147:7:7" line-data="	public byte[] getByteBuffer() {">`getByteBuffer`</SwmToken> function returns the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="148:3:3" line-data="		return byteBuffer;">`byteBuffer`</SwmToken>.

```java
	public byte[] getByteBuffer() {
		return byteBuffer;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="152">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="152:5:5" line-data="	public String getCustomerEyecatcher() {">`getCustomerEyecatcher`</SwmToken> function retrieves the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="153:4:4" line-data="		if (customerEyecatcher == null) {">`customerEyecatcher`</SwmToken> from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="154:9:9" line-data="			customerEyecatcher = CUSTOMER_EYECATCHER.getString(byteBuffer);">`byteBuffer`</SwmToken> if it is not already set.

```java
	public String getCustomerEyecatcher() {
		if (customerEyecatcher == null) {
			customerEyecatcher = CUSTOMER_EYECATCHER.getString(byteBuffer);
		}
		return customerEyecatcher;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="159">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="159:5:5" line-data="	public void setCustomerEyecatcher(String customerEyecatcher) {">`setCustomerEyecatcher`</SwmToken> function sets the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="159:9:9" line-data="	public void setCustomerEyecatcher(String customerEyecatcher) {">`customerEyecatcher`</SwmToken> in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="163:8:8" line-data="		CUSTOMER_EYECATCHER.putString(customerEyecatcher, byteBuffer);">`byteBuffer`</SwmToken> if it is different from the current value.

```java
	public void setCustomerEyecatcher(String customerEyecatcher) {
		if (CUSTOMER_EYECATCHER.equals(this.customerEyecatcher, customerEyecatcher)) {
			return;
		}
		CUSTOMER_EYECATCHER.putString(customerEyecatcher, byteBuffer);
		this.customerEyecatcher = customerEyecatcher;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="167">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="167:5:5" line-data="	public boolean isCustomerEyecatcherValue() {">`isCustomerEyecatcherValue`</SwmToken> function checks if the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="153:4:4" line-data="		if (customerEyecatcher == null) {">`customerEyecatcher`</SwmToken> equals the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="168:9:9" line-data="		return getCustomerEyecatcher().equals(CUSTOMER_EYECATCHER_VALUE);">`CUSTOMER_EYECATCHER_VALUE`</SwmToken>.

```java
	public boolean isCustomerEyecatcherValue() {
		return getCustomerEyecatcher().equals(CUSTOMER_EYECATCHER_VALUE);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="171">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="171:5:5" line-data="	public int getCustomerSortcode() {">`getCustomerSortcode`</SwmToken> function retrieves the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="173:1:1" line-data="			customerSortcode = CUSTOMER_SORTCODE.getInt(byteBuffer);">`customerSortcode`</SwmToken> from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="173:9:9" line-data="			customerSortcode = CUSTOMER_SORTCODE.getInt(byteBuffer);">`byteBuffer`</SwmToken> if it is not already set.

```java
	public int getCustomerSortcode() {
		if (!customerSortcodeIsSet) {
			customerSortcode = CUSTOMER_SORTCODE.getInt(byteBuffer);
			customerSortcodeIsSet = true;
		}
		return customerSortcode;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="179">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="179:5:5" line-data="	public void setCustomerSortcode(int customerSortcode) {">`setCustomerSortcode`</SwmToken> function sets the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="179:9:9" line-data="	public void setCustomerSortcode(int customerSortcode) {">`customerSortcode`</SwmToken> in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="183:8:8" line-data="		CUSTOMER_SORTCODE.putInt(customerSortcode, byteBuffer);">`byteBuffer`</SwmToken> if it is different from the current value.

```java
	public void setCustomerSortcode(int customerSortcode) {
		if (customerSortcodeIsSet && CUSTOMER_SORTCODE.equals(this.customerSortcode, customerSortcode)) {
			return;
		}
		CUSTOMER_SORTCODE.putInt(customerSortcode, byteBuffer);
		this.customerSortcode = customerSortcode;
		customerSortcodeIsSet = true;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="188">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="188:5:5" line-data="	public long getCustomerNumber() {">`getCustomerNumber`</SwmToken> function retrieves the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="190:1:1" line-data="			customerNumber = CUSTOMER_NUMBER.getLong(byteBuffer);">`customerNumber`</SwmToken> from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="190:9:9" line-data="			customerNumber = CUSTOMER_NUMBER.getLong(byteBuffer);">`byteBuffer`</SwmToken> if it is not already set.

```java
	public long getCustomerNumber() {
		if (!customerNumberIsSet) {
			customerNumber = CUSTOMER_NUMBER.getLong(byteBuffer);
			customerNumberIsSet = true;
		}
		return customerNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="196">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="196:5:5" line-data="	public void setCustomerNumber(long customerNumber) {">`setCustomerNumber`</SwmToken> function sets the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="196:9:9" line-data="	public void setCustomerNumber(long customerNumber) {">`customerNumber`</SwmToken> in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="200:8:8" line-data="		CUSTOMER_NUMBER.putLong(customerNumber, byteBuffer);">`byteBuffer`</SwmToken> if it is different from the current value.

```java
	public void setCustomerNumber(long customerNumber) {
		if (customerNumberIsSet && CUSTOMER_NUMBER.equals(this.customerNumber, customerNumber)) {
			return;
		}
		CUSTOMER_NUMBER.putLong(customerNumber, byteBuffer);
		this.customerNumber = customerNumber;
		customerNumberIsSet = true;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="205">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="205:5:5" line-data="	public String getCustomerName() {">`getCustomerName`</SwmToken> function retrieves the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="206:4:4" line-data="		if (customerName == null) {">`customerName`</SwmToken> from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="207:9:9" line-data="			customerName = CUSTOMER_NAME.getString(byteBuffer);">`byteBuffer`</SwmToken> if it is not already set.

```java
	public String getCustomerName() {
		if (customerName == null) {
			customerName = CUSTOMER_NAME.getString(byteBuffer);
		}
		return customerName;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="212">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="212:5:5" line-data="	public void setCustomerName(String customerName) {">`setCustomerName`</SwmToken> function sets the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="212:9:9" line-data="	public void setCustomerName(String customerName) {">`customerName`</SwmToken> in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="216:8:8" line-data="		CUSTOMER_NAME.putString(customerName, byteBuffer);">`byteBuffer`</SwmToken> if it is different from the current value.

```java
	public void setCustomerName(String customerName) {
		if (CUSTOMER_NAME.equals(this.customerName, customerName)) {
			return;
		}
		CUSTOMER_NAME.putString(customerName, byteBuffer);
		this.customerName = customerName;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="220">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="220:5:5" line-data="	public String getCustomerAddress() {">`getCustomerAddress`</SwmToken> function retrieves the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="221:4:4" line-data="		if (customerAddress == null) {">`customerAddress`</SwmToken> from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="222:9:9" line-data="			customerAddress = CUSTOMER_ADDRESS.getString(byteBuffer);">`byteBuffer`</SwmToken> if it is not already set.

```java
	public String getCustomerAddress() {
		if (customerAddress == null) {
			customerAddress = CUSTOMER_ADDRESS.getString(byteBuffer);
		}
		return customerAddress;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="227">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="227:5:5" line-data="	public void setCustomerAddress(String customerAddress) {">`setCustomerAddress`</SwmToken> function sets the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="227:9:9" line-data="	public void setCustomerAddress(String customerAddress) {">`customerAddress`</SwmToken> in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="231:8:8" line-data="		CUSTOMER_ADDRESS.putString(customerAddress, byteBuffer);">`byteBuffer`</SwmToken> if it is different from the current value.

```java
	public void setCustomerAddress(String customerAddress) {
		if (CUSTOMER_ADDRESS.equals(this.customerAddress, customerAddress)) {
			return;
		}
		CUSTOMER_ADDRESS.putString(customerAddress, byteBuffer);
		this.customerAddress = customerAddress;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="235">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="235:5:5" line-data="	public int getCustomerDateOfBirth() {">`getCustomerDateOfBirth`</SwmToken> function retrieves the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="239:9:9" line-data="	public void setCustomerDateOfBirth(int customerDateOfBirth) {">`customerDateOfBirth`</SwmToken> from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="236:7:7" line-data="		return CUSTOMER_DATE_OF_BIRTH.getInt(byteBuffer);">`byteBuffer`</SwmToken>.

```java
	public int getCustomerDateOfBirth() {
		return CUSTOMER_DATE_OF_BIRTH.getInt(byteBuffer);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="239">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="239:5:5" line-data="	public void setCustomerDateOfBirth(int customerDateOfBirth) {">`setCustomerDateOfBirth`</SwmToken> function sets the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="239:9:9" line-data="	public void setCustomerDateOfBirth(int customerDateOfBirth) {">`customerDateOfBirth`</SwmToken> in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="240:8:8" line-data="		CUSTOMER_DATE_OF_BIRTH.putInt(customerDateOfBirth, byteBuffer);">`byteBuffer`</SwmToken>.

```java
	public void setCustomerDateOfBirth(int customerDateOfBirth) {
		CUSTOMER_DATE_OF_BIRTH.putInt(customerDateOfBirth, byteBuffer);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="243">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="243:5:5" line-data="	public int getCustomerBirthDay() {">`getCustomerBirthDay`</SwmToken> function retrieves the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="247:9:9" line-data="	public void setCustomerBirthDay(int customerBirthDay) {">`customerBirthDay`</SwmToken> from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="244:7:7" line-data="		return CUSTOMER_BIRTH_DAY.getInt(byteBuffer);">`byteBuffer`</SwmToken>.

```java
	public int getCustomerBirthDay() {
		return CUSTOMER_BIRTH_DAY.getInt(byteBuffer);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="247">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="247:5:5" line-data="	public void setCustomerBirthDay(int customerBirthDay) {">`setCustomerBirthDay`</SwmToken> function sets the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="247:9:9" line-data="	public void setCustomerBirthDay(int customerBirthDay) {">`customerBirthDay`</SwmToken> in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="248:8:8" line-data="		CUSTOMER_BIRTH_DAY.putInt(customerBirthDay, byteBuffer);">`byteBuffer`</SwmToken>.

```java
	public void setCustomerBirthDay(int customerBirthDay) {
		CUSTOMER_BIRTH_DAY.putInt(customerBirthDay, byteBuffer);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="251">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="251:5:5" line-data="	public int getCustomerBirthMonth() {">`getCustomerBirthMonth`</SwmToken> function retrieves the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="255:9:9" line-data="	public void setCustomerBirthMonth(int customerBirthMonth) {">`customerBirthMonth`</SwmToken> from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="252:7:7" line-data="		return CUSTOMER_BIRTH_MONTH.getInt(byteBuffer);">`byteBuffer`</SwmToken>.

```java
	public int getCustomerBirthMonth() {
		return CUSTOMER_BIRTH_MONTH.getInt(byteBuffer);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" line="255">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="255:5:5" line-data="	public void setCustomerBirthMonth(int customerBirthMonth) {">`setCustomerBirthMonth`</SwmToken> function sets the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="255:9:9" line-data="	public void setCustomerBirthMonth(int customerBirthMonth) {">`customerBirthMonth`</SwmToken> in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CUSTOMER.java" pos="256:8:8" line-data="		CUSTOMER_BIRTH_MONTH.putInt(customerBirthMonth, byteBuffer);">`byteBuffer`</SwmToken>.

```java
	public void setCustomerBirthMonth(int customerBirthMonth) {
		CUSTOMER_BIRTH_MONTH.putInt(customerBirthMonth, byteBuffer);
	}
```

---

</SwmSnippet>

# Usage

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/PROCTRAN.java" line="160">

---

## PROCTRAN

In the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/PROCTRAN.java" pos="14:4:4" line-data="public class PROCTRAN {">`PROCTRAN`</SwmToken> class, the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/PROCTRAN.java" pos="161:11:11" line-data="	        88 PROC-TY-WEB-CREATE-CUSTOMER      VALUE &#39;ICC&#39;. &lt;/pre&gt; */">`CUSTOMER`</SwmToken> class is referenced through various constants that define different types of customer-related processes. For example, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/PROCTRAN.java" pos="162:9:9" line-data="	public static final String PROC_TY_WEB_CREATE_CUSTOMER = &quot;ICC&quot;;">`PROC_TY_WEB_CREATE_CUSTOMER`</SwmToken> is used to represent the process type for creating a customer via the web interface.

```java
	/** <pre>
	        88 PROC-TY-WEB-CREATE-CUSTOMER      VALUE 'ICC'. </pre> */
	public static final String PROC_TY_WEB_CREATE_CUSTOMER = "ICC";
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
