---
title: Exploring Customer Management
---
# Customer Control Overview

Customer control refers to the management and handling of customer-related data and operations. It includes functionalities such as creating a new customer, viewing customer details, and managing customer records.

# How to Use Customer Control

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="688:1:1" line-data="		CustomerControl myCustomerControl = new CustomerControl();">`CustomerControl`</SwmToken> class in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="20:12:12" line-data="import com.ibm.cics.cip.bankliberty.datainterfaces.CUSTOMER;">`datainterfaces`</SwmToken> package is responsible for defining and managing customer data fields and operations. This class includes fields for customer control attributes like customer number, sort code, number of customers, and success flags. It also provides methods to get and set these attributes, ensuring proper handling and validation of customer data.

# Where Customer Control is Used

Customer control is used in various parts of the application, including the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="48:4:4" line-data="public class Customer">`Customer`</SwmToken> class in the <SwmPath>[src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/](src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/)</SwmPath> package.

# Example of Customer Control Usage

An example of customer control usage is in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="48:4:4" line-data="public class Customer">`Customer`</SwmToken> class where it creates a new <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="688:1:1" line-data="		CustomerControl myCustomerControl = new CustomerControl();">`CustomerControl`</SwmToken> instance and retrieves the last customer number.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" line="687">

---

This code snippet shows how a new <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="688:1:1" line-data="		CustomerControl myCustomerControl = new CustomerControl();">`CustomerControl`</SwmToken> instance is created and used to set customer control attributes and read customer data.

```java
	{
		CustomerControl myCustomerControl = new CustomerControl();
		customerFile.setName(FILENAME);

		myCustomerControl.setCustomerControlSortcode(0);
		myCustomerControl.setCustomerControlNumber(9999999999L);

		byte[] key = LAST_CUSTOMER.getBytes();

		holder = new RecordHolder();

		try
		{
			customerFile.readForUpdate(key, holder);
		}
		catch (LogicException | InvalidRequestException | IOErrorException
				| InvalidSystemIdException | LockedException | ChangedException
				| LoadingException | RecordBusyException | FileDisabledException
				| DuplicateKeyException | FileNotFoundException
				| ISCInvalidRequestException | NotAuthorisedException
				| RecordNotFoundException | NotOpenException e)
```

---

</SwmSnippet>

# Main Functions

There are several main functions in the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="688:1:1" line-data="		CustomerControl myCustomerControl = new CustomerControl();">`CustomerControl`</SwmToken> class. Some of them are <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="281:5:5" line-data="	public long getCustomerControlNumber()">`getCustomerControlNumber`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="692:3:3" line-data="		myCustomerControl.setCustomerControlNumber(9999999999L);">`setCustomerControlNumber`</SwmToken>, <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="305:5:5" line-data="	public long getNumberOfCustomers()">`getNumberOfCustomers`</SwmToken>, and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="316:5:5" line-data="	public void setNumberOfCustomers(long numberOfCustomers)">`setNumberOfCustomers`</SwmToken>. We will dive a little into <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="281:5:5" line-data="	public long getCustomerControlNumber()">`getCustomerControlNumber`</SwmToken> and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="692:3:3" line-data="		myCustomerControl.setCustomerControlNumber(9999999999L);">`setCustomerControlNumber`</SwmToken>.

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="281:5:5" line-data="	public long getCustomerControlNumber()">`getCustomerControlNumber`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="281:5:5" line-data="	public long getCustomerControlNumber()">`getCustomerControlNumber`</SwmToken> function retrieves the customer control number from the byte buffer. It ensures that the customer control number is set before returning it.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" line="281">

---

This code snippet shows the implementation of the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="281:5:5" line-data="	public long getCustomerControlNumber()">`getCustomerControlNumber`</SwmToken> function.

```java
	public long getCustomerControlNumber()
	{
		if (!customerControlNumberIsSet)
		{
			customerControlNumber = CUSTOMER_CONTROL_NUMBER.getLong(byteBuffer);
			customerControlNumberIsSet = true;
		}
		return customerControlNumber;
	}
```

---

</SwmSnippet>

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="692:3:3" line-data="		myCustomerControl.setCustomerControlNumber(9999999999L);">`setCustomerControlNumber`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/web/vsam/Customer.java" pos="692:3:3" line-data="		myCustomerControl.setCustomerControlNumber(9999999999L);">`setCustomerControlNumber`</SwmToken> function sets the customer control number in the byte buffer. It ensures that the new value is different from the current value before updating it.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" line="292">

---

This code snippet shows the implementation of the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="292:5:5" line-data="	public void setCustomerControlNumber(long customerControlNumber)">`setCustomerControlNumber`</SwmToken> function.

```java
	public void setCustomerControlNumber(long customerControlNumber)
	{
		if (customerControlNumberIsSet && CUSTOMER_CONTROL_NUMBER
				.equals(this.customerControlNumber, customerControlNumber))
		{
			return;
		}
		CUSTOMER_CONTROL_NUMBER.putLong(customerControlNumber, byteBuffer);
		this.customerControlNumber = customerControlNumber;
		customerControlNumberIsSet = true;
	}
```

---

</SwmSnippet>

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="305:5:5" line-data="	public long getNumberOfCustomers()">`getNumberOfCustomers`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="305:5:5" line-data="	public long getNumberOfCustomers()">`getNumberOfCustomers`</SwmToken> function retrieves the number of customers from the byte buffer. It ensures that the number of customers is set before returning it.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" line="305">

---

This code snippet shows the implementation of the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="305:5:5" line-data="	public long getNumberOfCustomers()">`getNumberOfCustomers`</SwmToken> function.

```java
	public long getNumberOfCustomers()
	{
		if (!numberOfCustomersIsSet)
		{
			numberOfCustomers = NUMBER_OF_CUSTOMERS.getLong(byteBuffer);
			numberOfCustomersIsSet = true;
		}
		return numberOfCustomers;
	}
```

---

</SwmSnippet>

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="316:5:5" line-data="	public void setNumberOfCustomers(long numberOfCustomers)">`setNumberOfCustomers`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="316:5:5" line-data="	public void setNumberOfCustomers(long numberOfCustomers)">`setNumberOfCustomers`</SwmToken> function sets the number of customers in the byte buffer. It ensures that the new value is different from the current value before updating it.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" line="316">

---

This code snippet shows the implementation of the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/datainterfaces/CustomerControl.java" pos="316:5:5" line-data="	public void setNumberOfCustomers(long numberOfCustomers)">`setNumberOfCustomers`</SwmToken> function.

```java
	public void setNumberOfCustomers(long numberOfCustomers)
	{
		if (numberOfCustomersIsSet && NUMBER_OF_CUSTOMERS
				.equals(this.numberOfCustomers, numberOfCustomers))
		{
			return;
		}
		NUMBER_OF_CUSTOMERS.putLong(numberOfCustomers, byteBuffer);
		this.numberOfCustomers = numberOfCustomers;
		numberOfCustomersIsSet = true;
	}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
