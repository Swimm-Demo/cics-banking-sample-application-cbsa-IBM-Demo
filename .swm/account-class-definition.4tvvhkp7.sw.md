---
title: Account Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="81:3:3" line-data="	public Account(String custNo, String sortCo, String accNo, String type,">`Account`</SwmToken> class definition in <SwmPath>[src/…/data_access/Account.java](src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java)</SwmPath>.

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="81:3:3" line-data="	public Account(String custNo, String sortCo, String accNo, String type,">`Account`</SwmToken> class is designed to manage account data and interact with the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="289:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken> for CRUD operations.

We will cover:

1. Class properties and their significance.
2. Constructors and their roles.
3. Methods for displaying and manipulating account data.
4. Interaction with <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="289:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken> for database operations.

# Class properties

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="29">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="81:3:3" line-data="	public Account(String custNo, String sortCo, String accNo, String type,">`Account`</SwmToken> class contains several properties that represent the details of a bank account. These properties are essential for storing and managing account information.

```java
	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.webui.data_access");

	private String customerNumber;

	private String sortcode;

	private String accountNumber;

	private String type;

	private BigDecimal interestRate;

	private Date opened;

	private int overdraftLimit;

	private Date lastStatement;

	private Date nextStatement;

	private BigDecimal availableBalance;

	private BigDecimal actualBalance;
```

---

</SwmSnippet>

# Constructors

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="81">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="81:3:3" line-data="	public Account(String custNo, String sortCo, String accNo, String type,">`Account`</SwmToken> class has multiple constructors to initialize account objects with different sets of data. This flexibility allows for creating account instances with varying levels of detail.

```java
	public Account(String custNo, String sortCo, String accNo, String type,
			BigDecimal intRate, Date opened, int overdraftL, Date lastStatement,
			Date nextStatement, BigDecimal avBal, BigDecimal acBal)
	{
		setCustomerNumber(custNo);
		setSortcode(sortCo);
		setAccountNumber(accNo);
		setType(type);
		setInterestRate(intRate);
		setOpened(opened);
		setOverdraftLimit(overdraftL);
		setLastStatement(lastStatement);
		setNextStatement(nextStatement);
		setAvailableBalance(avBal);
		setActualBalance(acBal);
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="99">

---

&nbsp;

```java
	public Account(String custNo, String sortCo, String accNo, String type,
			BigDecimal intRate, java.util.Date opened, int overdraftL,
			BigDecimal avBal, BigDecimal acBal)
	{
		setCustomerNumber(custNo);
		setSortcode(sortCo);
		setAccountNumber(accNo);
		setType(type);
		setInterestRate(intRate.setScale(2, RoundingMode.HALF_UP));
		setOpened(new Date(opened.getTime()));
		setOverdraftLimit(overdraftL);
		setLastStatement(null);
		setNextStatement(null);
		setAvailableBalance(avBal.setScale(2, RoundingMode.HALF_UP));
		setActualBalance(acBal.setScale(2, RoundingMode.HALF_UP));
	}
```

---

</SwmSnippet>

# Displaying account information

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="117">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="117:5:5" line-data="	public void showInfo()">`showInfo`</SwmToken> method logs the account details. This is useful for debugging and auditing purposes.

```java
	public void showInfo()
	{
		logger.log(Level.INFO, () -> "------------" + this.accountNumber + ":"
				+ this.sortcode + "------------");
		logger.log(Level.INFO,
				() -> "Customer number - " + this.customerNumber);
		logger.log(Level.INFO, () -> "Type - " + this.type);
		logger.log(Level.INFO, () -> "Interest rate - " + this.interestRate);
		logger.log(Level.INFO, () -> "Opened - " + this.opened.toString());
		logger.log(Level.INFO,
				() -> "Overdraft Limit - " + this.overdraftLimit);
		logger.log(Level.INFO,
				() -> "Last Statement - " + this.lastStatement.toString());
		logger.log(Level.INFO,
				() -> "Next Statement - " + this.nextStatement.toString());
		logger.log(Level.INFO,
				() -> ("Available Balance - " + this.availableBalance));
		logger.log(Level.INFO, () -> "Actual Balance - " + this.actualBalance);
	}
```

---

</SwmSnippet>

# Getters and setters

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="138">

---

The class provides getter and setter methods for each property. These methods ensure encapsulation and allow controlled access to the properties.

```java
	public String getCustomerNumber()
	{
		return customerNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="144">

---

&nbsp;

```java
	public void setCustomerNumber(String custNo)
	{
		this.customerNumber = custNo;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="150">

---

&nbsp;

```java
	public String getSortcode()
	{
		return sortcode;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="156">

---

&nbsp;

```java
	public void setSortcode(String sortcode)
	{
		this.sortcode = sortcode;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="162">

---

&nbsp;

```java
	public String getAccountNumber()
	{
		return accountNumber;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="168">

---

&nbsp;

```java
	public void setAccountNumber(String accNo)
	{
		this.accountNumber = accNo;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="174">

---

&nbsp;

```java
	public String getType()
	{
		return type;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="180">

---

&nbsp;

```java
	public void setType(String type)
	{
		this.type = type;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="186">

---

&nbsp;

```java
	public BigDecimal getInterestRate()
	{
		return interestRate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="192">

---

&nbsp;

```java
	public void setInterestRate(BigDecimal intRate)
	{
		this.interestRate = intRate;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="198">

---

&nbsp;

```java
	public Date getOpened()
	{
		return opened;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="204">

---

&nbsp;

```java
	public void setOpened(Date opened)
	{
		this.opened = opened;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="210">

---

&nbsp;

```java
	public int getOverdraftLimit()
	{
		return overdraftLimit;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="216">

---

&nbsp;

```java
	public void setOverdraftLimit(int overdraft)
	{
		this.overdraftLimit = overdraft;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="222">

---

&nbsp;

```java
	public Date getLastStatement()
	{
		return lastStatement;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="228">

---

&nbsp;

```java
	public void setLastStatement(Date last)
	{
		if (last == null)
		{
			this.lastStatement = this.opened;
		}
		else
		{
			this.lastStatement = last;
		}
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="241">

---

&nbsp;

```java
	public Date getNextStatement()
	{
		return nextStatement;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="247">

---

&nbsp;

```java
	public void setNextStatement(Date next)
	{
		if (next == null)
		{
			Calendar cal = Calendar.getInstance();
			cal.setTime(opened);
			cal.add(Calendar.DATE, +7);
			this.nextStatement = new Date(cal.getTime().getTime());
		}
		else
		{
			this.nextStatement = next;
		}
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="263">

---

&nbsp;

```java
	public BigDecimal getAvailableBalance()
	{
		return availableBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="269">

---

&nbsp;

```java
	public void setAvailableBalance(BigDecimal availableBalance)
	{
		this.availableBalance = availableBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="275">

---

&nbsp;

```java
	public BigDecimal getActualBalance()
	{
		return actualBalance;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="281">

---

&nbsp;

```java
	public void setActualBalance(BigDecimal actualBalance)
	{
		this.actualBalance = actualBalance;
	}
```

---

</SwmSnippet>

# Updating account data

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="287">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="287:5:5" line-data="	public boolean updateThis()">`updateThis`</SwmToken> method updates the account data by interacting with the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="289:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken>. It sends the updated account details and processes the response.

```java
	public boolean updateThis()
	{
		AccountsResource myAccountsResource = new AccountsResource();

		AccountJSON myAccountJSON = new AccountJSON();

		myAccountJSON.setAccountType(this.getType());
		myAccountJSON.setInterestRate(this.getInterestRate());
		myAccountJSON.setOverdraft(this.getOverdraftLimit());
		myAccountJSON.setSortCode(this.getSortcode());

		Response myAccountsResponse = myAccountsResource.updateAccountInternal(
				Long.parseLong(this.getAccountNumber()), myAccountJSON);

		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		if (myAccountsResponse.getStatus() == 200)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try
			{
				myAccountsJSON = JSONObject.parse(myAccountsString);
			}
			catch (IOException e)
			{
				logger.severe(e.toString());
				return false;
			}

			JSONObject myAccount = myAccountsJSON;

			this.lastStatement = sortOutDate(
					(String) myAccount.get(JSON_LAST_STATEMENT_DATE));
			this.nextStatement = sortOutDate(
					(String) myAccount.get(JSON_NEXT_STATEMENT_DATE));
			this.opened = sortOutDate((String) myAccount.get(JSON_DATE_OPENED));

			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount
					.get(JSON_CUSTOMER_NUMBER);

			this.accountNumber = accountNoString;
			this.customerNumber = customerNoString;

			this.actualBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_ACTUAL_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.availableBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_AVAILABLE_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.interestRate = BigDecimal
					.valueOf((Double) myAccount.get(JSON_INTEREST_RATE))
					.setScale(2, RoundingMode.HALF_UP);

			Long myLong = (Long) myAccount.get(JSON_OVERDRAFT);

			this.overdraftLimit = myLong.intValue();

			this.sortcode = (String) myAccount.get(JSON_SORT_CODE);
			this.type = (String) myAccount.get(JSON_ACCOUNT_TYPE);
		}
		else
		{
			return false;
		}
		return true;
	}
```

---

</SwmSnippet>

# Deleting account data

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="358">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="358:5:5" line-data="	public boolean deleteFromDB()">`deleteFromDB`</SwmToken> method deletes the account data from the database by interacting with the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="360:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken>. It processes the response to ensure the deletion was successful.

```java
	public boolean deleteFromDB()
	{
		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		myAccountsResponse = myAccountsResource
				.deleteAccountInternal(Long.parseLong(this.accountNumber));
		if (myAccountsResponse.getStatus() == 200)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try
			{
				myAccountsJSON = JSONObject.parse(myAccountsString);
			}
			catch (IOException e)
			{

				logger.severe(e.toString());
				return false;
			}

			JSONObject myAccount = myAccountsJSON;

			this.lastStatement = sortOutDate(
					(String) myAccount.get(JSON_LAST_STATEMENT_DATE));
			this.nextStatement = sortOutDate(
					(String) myAccount.get(JSON_NEXT_STATEMENT_DATE));
			this.opened = sortOutDate((String) myAccount.get(JSON_DATE_OPENED));

			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount
					.get(JSON_CUSTOMER_NUMBER);

			this.accountNumber = accountNoString;
			this.customerNumber = customerNoString;

			this.actualBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_ACTUAL_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.availableBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_AVAILABLE_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.interestRate = BigDecimal
					.valueOf((Double) myAccount.get(JSON_INTEREST_RATE))
					.setScale(2, RoundingMode.HALF_UP);

			Long myLong = (Long) myAccount.get(JSON_OVERDRAFT);

			this.overdraftLimit = myLong.intValue();
			
			this.sortcode = (String) myAccount.get(JSON_SORT_CODE);
			this.type = (String) myAccount.get(JSON_ACCOUNT_TYPE);
			return true;
		}
		return false;
	}
```

---

</SwmSnippet>

# Adding account data

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="420">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="420:5:5" line-data="	public int addToDB()">`addToDB`</SwmToken> method adds new account data to the database by interacting with the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="422:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken>. It processes the response to ensure the addition was successful.

```java
	public int addToDB()
	{
		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		AccountJSON myAccountJSON = new AccountJSON();

		myAccountJSON.setAccountType(this.getType());
		myAccountJSON.setInterestRate(
				this.getInterestRate().setScale(2, RoundingMode.HALF_UP));
		myAccountJSON.setOverdraft(Integer.valueOf(this.getOverdraftLimit()));
		myAccountJSON.setCustomerNumber(this.getCustomerNumber());
		myAccountJSON.setSortCode(this.getSortcode());

		myAccountsResponse = myAccountsResource
				.createAccountInternal(myAccountJSON);

		if (myAccountsResponse.getStatus() == 201)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try
			{
				myAccountsJSON = JSONObject.parse(myAccountsString);
			}
			catch (IOException e)
			{
				logger.severe(e.toString());
				myAccountsResponse.close();
				return -1;
			}

			JSONObject myAccount = myAccountsJSON;

			this.lastStatement = sortOutDate(
					(String) myAccount.get(JSON_LAST_STATEMENT_DATE));
			this.nextStatement = sortOutDate(
					(String) myAccount.get(JSON_NEXT_STATEMENT_DATE));
			this.opened = sortOutDate((String) myAccount.get(JSON_DATE_OPENED));

			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount
					.get(JSON_CUSTOMER_NUMBER);

			this.accountNumber = accountNoString;
			this.customerNumber = customerNoString;

			this.actualBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_ACTUAL_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.availableBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_AVAILABLE_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.interestRate = BigDecimal
					.valueOf((Double) myAccount.get(JSON_INTEREST_RATE))
					.setScale(2, RoundingMode.HALF_UP);

			Long myLong = (Long) myAccount.get(JSON_OVERDRAFT);

			this.overdraftLimit = myLong.intValue();

			this.sortcode = (String) myAccount.get(JSON_SORT_CODE);
			this.type = (String) myAccount.get(JSON_ACCOUNT_TYPE);
			myAccountsResponse.close();
			return Integer.parseInt(this.accountNumber);
		}
		myAccountsResponse.close();
		return -1;
	}
```

---

</SwmSnippet>

# Checking if account exists in the database

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="494">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="494:5:5" line-data="	public boolean inDB()">`inDB`</SwmToken> method checks if the account exists in the database by interacting with the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="496:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken>. It processes the response to determine the existence of the account.

```java
	public boolean inDB()
	{
		AccountsResource myAccountsResource = new AccountsResource();

		Response myAccountsResponse = null;
		String myAccountsString = null;
		JSONObject myAccountsJSON = null;

		myAccountsResponse = myAccountsResource
				.getAccountInternal(Long.parseLong(this.accountNumber));
		if (myAccountsResponse.getStatus() == 200)
		{
			myAccountsString = myAccountsResponse.getEntity().toString();
			try
			{
				myAccountsJSON = JSONObject.parse(myAccountsString);
			}
			catch (IOException e)
			{
				logger.severe(e.toString());
				myAccountsResponse.close();
				return false;
			}

			JSONObject myAccount = myAccountsJSON;

			this.lastStatement = sortOutDate(
					(String) myAccount.get(JSON_LAST_STATEMENT_DATE));
			this.nextStatement = sortOutDate(
					(String) myAccount.get(JSON_NEXT_STATEMENT_DATE));
			this.opened = sortOutDate((String) myAccount.get(JSON_DATE_OPENED));

			String accountNoString = (String) myAccount.get("id");

			String customerNoString = (String) myAccount
					.get(JSON_CUSTOMER_NUMBER);

			this.accountNumber = accountNoString;
			this.customerNumber = customerNoString;

			this.actualBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_ACTUAL_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.availableBalance = BigDecimal
					.valueOf((Double) myAccount.get(JSON_AVAILABLE_BALANCE))
					.setScale(2, RoundingMode.HALF_UP);
			this.interestRate = BigDecimal
					.valueOf((Double) myAccount.get(JSON_INTEREST_RATE))
					.setScale(2, RoundingMode.HALF_UP);

			Long myLong = (Long) myAccount.get(JSON_OVERDRAFT);
			this.overdraftLimit = myLong.intValue();
			
			this.sortcode = (String) myAccount.get(JSON_SORT_CODE);
			this.type = (String) myAccount.get(JSON_ACCOUNT_TYPE);
			myAccountsResponse.close();
			return true;
		}
		myAccountsResponse.close();
		return false;

	}
```

---

</SwmSnippet>

# Utility methods

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="558">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="558:5:5" line-data="	private Date sortOutDate(String dateString)">`sortOutDate`</SwmToken> method converts a date string into a <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="558:3:3" line-data="	private Date sortOutDate(String dateString)">`Date`</SwmToken> object. This is useful for handling date formats consistently.

```java
	private Date sortOutDate(String dateString)
	{
		String[] dateArray = dateString.split("-");

		Integer year = Integer.parseInt(dateArray[0]);
		Integer month = Integer.parseInt(dateArray[1]);
		Integer day = Integer.parseInt(dateArray[2]);

		Calendar myCalendar = Calendar.getInstance();
		myCalendar.set(Calendar.YEAR, year);
		myCalendar.set(Calendar.MONTH, month - 1);
		myCalendar.set(Calendar.DATE, day);
		return new Date(myCalendar.getTimeInMillis());
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" line="574">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="574:7:7" line-data="	private static void sortOutLogging()">`sortOutLogging`</SwmToken> method configures the logging settings. This ensures that logging is set up correctly for the class.

```java
	private static void sortOutLogging()
	{
		try
		{
			LogManager.getLogManager().readConfiguration();
		}
		catch (SecurityException | IOException e)
		{
			logger.severe(e.toString());
		}
	}
}
```

---

</SwmSnippet>

This document has covered the main aspects of the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="81:3:3" line-data="	public Account(String custNo, String sortCo, String accNo, String type,">`Account`</SwmToken> class, including its properties, constructors, methods for displaying and manipulating account data, and interactions with the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/webui/data_access/Account.java" pos="289:1:1" line-data="		AccountsResource myAccountsResource = new AccountsResource();">`AccountsResource`</SwmToken> for database operations.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
