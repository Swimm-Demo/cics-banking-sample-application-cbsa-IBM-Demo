---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: t4tb2xrr
---
# Getting Started with Customer Details Page

## Getting Started with Customer Details Page

The Customer Details Page allows users to view and manage customer information. It provides a user interface for searching customers by their number or name. The page includes input fields for entering a customer's number or name, and a submit button to initiate the search. If a customer is found, their details are displayed in a table format. The page also handles cases where no customer is found by displaying a modal with an appropriate message. Additionally, it retrieves and displays account details associated with the customer.

### Navigating the Customer Details Page

To see customer details, click the "View customer details" button on the landing page. To amend customer details, click the "Update customer details" button on the landing page. To enquire about an account, click on "View account details" from the landing page. To display the accounts for a particular customer, click on "List accounts belonging to customer".

### <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="23:2:2" line-data="const CustomerDetailsPage = () =&gt; {">`CustomerDetailsPage`</SwmToken> Component

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="23:2:2" line-data="const CustomerDetailsPage = () =&gt; {">`CustomerDetailsPage`</SwmToken> component is defined in the file `src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js`. It includes input fields for entering a customer's number or name, and a submit button to initiate the search. If a customer is found, their details are displayed in a table format. The page also handles cases where no customer is found by displaying a modal with an appropriate message. Additionally, it retrieves and displays account details associated with the customer.

### Main Functions

There are several main functions in this component. Some of them are <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="35:3:3" line-data="  function handleNumInputChange(e) {">`handleNumInputChange`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="39:3:3" line-data="  function handleNameInputChange(e) {">`handleNameInputChange`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="43:3:3" line-data="  function displayNoResultsModal() {">`displayNoResultsModal`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="47:3:3" line-data="  function submitButtonHandler() {">`submitButtonHandler`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="55:1:1" line-data="      getCustomersByName(searchQuery)">`getCustomersByName`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="51:1:1" line-data="      getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken>, and <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="101:1:1" line-data="            getAccountsForCustomers(row.id)">`getAccountsForCustomers`</SwmToken>. We will dive a little into <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="47:3:3" line-data="  function submitButtonHandler() {">`submitButtonHandler`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="55:1:1" line-data="      getCustomersByName(searchQuery)">`getCustomersByName`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="51:1:1" line-data="      getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken>, and <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="101:1:1" line-data="            getAccountsForCustomers(row.id)">`getAccountsForCustomers`</SwmToken>.

#### <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="47:3:3" line-data="  function submitButtonHandler() {">`submitButtonHandler`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="47:3:3" line-data="  function submitButtonHandler() {">`submitButtonHandler`</SwmToken> function is triggered when the user clicks the submit button. It determines whether to search by customer number or name based on the input provided and calls the appropriate function (<SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="51:1:1" line-data="      getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken> or <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="55:1:1" line-data="      getCustomersByName(searchQuery)">`getCustomersByName`</SwmToken>). It also toggles the visibility of the results table.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="47">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="47:3:3" line-data="  function submitButtonHandler() {">`submitButtonHandler`</SwmToken> function determines the search query and calls the appropriate function to fetch customer details.

```javascript
  function submitButtonHandler() {
    let searchQuery;
    if (numSearch !== "") {
      searchQuery = numSearch
      getCustomerByNum(searchQuery)
    }
    else if (nameSearch !== "") {
      searchQuery = nameSearch
      getCustomersByName(searchQuery)
    }
    setTableOpened(wasOpened => !wasOpened)
  }
```

---

</SwmSnippet>

#### <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="55:1:1" line-data="      getCustomersByName(searchQuery)">`getCustomersByName`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="55:1:1" line-data="      getCustomersByName(searchQuery)">`getCustomersByName`</SwmToken> function sends a request to retrieve the first 10 customers matching the provided name. It formats the received data and updates the state with the customer details. If no customers are found, it displays a modal with an appropriate message.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="72">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="75:5:5" line-data="  async function getCustomersByName(searchQuery) {">`getCustomersByName`</SwmToken> function formats the received data and updates the state with the customer details.

```javascript
  /**
   * Gets the first 10 customers from a given name, builds an array from the response and sets customerDetailsRows' state to this array
   */
  async function getCustomersByName(searchQuery) {
    let responseData;
    let rowBuild = [];
    await axios
      .get(process.env.REACT_APP_CUSTOMER_URL + `/name?name=${searchQuery}&limit=10`)
      .then(response => {
        responseData = response.data;
        try {
          responseData.customers.forEach(customer => {
            let formattedDOB = getDay(customer.dateOfBirth) + "-" + getMonth(customer.dateOfBirth) + "-" + getYear(customer.dateOfBirth)
            let formattedReviewDate = getDay(customer.customerCreditScoreReviewDate) + "-" + getMonth(customer.customerCreditScoreReviewDate) +
            "-" + getYear(customer.customerCreditScoreReviewDate)
            let row;
            row = {
              id: parseInt(customer.id).toString(),
              customerNumber: parseInt(customer.id).toString(),
              sortCode: customer.sortCode,
              customerName: customer.customerName,
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="119">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="119:5:5" line-data="  async function getCustomerByNum(searchQuery) {">`getCustomerByNum`</SwmToken> function formats the received data and updates the state with the customer details.

```javascript
  async function getCustomerByNum(searchQuery) {
    let responseData;
    let rowBuild = [];
    await axios
      .get(process.env.REACT_APP_CUSTOMER_URL + `/${searchQuery}`)
      .then(response => {
        responseData = response.data;
        try {
          let row;
          let formattedDOB = getDay(responseData.dateOfBirth) + "-" + getMonth(responseData.dateOfBirth) + "-" + getYear(responseData.dateOfBirth)
          let formattedReviewDate = getDay(responseData.customerCreditScoreReviewDate) + "-" + getMonth(responseData.customerCreditScoreReviewDate) +
            "-" + getYear(responseData.customerCreditScoreReviewDate)
          row = {
            id: parseInt(responseData.id).toString(),
            customerNumber: parseInt(responseData.id).toString(),
            sortCode: responseData.sortCode,
            customerName: responseData.customerName,
            customerAddress: responseData.customerAddress,
            formattedDOB : formattedDOB,
            dateOfBirth: responseData.dateOfBirth,
            creditScore: responseData.customerCreditScore,
```

---

</SwmSnippet>

#### <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="101:1:1" line-data="            getAccountsForCustomers(row.id)">`getAccountsForCustomers`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="101:1:1" line-data="            getAccountsForCustomers(row.id)">`getAccountsForCustomers`</SwmToken> function sends a request to retrieve the accounts associated with a given customer ID. It formats the received data and updates the state with the account details.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
