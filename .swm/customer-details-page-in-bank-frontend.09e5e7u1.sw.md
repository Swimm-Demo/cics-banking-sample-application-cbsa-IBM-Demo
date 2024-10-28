---
title: Customer Details Page in Bank Frontend
---
# Overview

The Customer Details Page is a component in the Bank Frontend that allows users to view and manage customer information. It provides functionality to search for customers either by their customer number or by their name. The page displays a table of customer details, including customer number, sort code, name, address, date of birth, credit score, and next review date. Users can also view the accounts associated with a customer by expanding the customer row in the table. The page includes modals for handling cases where no customers are found and for updating customer details.

# States Management

The component uses various states to manage the visibility of tables and modals, as well as to store search inputs and customer data. It makes asynchronous calls to retrieve customer and account data from the backend services.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="23">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="23:2:2" line-data="const CustomerDetailsPage = () =&gt; {">`CustomerDetailsPage`</SwmToken> component initializes states for managing the visibility of tables and modals, as well as storing search inputs and customer data.

```javascript
const CustomerDetailsPage = () => {
  /**
   * States for table visibility and entered search values from the user
   */
  const [isOpened, setTableOpened] = useState(false);
  const [customerDetailsRows, setRows] = useState([]);
  const [accountDetailsRows, setAccountRows] = useState([]);
  const [noResultsOpened, setNoResultsOpened] = useState(false)
```

---

</SwmSnippet>

# Retrieving Customer Data

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="119:5:5" line-data="  async function getCustomerByNum(searchQuery) {">`getCustomerByNum`</SwmToken> function retrieves customer data by customer number and sets the state with the retrieved data.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="116">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="119:5:5" line-data="  async function getCustomerByNum(searchQuery) {">`getCustomerByNum`</SwmToken> function gets the customer from a given customer number, builds an array from the response, and sets the <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="117:34:34" line-data="   * Gets the customer from a given customerNum, builds an array from the response and sets customerDetailsRows&#39; state to this array">`customerDetailsRows`</SwmToken> state to this array.

```javascript
  /**
   * Gets the customer from a given customerNum, builds an array from the response and sets customerDetailsRows' state to this array
   */
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
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="72">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="75:5:5" line-data="  async function getCustomersByName(searchQuery) {">`getCustomersByName`</SwmToken> function gets the first 10 customers from a given name, builds an array from the response, and sets the <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="73:38:38" line-data="   * Gets the first 10 customers from a given name, builds an array from the response and sets customerDetailsRows&#39; state to this array">`customerDetailsRows`</SwmToken> state to this array.

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

# Retrieving Account Data

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="161:5:5" line-data="  async function getAccountsForCustomers(customerID) {">`getAccountsForCustomers`</SwmToken> function retrieves account data for a given customer and sets the state with the retrieved data.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="158">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="161:5:5" line-data="  async function getAccountsForCustomers(customerID) {">`getAccountsForCustomers`</SwmToken> function gets the accounts for a given customer ID, builds an array from the response, and sets the <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="159:34:34" line-data="   * Gets the accounts for a given customerID, builds an array from the response and sets accountDetailsRows&#39; state to this array">`accountDetailsRows`</SwmToken> state to this array.

```javascript
  /**
   * Gets the accounts for a given customerID, builds an array from the response and sets accountDetailsRows' state to this array
   */
  async function getAccountsForCustomers(customerID) {
    let accountData;
    let accountRowBuild = []
    await axios
      .get(process.env.REACT_APP_ACCOUNT_URL + `/retrieveByCustomerNumber/${customerID}`)
      .then(response => {
        accountData = response.data;
        let row;
        accountData.accounts.forEach(account => {
          row = {
            accountNumber: account.id,
            sortCode: account.sortCode,
            accountType: account.accountType,
            interestRate: account.interestRate,
            overdraft: account.overdraft,
            availableBalance: account.availableBalance,
            actualBalance: account.actualBalance,
            accountOpened: account.dateOpened,
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
