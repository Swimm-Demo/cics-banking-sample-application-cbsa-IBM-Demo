---
title: Customer Deletion Page Component
---
# Introduction

This document will walk you through the implementation of the Customer Deletion Page Component in the bank application frontend.

The Customer Deletion Page allows administrators to search for customers by their customer number and view their details and associated accounts. If no customer is found, a modal is displayed to inform the user.

We will cover:

1. State management for user input and UI control.
2. Handling user input and form submission.
3. Fetching and displaying customer and account data.
4. UI structure and components.

# State management for user input and UI control

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="22">

---

We initialize several state variables to manage the component's behavior and data. These include states for the search input, customer and account details, and modal visibility.

```javascript
const CustomerDeletePage = () => {

  /**
   * States to store isOpened value of the table and the value the user has entered to search with
   */
  const [isOpened, setIsOpened] = useState(false);
  var [searchCustomerValue, setSearchCustomerValue] = useState("")
  const [customerDetailsRows, setRows] = useState([]);
  const [accountDetailsRows, setAccountRows] = useState([]);
  const [isNoResultsModalOpen, setIsNoResultsModalOpen] = useState(false)
```

---

</SwmSnippet>

# Handling user input and form submission

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="33">

---

We define functions to handle user input and form submission. The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="33:3:3" line-data="  function handleCustomerNumberInput(e){">`handleCustomerNumberInput`</SwmToken> function updates the search input state.

```javascript
  function handleCustomerNumberInput(e){
    setSearchCustomerValue(e.target.value)
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="45">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="45:5:5" line-data="  async function handleSubmitButtonClick(){">`handleSubmitButtonClick`</SwmToken> function triggers the search operation and toggles the display of the results.

```javascript
  async function handleSubmitButtonClick(){
    let searchQuery = searchCustomerValue;
    await getCustomerByNum(searchQuery)
    .then(display())
  }
```

---

</SwmSnippet>

# Fetching and displaying customer and account data

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="63">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="67:5:5" line-data="   async function getCustomerByNum(searchQuery) {">`getCustomerByNum`</SwmToken> function fetches customer data based on the search input. It formats the dates and updates the customer details state. If no customer is found, it displays a modal.

```javascript
   /**
    * Finds the customer using the customerNumber entered by the user and creates an array from the server response
    * customerDetailsRows' state is set to this array
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
             formattedDOB : formattedDOB,
             dateOfBirth: responseData.dateOfBirth,
             creditScore: responseData.customerCreditScore,
             formattedReviewDate : formattedReviewDate,
             nextReviewDate: responseData.customerCreditScoreReviewDate,
           };
           rowBuild.push(row);
           getAccountsForCustomers(row.id)
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="105">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="109:5:5" line-data="   async function getAccountsForCustomers(customerID) {">`getAccountsForCustomers`</SwmToken> function fetches account data for the given customer ID and updates the account details state.

```javascript
   /**
    * Finds the accounts for the given customer ID and creates an array from the server response.
    * accountDetailsRows' state is set to this array
    */
   async function getAccountsForCustomers(customerID) {
     let accountData;
     let rowBuild = [];
     await axios
       .get(process.env.REACT_APP_ACCOUNT_URL + `/retrieveByCustomerNumber/${customerID}`)
       .then(response => {
         accountData = response.data;
         try {
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
               lastStatementDate: account.lastStatementDate,
             };
             rowBuild.push(row)
           });
           setAccountRows(rowBuild)
         } catch (e) {
           console.log("Error fetching accounts for customer: " + customerID + ": " + e);
         }
```

---

</SwmSnippet>

# UI structure and components

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="143">

---

The component's UI is structured using Carbon Design System components. The breadcrumb navigation and page heading are defined first.

```javascript
  return (
    <Grid className="landing-page" fullWidth>
      <Column lg={16} md={8} sm={4} className="landing-page__banner">
        <Breadcrumb noTrailingSlash aria-label="Page navigation">
          <BreadcrumbItem>
            <a href="./">Home</a>
          </BreadcrumbItem>
          <BreadcrumbItem>
            <a href="./#/profile/Admin">Control Panel</a>
          </BreadcrumbItem>
          <BreadcrumbItem>Delete</BreadcrumbItem>
        </Breadcrumb>
        <h1 className="landing-page__heading">Customer Deletion</h1>
      </Column>
      <Column lg={16} md={8} sm={4} className="landing-page__r2">
        <div className="lower-content">
          <div class="cds--grid" style={{ marginLeft: '30px' }}>
            <div class="cds--row">
              <div class="cds--col">
                <div className="upper">
                  <div className="left-part">
                  <h5>Note: A customer cannot be deleted if they still have accounts associated with them</h5>
                    <NumberInput
                      className="customer-list-view"
                      label= 'Enter customer number'
                      min= "0"
                      invalidText= 'Please provide a valid number'
                      hideSteppers
                      allowEmpty
                      onChange={(e) => handleCustomerNumberInput(e)}
                    />
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="174">

---

The user input section includes a <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="165:2:2" line-data="                    &lt;NumberInput">`NumberInput`</SwmToken> for entering the customer number and a <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="175:2:2" line-data="                      &lt;Button type=&quot;submit&quot; onClick={handleSubmitButtonClick}&gt;">`Button`</SwmToken> to submit the form.

```javascript
                    <div style={{ marginTop: '20px' }}>
                      <Button type="submit" onClick={handleSubmitButtonClick}>
                        Submit
                      </Button>
                    </div>
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="187">

---

If the search results are available, the <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="190:2:2" line-data="                    &lt;CustomerDeleteTables ">`CustomerDeleteTables`</SwmToken> component is displayed with the customer and account details.

```javascript
                </div>
                {isOpened && (
                  <Column lg={16}>
                    <CustomerDeleteTables 
                    customerRow={customerDetailsRows} accountRow={accountDetailsRows}/>
                  </Column>
                )}
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="197">

---

A modal is displayed if no customer is found, informing the user to check the customer number.

```javascript
        </div>
      </Column>
      <Modal
        modalHeading="No customers found!"
        open={isNoResultsModalOpen}
        onRequestClose={displayNoResultsModal}
        danger
        passiveModal>
        <ModalBody hasForm>
          Please check that the customer number is correct
        </ModalBody>
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="213">

---

Finally, the component is exported for use in other parts of the application.

```javascript
export default CustomerDeletePage;
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
