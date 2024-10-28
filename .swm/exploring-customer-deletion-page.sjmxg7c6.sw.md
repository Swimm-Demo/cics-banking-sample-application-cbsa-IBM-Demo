---
title: Exploring Customer Deletion Page
---
# Overview

The Customer Delete Page is a part of the Bank Frontend that allows users to remove customer records from the system. It provides a user-friendly interface for bank tellers to manage customer data efficiently.

# How to Use the Customer Delete Page

To delete a customer, the user must enter the customer number and submit the request. The page retrieves the customer details and associated accounts using RESTful API calls. If the customer has no associated accounts, the deletion process proceeds, and the customer is removed from the database. If the customer has associated accounts, the user is notified, and the deletion process is halted. The page provides feedback to the user, such as confirmation messages for successful deletions or error messages if the deletion fails.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="45">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="45:5:5" line-data="  async function handleSubmitButtonClick(){">`handleSubmitButtonClick`</SwmToken> function handles the submission of the customer number for deletion. It retrieves the customer details and toggles the display state.

```javascript
  async function handleSubmitButtonClick(){
    let searchQuery = searchCustomerValue;
    await getCustomerByNum(searchQuery)
    .then(display())
  }
```

---

</SwmSnippet>

# Main Functions

The main functions involved in the Customer Delete Page include <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="22:2:2" line-data="const CustomerDeletePage = () =&gt; {">`CustomerDeletePage`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="47:3:3" line-data="    await getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="120:5:5" line-data="  async function deleteCustomer(row) {">`deleteCustomer`</SwmToken>, and <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="154:5:5" line-data="  async function deleteAccount(row) {">`deleteAccount`</SwmToken>.

## <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="22:2:2" line-data="const CustomerDeletePage = () =&gt; {">`CustomerDeletePage`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="22:2:2" line-data="const CustomerDeletePage = () =&gt; {">`CustomerDeletePage`</SwmToken> function is the main component for the customer deletion page. It handles the state and user interactions for deleting a customer.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="22">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="22:2:2" line-data="const CustomerDeletePage = () =&gt; {">`CustomerDeletePage`</SwmToken> function manages the state and user interactions for deleting a customer. It includes state variables for storing the customer number, customer details, and account details.

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

  function handleCustomerNumberInput(e){
    setSearchCustomerValue(e.target.value)
  }

  function display() {
    setIsOpened(wasOpened => !wasOpened);
  }

  function displayNoResultsModal(){
    setIsNoResultsModalOpen(wasOpened => !wasOpened)
```

---

</SwmSnippet>

## <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="47:3:3" line-data="    await getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="47:3:3" line-data="    await getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken> function retrieves customer details based on the customer number entered by the user. It sets the state with the retrieved customer details.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="63">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="67:5:5" line-data="   async function getCustomerByNum(searchQuery) {">`getCustomerByNum`</SwmToken> function retrieves customer details using the customer number entered by the user. It formats the retrieved data and updates the state with the customer details.

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
```

---

</SwmSnippet>

## <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="120:5:5" line-data="  async function deleteCustomer(row) {">`deleteCustomer`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="120:5:5" line-data="  async function deleteCustomer(row) {">`deleteCustomer`</SwmToken> function checks if the customer has any associated accounts. If not, it deletes the customer and shows a success modal. Otherwise, it shows an error modal.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="116">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="120:5:5" line-data="  async function deleteCustomer(row) {">`deleteCustomer`</SwmToken> function checks if the customer has any associated accounts. If the customer has no accounts, it deletes the customer and displays a success modal. If the customer has accounts, it displays an error modal.

```javascript
  /**
   * Checks that a customer has no outstanding accounts and then deletes the customer
   * If the customer still has accounts or the delete fails a failure modal is shown, else a success modal is shown
   */
  async function deleteCustomer(row) {
    let customerNumber = row.cells[0].value
    let responseData;
    let howManyAccountsData;
    try {
      await axios
        .get(process.env.REACT_APP_ACCOUNT_URL + `/retrieveByCustomerNumber/${customerNumber}`)
        .then(response => {
          howManyAccountsData = response.data
        })
      let numberOfAccounts = howManyAccountsData.numberOfAccounts
      if (parseInt(numberOfAccounts) === 0) {
        await axios
          .delete(process.env.REACT_APP_CUSTOMER_URL + `/${customerNumber}`)
          .then(response => {
            responseData = response.data
            displayModal()
```

---

</SwmSnippet>

## <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="154:5:5" line-data="  async function deleteAccount(row) {">`deleteAccount`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="154:5:5" line-data="  async function deleteAccount(row) {">`deleteAccount`</SwmToken> function deletes an account based on the account number. It shows a success modal if the deletion is successful or an error modal if it fails.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="151">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="154:5:5" line-data="  async function deleteAccount(row) {">`deleteAccount`</SwmToken> function deletes an account using the account number. It displays a success modal if the deletion is successful or an error modal if it fails.

```javascript
  /**
   * Deletes the account from a given row
   */
  async function deleteAccount(row) {
    let accountNumber = row.accountNumber
    let responseData;
    try {
      await axios
        .delete(process.env.REACT_APP_ACCOUNT_URL + `/${accountNumber}`)
        .then(response => {
          responseData = response.data
        })
      displayAccountModal()
      displaySuccessfulAccountDeleteModal()
    } catch (e) {
      console.log(e)
      displayAccountModal()
      displayUnableDeleteModal()
    }
  }
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
