---
title: Exploring the Account Deletion Page
---
# Overview

The Account Delete Page is a part of the Bank Frontend that allows users to delete an existing bank account. Users can access this page by clicking on 'Delete account' from the landing page. The page includes a form where users can enter the account number they wish to delete. Upon submitting the account number, the page displays the account details and provides an option to confirm the deletion. The deletion process involves making a RESTful API call to the backend to remove the account from the database. If the deletion is successful, a success message is displayed; otherwise, an error message is shown.

# How to Use the Account Delete Page

To remove an account, click on 'Delete account' from the landing page. Enter the account number in the provided form and submit. The page will display the account details and provide an option to confirm the deletion.

# Example Usage

The <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeletePage.js" pos="19:2:2" line-data="const AccountDeletePage = () =&gt; {">`AccountDeletePage`</SwmToken> component handles the user interface for deleting an account. It includes a form for entering the account number and displays the account details upon submission.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDeletePage/AccountDeletePage.js" line="19">

---

The <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeletePage.js" pos="19:2:2" line-data="const AccountDeletePage = () =&gt; {">`AccountDeletePage`</SwmToken> component includes a form for entering the account number and displays the account details upon submission. It also contains navigation links to the Home and Control Panel pages.

```javascript
const AccountDeletePage = () => {
  const [isOpened, setIsOpened] = useState(false);
  var [searchAccountValue, setSearchAccountValue] = useState("")

  function handleAccountNumberInput(e){
    setSearchAccountValue(e.target.value)
  }

  function display() {
    setIsOpened(wasOpened => !wasOpened);
  }
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
```

---

</SwmSnippet>

# Deletion Process

The <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="169:5:5" line-data="  async function deleteAccount(){">`deleteAccount`</SwmToken> function handles the deletion process by making a RESTful API call to the backend. It displays a success or error message based on the response.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" line="165">

---

The <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="169:5:5" line-data="  async function deleteAccount(){">`deleteAccount`</SwmToken> function makes an API call to delete the account and displays either a success or failure modal based on the response.

```javascript
  /**
   * Deletes the account tied to accountNumberToDelete
   * Displays either a success or failure modal once a response is received
   */
  async function deleteAccount(){
    let accountNumber = accountNumberToDelete
    let responseData;
    try{
     await axios
     .delete(process.env.REACT_APP_ACCOUNT_URL + `/${accountNumber}`)
     .then(response => {
       responseData = response.data
       console.log(responseData)
     }).catch(function (error){
      if (error.response){
        console.log(error) 
        displayModal()
        displayUnableDeleteModal()
      }
     })
     displayModal()
```

---

</SwmSnippet>

# Main Functions

There are several main functions in this folder. Some of them are <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="87:2:2" line-data="const AccountDeleteTables = ({accountQuery}) =&gt; {">`AccountDeleteTables`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="169:5:5" line-data="  async function deleteAccount(){">`deleteAccount`</SwmToken>, and <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="181:1:1" line-data="        displayModal()">`displayModal`</SwmToken>. We will dive a little into <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="169:5:5" line-data="  async function deleteAccount(){">`deleteAccount`</SwmToken> and <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="181:1:1" line-data="        displayModal()">`displayModal`</SwmToken>.

## <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="87:2:2" line-data="const AccountDeleteTables = ({accountQuery}) =&gt; {">`AccountDeleteTables`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="87:2:2" line-data="const AccountDeleteTables = ({accountQuery}) =&gt; {">`AccountDeleteTables`</SwmToken> function is responsible for rendering the table that displays the account details. It includes functions to fetch account data, display modals for confirmation, and handle the deletion process.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" line="87">

---

The <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="87:2:2" line-data="const AccountDeleteTables = ({accountQuery}) =&gt; {">`AccountDeleteTables`</SwmToken> function fetches account data and displays it in a table. It also handles the deletion process and displays modals for confirmation.

```javascript
const AccountDeleteTables = ({accountQuery}) => {
  const [mainAccountRow, setMainRows] = useState([]);
  const [otherAccountRows, setOtherAccountRows] = useState([]);
  getAccountByNum(accountQuery)

  /**
   * get the account from the given account number, create an array of the results and set mainAccountRow to this
   * then call getOtherAccounts to find any other accounts belonging to the customer
   */
  async function getAccountByNum(accountQuery) {
    let account;
    let rowBuild = [];
    await axios
    .get(process.env.REACT_APP_ACCOUNT_URL + `/${accountQuery}`)
      .then(response => {
        account = response.data;
      });
    try {
      let row;
      row = {
        id : account.id,
```

---

</SwmSnippet>

## <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="181:1:1" line-data="        displayModal()">`displayModal`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="181:1:1" line-data="        displayModal()">`displayModal`</SwmToken> function toggles the state of the modal used for confirming account deletion.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" line="212">

---

The <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" pos="212:3:3" line-data="  function displayModal() {">`displayModal`</SwmToken> function toggles the state of the modal used for confirming account deletion.

```javascript
  function displayModal() {
    setModalOpened(wasOpened => !wasOpened);
  }
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
