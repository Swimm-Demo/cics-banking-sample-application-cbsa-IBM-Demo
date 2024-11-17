---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: 9yrt26xi
---
# Getting Started with Account Details Page

## Overview

The Account Details Page is a component in the Bank Frontend that allows users to view detailed information about a specific bank account. Users can access the Account Details Page by clicking on 'View account details' from the landing page.

## Accessing the Account Details Page

Users can access the Account Details Page by clicking on 'View account details' from the landing page. This action will navigate the user to the Account Details Page where they can enter an account number to retrieve the associated account details.

## Fetching Account Details

Upon entering a valid account number, the page fetches the account details from the backend and displays them in a structured format. The function <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="47:1:1" line-data="        getCustomerAccounts(searchQuery)">`getCustomerAccounts`</SwmToken> is responsible for fetching the account details from the backend and setting the state to display the retrieved information.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" line="66">

---

The function <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="70:5:5" line-data="  async function getCustomerAccounts(searchQuery) {">`getCustomerAccounts`</SwmToken> fetches the account details from the backend using the provided account number. It processes the response and updates the state with the retrieved account details.

```javascript
  /**
   * Get the account for a given accountNumber, create an array of the response and set accountMainRow to this array
   * Calls getOtherAccountsForCustomer to find the other accounts tied to this customer's number
   */
  async function getCustomerAccounts(searchQuery) {
    let account;
    let rowBuild = [];
    await axios
      .get(process.env.REACT_APP_ACCOUNT_URL + `/${searchQuery}`)
      .then(response => {
        account = response.data;
      }).catch (function (error) {
        if (error.response){
          displayNoResults()
          console.log(error)
        }
      })
    try {
      let row;
      let formattedDateOpened = getDay(account.dateOpened) + "-" + getMonth(account.dateOpened) + "-" + getYear(account.dateOpened)
      let formattedLastStatementDue = getDay(account.lastStatementDate) + "-" + getMonth(account.lastStatementDate) + "-" + getYear(account.lastStatementDate)
```

---

</SwmSnippet>

## Handling No Results

If no account is found for the entered account number, a modal is displayed informing the user that no accounts were found. The function <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="40:3:3" line-data="  function displayNoResults() {">`displayNoResults`</SwmToken> toggles the modal to inform the user that no accounts were found.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" line="40">

---

The function <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="40:3:3" line-data="  function displayNoResults() {">`displayNoResults`</SwmToken> toggles the modal to inform the user that no accounts were found.

```javascript
  function displayNoResults() {
    setShowNoResultsModal(wasOpened => !wasOpened)
  }
```

---

</SwmSnippet>

## Main Functions

There are several main functions in this folder. Some of them are <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="22:2:2" line-data="const AccountDetailsPage = () =&gt; {">`AccountDetailsPage`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="44:3:3" line-data="  function handleClick() {">`handleClick`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="47:1:1" line-data="        getCustomerAccounts(searchQuery)">`getCustomerAccounts`</SwmToken>, and <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" pos="92:2:2" line-data="const AccountDetailsTable = ({accountMainRow}) =&gt; {">`AccountDetailsTable`</SwmToken>. We will dive a little into <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="44:3:3" line-data="  function handleClick() {">`handleClick`</SwmToken> and <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="47:1:1" line-data="        getCustomerAccounts(searchQuery)">`getCustomerAccounts`</SwmToken>.

### <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="22:2:2" line-data="const AccountDetailsPage = () =&gt; {">`AccountDetailsPage`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="22:2:2" line-data="const AccountDetailsPage = () =&gt; {">`AccountDetailsPage`</SwmToken> function initializes the state variables and defines the structure of the account details page.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" line="22">

---

The <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="22:2:2" line-data="const AccountDetailsPage = () =&gt; {">`AccountDetailsPage`</SwmToken> function initializes the state variables and defines the structure of the account details page.

```javascript
const AccountDetailsPage = () => {
  const [isOpened, setIsOpened] = useState(false);
  const [userInput, setUserInput] = useState("")
  const [accountMainRow, setMainRow] = useState([]);
  const [showNoResultsModal, setShowNoResultsModal] = useState(false)
```

---

</SwmSnippet>

### <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="44:3:3" line-data="  function handleClick() {">`handleClick`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="44:3:3" line-data="  function handleClick() {">`handleClick`</SwmToken> function handles the click event when the user submits an account number. It validates the input and calls <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="47:1:1" line-data="        getCustomerAccounts(searchQuery)">`getCustomerAccounts`</SwmToken> to fetch the account details.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" line="44">

---

The <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="44:3:3" line-data="  function handleClick() {">`handleClick`</SwmToken> function handles the click event when the user submits an account number. It validates the input and calls <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="47:1:1" line-data="        getCustomerAccounts(searchQuery)">`getCustomerAccounts`</SwmToken> to fetch the account details.

```javascript
  function handleClick() {
    let searchQuery = userInput;
    if (userInput.length !== 0){
        getCustomerAccounts(searchQuery)
        setIsOpened(wasOpened => !wasOpened)
      } else {
        displayNoResults()
      }
  }
```

---

</SwmSnippet>

### <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" pos="92:2:2" line-data="const AccountDetailsTable = ({accountMainRow}) =&gt; {">`AccountDetailsTable`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" pos="92:2:2" line-data="const AccountDetailsTable = ({accountMainRow}) =&gt; {">`AccountDetailsTable`</SwmToken> function displays the account details in a tabular format. It includes functions to handle updates and manage the state of the account details.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" line="92">

---

The <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" pos="92:2:2" line-data="const AccountDetailsTable = ({accountMainRow}) =&gt; {">`AccountDetailsTable`</SwmToken> function displays the account details in a tabular format. It includes functions to handle updates and manage the state of the account details.

```javascript
const AccountDetailsTable = ({accountMainRow}) => {
  /**
   * Set states for all of the current account values, before the user edits any. This provides a fallback if any fields are left blank by the user
   */
  const [accountNumber, setAccountNumber] = useState("")
  const [currentAccountType, setCurrentAccountType] = useState("")
  const [currentOverdraft, setCurrentOverdraft] = useState("")
  const [currentInterestRate, setCurrentInterestRate] = useState("")
  const [accountSortCode, setAccountSortCode] = useState("")
  const [currentActualBalance, setCurrentActualBalance] = useState("")
  const [lastStatementDate, setLastStatementDate] = useState("")
  const [nextStatementDate, setNextStatementDate] = useState("")
  const [dateOpened, setDateOpened] = useState("")
  const [currentAvailableBalance, setCurrentAvailableBalance] = useState("")
  const [currentAccountCustomerNumber, setAccountCustomerNumber] = useState("")

  /**
   * States that are edited by the user
   */
  const [enteredOverdraftLimit, setOverdraftLimit] = useState('');
  const [enteredAccountType, setAccountType] = useState("");
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
