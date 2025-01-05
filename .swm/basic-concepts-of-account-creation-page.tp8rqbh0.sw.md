---
title: Basic Concepts of Account Creation Page
---
# Fields and Input Handlers

# Overview

The Account Creation Page is a component that allows users to create new bank accounts by entering necessary details.

# Modal Management

It includes fields for customer number, account type, overdraft limit, and interest rate, which are captured through input handlers.

# Form Submission

The page also manages various modals to provide feedback to the user, such as success, failure, and loading states.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" line="83">

---

When the user submits the form, the <SwmToken path="src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" pos="83:5:5" line-data="  async function createAccount() {">`createAccount`</SwmToken> function is called to send the account details to the server.

```javascript
  async function createAccount() {
    let responseData;
    try {
      await axios
        .post(process.env.REACT_APP_ACCOUNT_URL, {
          interestRate: enteredInterestRate,
          dateOpened: currentDate,
          overdraft: enteredOverdraftLimit,
          accountType: enteredAccountType,
          customerNumber: enteredCustomerID,
          sortCode: sortCode
        }).then((response) => {
          responseData = response.data
          setSuccessText(responseData.id)
          displayLoadingModal()
          displayModal()
        }).catch(function (error) {
          if (error.response) {
            displayLoadingModal()
            displayFailedModal()
            console.log(error)
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
