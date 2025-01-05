---
title: Basic Concepts of the Account Deletion Page
---
# Form for Account Number

# Account Delete Page Overview

The Account Delete Page is a user interface component that allows bank tellers to delete an existing account. This page is essential for managing and maintaining the bank's account records by providing a straightforward way to remove accounts that are no longer needed.

# Submitting the Form

The page includes a form where the user can enter the account number they wish to delete. This form is the initial step in the account deletion process, ensuring that the correct account is targeted for deletion.

# Fetching Account Details

Upon entering the account number, the user can submit the form. This action triggers the display of account details, allowing the user to verify that the correct account has been selected for deletion.

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDeletePage/AccountDeletePage.js" line="76">

---

The page uses the <SwmToken path="src/bank-application-frontend/src/content/AccountDeletePage/AccountDeletePage.js" pos="77:2:2" line-data="                    &lt;AccountDeleteTables ">`AccountDeleteTables`</SwmToken> component to fetch and display the account details based on the entered account number. This component is responsible for retrieving the necessary information from the backend and presenting it to the user.

```javascript
                  <Column lg={16}>
                    <AccountDeleteTables 
                    accountQuery={searchAccountValue} />
                  </Column>
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
