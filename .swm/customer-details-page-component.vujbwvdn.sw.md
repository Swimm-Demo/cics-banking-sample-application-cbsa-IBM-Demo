---
title: Customer Details Page Component
---
# Introduction

This document will walk you through the implementation of the Customer Details Page component in the bank application frontend.

The Customer Details Page allows users to search for customer details by either customer number or name and view the associated account details.

We will cover:

1. State management for user inputs and table visibility.
2. Handling user input changes.
3. Submitting the search query.
4. Fetching customer and account details from the backend.
5. Rendering the component and handling the UI.

# State management for user inputs and table visibility

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="23">

---

We define the states necessary for managing user inputs and the visibility of the results table.

```javascript
const CustomerDetailsPage = () => {
  /**
   * States for table visibility and entered search values from the user
   */
  const [isOpened, setTableOpened] = useState(false);
  const [customerDetailsRows, setRows] = useState([]);
  const [accountDetailsRows, setAccountRows] = useState([]);
  const [noResultsOpened, setNoResultsOpened] = useState(false)
  var [numSearch, setNumSearch] = useState("");
  var [nameSearch, setNameSearch] = useState("")
```

---

</SwmSnippet>

# Handling user input changes

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="35">

---

We have two functions to handle changes in the user inputs for customer number and name.

```javascript
  function handleNumInputChange(e) {
    setNumSearch(e.target.value)
  }

  function handleNameInputChange(e) {
    setNameSearch(e.target.value)
  }
```

---

</SwmSnippet>

# Submitting the search query

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="47">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="47:3:3" line-data="  function submitButtonHandler() {">`submitButtonHandler`</SwmToken> function determines which search query to execute based on the user input and toggles the table visibility.

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

# Fetching customer and account details from the backend

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="72">

---

We have three functions to fetch data from the backend:

1. <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="55:1:1" line-data="      getCustomersByName(searchQuery)">`getCustomersByName`</SwmToken> fetches the first 10 customers matching the given name.

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
              customerAddress: customer.customerAddress,
              formattedDOB : formattedDOB,
              dateOfBirth: customer.dateOfBirth,
              creditScore: customer.customerCreditScore,
              formattedReviewDate : formattedReviewDate,
              nextReviewDate: customer.customerCreditScoreReviewDate,
            };
            rowBuild.push(row);
            getAccountsForCustomers(row.id)
          })
          setRows(rowBuild)
        } catch (e) {
          console.log("Error: " + e);
        }
      }).catch(function (error) {
        if (error.response) {
          console.log(error)
          displayNoResultsModal()
        }
      })

  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="116">

---

2. <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="51:1:1" line-data="      getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken> fetches the customer details for a given customer number.

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
            formattedDOB : formattedDOB,
            dateOfBirth: responseData.dateOfBirth,
            creditScore: responseData.customerCreditScore,
            formattedReviewDate : formattedReviewDate,
            nextReviewDate: responseData.customerCreditScoreReviewDate,
          };
          rowBuild.push(row);
          getAccountsForCustomers(row.id)
          setRows(rowBuild)
        } catch (e) {
          console.log("Error: " + e);
        }
      }).catch(function (error) {
        if (error.response) {
          console.log(error)
          displayNoResultsModal()
        }
      })

  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="158">

---

3. <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="101:1:1" line-data="            getAccountsForCustomers(row.id)">`getAccountsForCustomers`</SwmToken> fetches the account details for a given customer ID.

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
            lastStatementDate: account.lastStatementDate,
          };
          accountRowBuild.push(row)
        });
        setAccountRows(accountRowBuild)
      }).catch(function (error) {
        if (error.response) {
          console.log(error)
        }
      })
  }
```

---

</SwmSnippet>

# Rendering the component and handling the UI

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="191">

---

The component renders the UI elements including input fields, buttons, and the results table. It also handles the display of a modal when no results are found.

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
          <BreadcrumbItem>Customer Details</BreadcrumbItem>
        </Breadcrumb>
        <h1 className="landing-page__heading">
          View Customer Details
        </h1>
      </Column>
      <Column lg={16} md={8} sm={4} className="landing-page__r2">
        <div className="lower-content">
          <div class="cds--grid" style={{ marginLeft: '30px' }}>
            <div class="cds--row">
              <div class="cds--col">
                <div className="upper">
                  <div className="left-part">
                    <p> Please ensure one field is empty when you press Submit, otherwise the search may not work. After searching, the twistee can be expanded to see accounts belonging to this customer.
</p>
                    <NumberInput
                      className="customer-list-view"
                      id="customerNum"
                      label="Enter a customer's number to view details"
                      placeholder="e.g 1000"
                      invalidText='Please provide a valid number'
                      onChange={e => handleNumInputChange(e)}
                      hideSteppers
                      allowEmpty
                    />
                    <div style={{ marginTop: '20px' }}>
                      <TextInput
                        className="customer-list-name"
                        id="customerNameInput"
                        type="text"
                        labelText="Alternatively, enter the customer's name:"
                        placeholder="Case sensitive"
                        onChange={e => handleNameInputChange(e)}
                      />
                    </div>
                    <div style={{ marginTop: '20px' }}>
                      <Button type="submit" onClick={submitButtonHandler}>
                        Submit
                      </Button>
                    </div>
                  </div>
                  <div className="right-part">
                    <img
                      className="customers"
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="249">

---

&nbsp;

```javascript
                  </div>
                </div>
                {isOpened && (
                  <Column lg={16}>
                    <CustomerDetailsTable customerDetailsRows={customerDetailsRows} accountDetailsRows={accountDetailsRows} />
                  </Column>
                )}
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="259">

---

&nbsp;

```javascript
        </div>
      </Column>
      <Modal
        modalHeading="No customers found!"
        open={noResultsOpened}
        onRequestClose={displayNoResultsModal}
        danger
        passiveModal>
        <ModalBody hasForm>
          Please check that the customer number/name is correct
        </ModalBody>
      </Modal>
    </Grid>
  );
};

export default CustomerDetailsPage;
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
