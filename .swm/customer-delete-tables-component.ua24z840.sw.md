---
title: Customer Delete Tables Component
---
# Introduction

This document will walk you through the implementation of the <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="76:2:2" line-data="const CustomerDeleteTables = ({customerRow, accountRow}) =&gt; {">`CustomerDeleteTables`</SwmToken> component in the <SwmPath>[src/…/CustomerDeletePage/CustomerDeleteTables.js](src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js)</SwmPath> file.

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="76:2:2" line-data="const CustomerDeleteTables = ({customerRow, accountRow}) =&gt; {">`CustomerDeleteTables`</SwmToken> component is responsible for displaying customer and account data in a table format and providing functionality to delete customers and accounts with confirmation modals.

We will cover:

1. Table headers definition
2. State management for modals
3. Customer and account deletion logic
4. Rendering the table and modals

# Table headers definition

We define the headers for the customer and account tables. These headers are used to display the column names in the tables.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="27">

---

Customer table headers:

```javascript
/**
 * Headers for customer row in table
 */
const headers = [
  {
    key: 'customerNumber',
    header: 'Customer Number',
  },
  {
    key: 'sortCode',
    header: 'Sort Code',
  },
  {
    key: 'customerName',
    header: 'Customer Name',
  },
  {
    key: 'customerAddress',
    header: 'Customer Address',
  },
  {
    key: 'formattedDOB',
    header: 'Date of Birth',
  },
  {
    key: 'creditScore',
    header: 'Credit Score',
  },
  {
    key: 'formattedReviewDate',
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="61">

---

Account table headers:

```javascript
/**
 * Headers for account rows in table
 */
const account_headers = [
  'Account Number',
  'Sort Code',
  'Account Type',
  'Interest Rate',
  'Overdraft Limit',
  'Available Balance',
  'Actual Balance',
  'Account Opened',
  'Last Statement Due',
];
```

---

</SwmSnippet>

# State management for modals

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="76">

---

We use React's <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="81:12:12" line-data="  const [customerNameToDelete, setCustomerNameToDelete] = useState(&quot;&quot;)">`useState`</SwmToken> to manage the state of various modals and the customer/account to be deleted.

```javascript
const CustomerDeleteTables = ({customerRow, accountRow}) => {

  /**
   * States to store the customer and account to delete, as well as the open/close status of the respective modals
   */
  const [customerNameToDelete, setCustomerNameToDelete] = useState("")
  const [accountNumberToDelete, setAccountNumberToDelete] = useState("")
  const [isModalOpened, setModalOpened] = useState(false);
  const [wasUnableDeleteOpened, setUnableDeleteModalOpened] = useState(false);
  const [isSuccessfulCustomerDeleteModalOpened, setSuccessfulCustomerDeleteModalOpened] = useState(false)
  const [isSuccessfulAccountDeleteModalOpened, setSuccessfulAccountDeleteModalOpened] = useState(false)
```

---

</SwmSnippet>

# Customer and account deletion logic

We define functions to handle the deletion of customers and accounts. These functions manage the modal display and perform the necessary API calls to delete the data.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="88">

---

Function to handle customer deletion:

```javascript
  /**
   * Get the customer name from the row and show the confirm customer delete modal
   */
  function onDeleteCustomerClick(row) {
    setCustomerNameToDelete(row.cells[2].value)
    displayModal()
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="96">

---

Function to display the customer delete confirmation modal:

```javascript
  function displayModal() {
    setModalOpened(wasOpened => !wasOpened);
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="100">

---

Function to display the successful customer delete modal:

```javascript
  function displaySuccessfulCustomerDeleteModal() {
    setSuccessfulCustomerDeleteModalOpened(wasOpened => !wasOpened)
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="104">

---

Function to display the successful account delete modal:

```javascript
  function displaySuccessfulAccountDeleteModal() {
    setSuccessfulAccountDeleteModalOpened(wasOpened => !wasOpened)
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="108">

---

Function to handle account deletion:

```javascript
  /**
   * Get the accountNumber from the row and show the confirm account delete modal
   */
  function onDeleteAccountClick(row) {
    setAccountNumberToDelete(row.accountNumber)
    displayAccountModal()
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="116">

---

Function to delete a customer:

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
            displaySuccessfulCustomerDeleteModal()
          })
      }
      else {
        displayModal()
        displayUnableDeleteModal()
      }
    } catch (e) {
      console.log(e)
      displayModal()
      displayUnableDeleteModal()
    }
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="151">

---

Function to delete an account:

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

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="172">

---

Function to display the account delete confirmation modal:

```javascript
  const [isModalAccountOpened, setAccountModalOpened] = useState(false);

  function displayAccountModal() {
    setAccountModalOpened(wasAccountOpened => !wasAccountOpened);
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="178">

---

Function to display the unable to delete modal:

```javascript
  function displayUnableDeleteModal() {
    setUnableDeleteModalOpened(wasUnableDeleteOpened => !wasUnableDeleteOpened);
  }
```

---

</SwmSnippet>

# Rendering the table and modals

Finally, we render the <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="183:2:2" line-data="    &lt;DataTable">`DataTable`</SwmToken> component with the customer and account data. We also include the modals for confirming and displaying the results of the delete actions.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="182">

---

Rendering the table and modals:

```javascript
  return (
    <DataTable
      rows={customerRow}
      headers={headers}
      render={({
        rows,
        headers,
        getHeaderProps,
        getRowProps,
        getTableProps,
      }) => (
        <TableContainer title="" description="">
          <Table {...getTableProps()}>
            <TableHead>
              <TableRow>
                <TableExpandHeader />
                {headers.map(header => (
                  <TableHeader {...getHeaderProps({ header })}>
                    {header.header}
                  </TableHeader>
                ))}
                <div className="header-filler" />
              </TableRow>
            </TableHead>
            <TableBody>
              {rows.map(row => (
                <React.Fragment key={row.id}>
                  <TableExpandRow {...getRowProps({ row })}>
                    {row.cells.map(cell => (
                      <TableCell key={cell.id}>{cell.value}</TableCell>
                    ))}
                    <Button
                      kind="danger"
                      className="displayModal"
                      onClick={() => onDeleteCustomerClick(row)}>
                      Delete
                    </Button>
                    <Modal
                      modalHeading="Are you sure you want to delete this customer?"
                      open={isModalOpened}
                      onRequestClose={displayModal}
                      onRequestSubmit={() => deleteCustomer(row)}
                      danger
                      primaryButtonText="Delete"
                      secondaryButtonText="Cancel">
                      <ModalBody hasForm>
                        Warning! Are you sure you want to delete {customerNameToDelete}? This action cannot be undone
                      </ModalBody>
                    </Modal>
                    <Modal
                      modalHeading="Unable to delete the customer!"
                      open={wasUnableDeleteOpened}
                      onRequestClose={displayUnableDeleteModal}
                      danger
                      passiveModal>
                      <ModalBody hasForm>
                        Please delete all associated accounts before deleting
                        the customer
                      </ModalBody>
                    </Modal>
                    <Modal
                      modalHeading="Customer deleted successfully"
                      open={isSuccessfulCustomerDeleteModalOpened}
                      onRequestClose={() => {displaySuccessfulCustomerDeleteModal(); window.location.reload(true)}}
                      passiveModal>
                    </Modal>
                  </TableExpandRow>

                  <TableExpandedRow colSpan={headers.length + 2}>
                    <p className="account-details">Accounts belonging to this customer</p>
                    <Table>
                      <TableHead>
                        <TableRow>
                          {account_headers.map(header => (
                            <TableHeader id={header.key} key={header}>
                              {header}
                            </TableHeader>
                          ))}
                        </TableRow>
                      </TableHead>
                      <TableBody>
                        {accountRow.map((row, index) => (
                          <TableRow key={row.id}>
                            {Object.keys(row)
                              .filter(key => key !== 'id')
                              .map(key => {
                                return (
                                  <TableCell key={key}>{row[key]}</TableCell>
                                );
                              })}
                            <Button
                              kind="danger"
                              className="displayModal"
                              onClick={() => onDeleteAccountClick(row)}>
                              Delete
                            </Button>
                            <Modal
                              modalHeading="Are you sure you want to delete account"
                              open={isModalAccountOpened}
                              onRequestClose={displayAccountModal}
                              onRequestSubmit={() => deleteAccount(row)}
                              danger
                              primaryButtonText="Delete"
                              secondaryButtonText="Cancel">
                              <ModalBody>
                                Are you sure you want to delete account {accountNumberToDelete}? This action cannot be undone
                              </ModalBody>
                            </Modal>
                            <Modal
                              modalHeading="Account deleted successfully"
                              open={isSuccessfulAccountDeleteModalOpened}
                              onRequestClose={() => {displaySuccessfulAccountDeleteModal(); window.location.reload()}}
                              passiveModal
                            />
                          </TableRow>
                        ))}
                      </TableBody>
                    </Table>
                  </TableExpandedRow>
                </React.Fragment>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
      )}
    />
  );
};
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" line="311">

---

Exporting the component:

```javascript
export default CustomerDeleteTables;
```

---

</SwmSnippet>

This concludes the walkthrough of the <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeleteTables.js" pos="76:2:2" line-data="const CustomerDeleteTables = ({customerRow, accountRow}) =&gt; {">`CustomerDeleteTables`</SwmToken> component.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
