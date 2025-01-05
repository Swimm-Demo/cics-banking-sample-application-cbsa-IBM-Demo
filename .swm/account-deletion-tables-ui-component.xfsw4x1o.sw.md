---
title: Account Deletion Tables UI Component
---
# Introduction

This document will walk you through the "Account Deletion Tables" UI component in the bank application frontend.

The Account Deletion Tables component is designed to display account information and allow users to delete accounts. It fetches account data, displays it in a table, and provides modals for account deletion confirmation and status.

We will cover:

1. Defining table headers.
2. Fetching and setting account data.
3. Handling account deletion.
4. Rendering the table and modals.

# Defining table headers

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" line="27">

---

We define the headers for the main account table and the other accounts table. These headers specify the columns to be displayed.

```javascript
const headers = [
  {
    key: 'customerNumber',
    header: "Customer Number",
  },
  {
    key: 'id',
    header: 'Account Number',
  },
  {
    key: 'sortCode',
    header: 'Sort Code',
  },
  {
    key: 'accountType',
    header: 'Account Type',
  },
  {
    key: 'interestRate',
    header: 'Interest Rate',
  },
  {
    key: 'overdraft',
    header: 'Overdraft',
  },
  {
    key: 'availableBalance',
    header: 'Available Balance',
  },
  {
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" line="74">

---

&nbsp;

```javascript
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
  'Next Statement Due'
];
```

---

</SwmSnippet>

# Fetching and setting account data

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" line="87">

---

The component fetches account data based on the provided account number. It sets the main account row and other account rows accordingly.

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
        customerNumber : account.customerNumber,
        accountNumber : account.id,
        sortCode : account.sortCode,
        accountType : account.accountType,
        interestRate : account.interestRate,
        overdraft : account.overdraft,
        availableBalance : account.availableBalance,
        actualBalance : account.actualBalance,
        accountOpened : account.dateOpened,
        lastStatementDate : account.lastStatementDate,
        nextStatementDate : account.nextStatementDate
      };
      rowBuild.push(account);
      getOtherAccounts(row.customerNumber, accountQuery)
      setMainRows(rowBuild)
    } catch (e) {
      console.log("Error: " + e);
    }
  }

  /**
   * get all other accounts belonging to the customerID passed in, create an array and set otherAccountRows to this array
   * Ignores the main account row already found by comparing the accountID in the responseData to the accountID entered by the user initially
   */
  async function getOtherAccounts(customerID, accountQuery){
    let accountData;
    let rowBuild = [];
    await axios
    .get(process.env.REACT_APP_ACCOUNT_URL + `/retrieveByCustomerNumber/${customerID}`)
    .then(response => {
      accountData = response.data;
    });
    try {
      let row;
      accountData.accounts.forEach(account => { 
      row = {
        accountNumber : account.id,
        sortCode : account.sortCode,
        accountType : account.accountType,
        interestRate : account.interestRate,
        overdraft : account.overdraft,
        availableBalance : account.availableBalance,
        actualBalance : account.actualBalance,
        accountOpened : account.dateOpened,
        lastStatementDate : account.lastStatementDate,
        nextStatementDate : account.nextStatementDate
      };
      if (parseInt(row.accountNumber) !== parseInt(accountQuery)){
      rowBuild.push(row)
      }
    });
      setOtherAccountRows(rowBuild)
    } catch (e) {
      console.log("Error fetching accounts for customer: " + customerID + ": " + e);
    }
  }
```

---

</SwmSnippet>

# Handling account deletion

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" line="165">

---

The component includes functions to handle account deletion. It displays modals for confirmation and status updates.

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
     displaySuccessfulDeleteModal()

   } catch (e) {
     console.log(e)
     displayModal()
     displayUnableDeleteModal()
   } 
   }

const [isModalOpened, setModalOpened] = useState(false);

const [wasUnableDeleteOpened, setUnableDeleteModalOpened] = useState(false);

const [wasSuccessfulDeleteModalOpened, setSuccessfulDeleteModalOpened] = useState(false)

const [accountNumberToDelete, setAccountNumberToDelete] = useState("")

function displayModalMainAccount(row){
  setAccountNumberToDelete(row.cells[1].value)
  displayModal()
}

function displayModalOtherAccount(row){
  setAccountNumberToDelete(row.accountNumber)
  displayModal()
}
  function displayModal() {
    setModalOpened(wasOpened => !wasOpened);
  }

  function displayUnableDeleteModal() {
    setUnableDeleteModalOpened(wasUnableDeleteOpened => !wasUnableDeleteOpened);
  }

  function displaySuccessfulDeleteModal(){
    setSuccessfulDeleteModalOpened(wasSuccessfulDeleteModalOpened => !wasSuccessfulDeleteModalOpened)
  }

  function refreshPage(){
    window.location.reload()
  }
```

---

</SwmSnippet>

# Rendering the table and modals

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDeletePage/AccountDeleteTables.js" line="228">

---

Finally, the component renders the table with the fetched account data and includes buttons to trigger the deletion modals.

```javascript
  return (
    <DataTable
      rows={mainAccountRow}
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
                      onClick={() => displayModalMainAccount(row)}>
                      Delete
                    </Button>
                    <Modal
                      modalHeading="Are you sure you want to delete this account?"
                      open={isModalOpened}
                      onRequestClose={displayModal}
                      onRequestSubmit={deleteAccount}
                      danger
shouldCloseAfterSubmit
                      primaryButtonText="Delete"
                      secondaryButtonText="Cancel">
                      <ModalBody hasForm>
                        Warning! Are you sure you want to delete account {accountNumberToDelete}? This action cannot be undone
                      </ModalBody>
                    </Modal>
                    <Modal
                      modalHeading="Unable to delete the account!"
                      open={wasUnableDeleteOpened}
                      onRequestClose={displayUnableDeleteModal}
                      danger
                      passiveModal>
                    </Modal>
                  </TableExpandRow>

                  <TableExpandedRow colSpan={headers.length + 2}>
                    <p className="account-details">Other accounts belonging to this customer</p>
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
                        {otherAccountRows.map((row, index) => (
                          <TableRow key={row.id}>
                            {Object.keys(row)
                              .filter(key => key !== 'id')
                              .map(key => {
                                return (
                                  <TableCell key={key}>{row[key]}</TableCell>
                                );
                              })}
                            {/* <ModalWrapper
                                      triggerButtonKind="danger"
                                      buttonTriggerClassName = "modal-button"
                                      buttonTriggerText="Delete"
                                      modalHeading="Are you sure want to delete this user?"
                                      modalLabel="Delete Customer"
                                      handleSubmit={e => handleDelete(index,e);return true}
                                      shouldCloseAfterSubmit
                                      onRequestClose={displayModal}
                                      primaryButtonText="Yes, delete"
                                      danger
                                      secondaryButtonText="Cancel" >
                                    </ModalWrapper> */}
                            <Button
                              kind="danger"
                              className="displayModal"
                              onClick={() => displayModalOtherAccount(row)}>
                              Delete
                            </Button>
                            <Modal
                            modalHeading="Account deleted successfully"
                            open={wasSuccessfulDeleteModalOpened}
                            onRequestClose={() => {displaySuccessfulDeleteModal(); refreshPage()}}
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

export default AccountDeleteTables;
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
