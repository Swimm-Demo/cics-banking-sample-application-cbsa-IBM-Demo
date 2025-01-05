---
title: Account Details Table Component
---
# Introduction

This document will walk you through the implementation of the Account Details Table component in the bank application frontend.

The Account Details Table component is designed to display and update account details for customers. It includes functionalities for rendering account data in a table, handling user input for updating account details, and making API calls to persist changes.

We will cover:

1. Defining table headers
2. Managing component state
3. Handling user input and updates
4. Rendering the table and modal

# Defining table headers

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" line="26">

---

We define the headers for the account details table. These headers specify the columns that will be displayed in the table.

```javascript
/**
 * Customer headers in table
 */
const headers = [
  {
    key: 'customerNumber',
    header: "Customer Number",
  },
  {
    key: 'accountNumber',
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
    key: 'actualBalance',
    header: 'Actual Balance',
  },
  {
    key: 'formattedDateOpened',
    header: 'Date Opened',
  },
  {
    key: 'formattedLastStatementDue',
    header: 'Last Statement Due',
  },
  {
    key: 'formattedNextStatementDue',
    header: 'Next Statement Due',
  },
];
```

---

</SwmSnippet>

# Managing component state

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" line="92">

---

We initialize state variables to manage the current and entered values for account details. This ensures that we can track both the original and updated values.

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
  const [enteredInterestRate, setInterestRate] = useState('');
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" line="214">

---

&nbsp;

```javascript
  const [isUpdateAccountModalOpened, setUpdateAccountModalOpened] = useState(
    false
  );
```

---

</SwmSnippet>

# Handling user input and updates

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" line="122">

---

We define functions to handle user input changes and to update the account details. These functions ensure that the component can respond to user actions and persist changes to the backend.

```javascript
  const enteredInterestRateChangeHandler = event => {
    setInterestRate(event.target.value);
  };

  const enteredOverdraftLimitChangeHandler = event => {
    setOverdraftLimit(event.target.value);
  };

  const enteredAccountTypeChangeHandler = event => {
    setAccountType(event.target.value);
  };
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" line="116">

---

&nbsp;

```javascript
  function onUpdateMainAccountClick(row) {
    clearExistingData()
    setPrefilledMainAccountData(row)
    displayUpdateAccountModal()
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" line="134">

---

&nbsp;

```javascript
  /**
   * Ensure no states have not been reset between account update
   */
  function clearExistingData() {
    setCurrentAccountType("")
    setCurrentOverdraft("")
    setCurrentInterestRate("")
  }

  /**
   * Set current value states with the data from the main row
   */
  function setPrefilledMainAccountData(row) {
    setAccountNumber(row.cells[1].value)
    setAccountSortCode(row.cells[2].value)
    setAccountType(row.cells[3].value)
    setCurrentAccountType(row.cells[3].value)
    setCurrentInterestRate(row.cells[4].value)
    setCurrentOverdraft(row.cells[5].value)
    setCurrentAvailableBalance(row.cells[6].value)
    setCurrentActualBalance(row.cells[7].value)
    setDateOpened(row.cells[8].value)
    setLastStatementDate(row.cells[9].value)
    setNextStatementDate(row.cells[10].value)
  }

  /**
   * Checks that all edited fields have been filled, if one is empty it uses the fallback value that was set before the user began edits
   * 
   */
  async function updateAccount() {
    let responseData;
    let useInterestRate = enteredInterestRate;
    let useOverdraft = enteredOverdraftLimit;
    let useAccountType = enteredAccountType;
    let useAccountNumber = accountNumber;

    if (useInterestRate.length === 0) {
      useInterestRate = currentInterestRate
    }
    if (useOverdraft.length === 0) {
      useOverdraft = currentOverdraft
    }
    if (useAccountType.length === 0) {
      useAccountType = currentAccountType
    }

    //Update the account with the details given
    try {

let newLastStatementDate = lastStatementDate.substring(6,10) + "-" + lastStatementDate.substring(3,5) + "-" + lastStatementDate.substring(0,2)
let newNextStatementDate = nextStatementDate.substring(6,10) + "-" + nextStatementDate.substring(3,5) + "-" + nextStatementDate.substring(0,2)
let newDateOpened        = dateOpened.substring(6,10) + "-" + dateOpened.substring(3,5) + "-" + dateOpened.substring(0,2) 
      await axios
        .put(process.env.REACT_APP_ACCOUNT_URL + `/${useAccountNumber}`, {
          interestRate: useInterestRate,
          lastStatementDate: newLastStatementDate,
          nextStatementDate: newNextStatementDate,
          dateOpened: newDateOpened,
          actualBalance: currentActualBalance,
          overdraft: useOverdraft,
          accountType: useAccountType,
          id: accountNumber,
          customerNumber: currentAccountCustomerNumber,
          sortCode: accountSortCode,
          availableBalance: currentAvailableBalance
        }).then((response) => {
          responseData = response.data
        }).catch(function (error) {
          if (error.response) {
            console.log(error)
          }
        })
    } catch (e) {
      console.log("Error updating account: " + e)
    }
    setUpdateAccountModalOpened(wasUpdateAccountOpened => !wasUpdateAccountOpened)
    window.location.reload(true)
  }
```

---

</SwmSnippet>

# Rendering the table and modal

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsTable.js" line="224">

---

We render the account details table and the update modal. The table displays the account data, and the modal allows users to update account details.

```javascript
  return (
    <DataTable
      rows={accountMainRow}
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
                  <TableRow {...getRowProps({ row })}>
                    {row.cells.map(cell => (
                      <TableCell key={cell.id}>{cell.value}</TableCell>
                    ))}
                    <Button
                      className="displayModal"
                      onClick={() => onUpdateMainAccountClick(row)}>
                      Update
                    </Button>
                    <Modal
                      modalHeading="Update Account"
                      passiveModal
                      open={isUpdateAccountModalOpened}
                      onRequestClose={() => {displayUpdateAccountModal()}}>

                      <div style={{ width: 200 }}>
                        <TextInput
                          id="accountNumber"
                          value={accountNumber}
                          label="Account number (this cannot be changed)"
                          readOnly
                          hideSteppers
                          allowEmpty
                          style={{ marginBottom: "1rem" }}
                        />
                      </div>

                      <div style={{ width: 200 }}>
                        <TextInput
                          id="interestRate"
                          placeHolder={currentInterestRate}
                          labelText="Interest rate:"
                          onChange={enteredInterestRateChangeHandler}
                          style={{ marginBottom: "1rem" }}
                        />
                      </div>

                      <div style={{ width: 200 }}>
                        <Dropdown
                          items2={["MORTGAGE", "ISA", "LOAN", "SAVING", "CURRENT"]}
                          id="default"
                          titleText="Account Type"
                          label="Account Type"
                          items={["MORTGAGE", "ISA", "LOAN", "SAVING", "CURRENT"]}
                          onChange={({ selectedItem }) =>
                            setAccountType(selectedItem)
                          }
                          selectedItem={enteredAccountType}
                          style={{ marginBottom: "1rem" }}
                        />
                      </div>

                      <div style={{ width: 200 }}>
                        <TextInput
                          id="carbon-number"
                          labelText="Overdraft Limit:"
                          placeHolder={currentOverdraft}
                          onChange={enteredOverdraftLimitChangeHandler}
                        />
                      </div>

                      <ModalFooter>
                        <Button onClick={() => {updateAccount()}}>
                          Submit
                        </Button>
                      </ModalFooter>
                    </Modal>
                  </TableRow>
                </React.Fragment>
              ))}
            </TableBody>
          </Table>
        </TableContainer>
      )}
    />
  );
};

export default AccountDetailsTable;
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
