---
title: Account Creation Page UI Component
---
# Introduction

This document will walk you through the Account Creation Page UI Component.

The Account Creation Page allows users to create a new bank account by entering necessary details and submitting the form. The component handles user input, form submission, and displays appropriate modals based on the success or failure of the account creation process.

We will cover:

1. Initial state setup and default values.
2. Handling user input.
3. Modal display functions.
4. Account creation logic.
5. Form rendering and submission.

# Initial state setup and default values

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" line="25">

---

We start by defining the component and setting up initial states and default values. This includes setting up states for modals and user input fields, as well as defining default values like the current date and sort code.

```javascript
import { Link } from 'react-router-dom';

const AccountCreationPage = () => {

  /**
   * Create variables for default values - current date, sortcode
   */
  const [successText, setSuccessText] = useState("");
  const date = new Date()
  const currentDateToJSON = date.toJSON().split("T")
  const currentDate = currentDateToJSON[0]
  const items = [];
  const sortCode = "987654"

  /**
   * Create states for user entered fields
   */
  const [isModalOpened, setModalOpened] = useState(false);
  const [isFailureModalOpened, setIsFailureModalOpened] = useState(false);
  const [isFailureNetworkModalOpened, setIsFailureNetworkModalOpened] = useState(false);
  const [isLoadingModalOpened, setIsLoadingModalOpened] = useState(false);
  const [enteredInterestRate, setEnteredInterestRate] = useState('');
  const [enteredCustomerID, setEnteredCustomerID] = useState('');
  const [enteredAccountType, setEnteredAccountType] = useState(items[3]);
  const [enteredOverdraftLimit, setEnteredOverdraftLimit] = useState('');
```

---

</SwmSnippet>

# Handling user input

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" line="67">

---

Next, we define handlers for user input fields. These handlers update the state whenever the user changes the input values.

```javascript
  const enteredCustomerIDChangeHandler = event => {
    setEnteredCustomerID(event.target.value);
  };
  const enteredAccountTypeChangeHandler = event => {
    setEnteredAccountType(event.target.value);
  };
  const enteredOverdraftLimitChangeHandler = event => {
    setEnteredOverdraftLimit(event.target.value);
  };
  const enteredInterestRateChangeHandler = event => {
    setEnteredInterestRate(event.target.value);
  };
```

---

</SwmSnippet>

# Modal display functions

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" line="51">

---

We define functions to toggle the visibility of different modals. These functions are used to provide feedback to the user based on the result of the account creation process.

```javascript
  function displayModal() {
    setModalOpened(wasOpened => !wasOpened);
  };

  function displayLoadingModal(){
    setIsLoadingModalOpened(wasOpened => !wasOpened)
  }

  function displayFailedModal() {
    setIsFailureModalOpened(wasOpened => !wasOpened);
  }

  function displayFailedNetworkModal() {
    setIsFailureNetworkModalOpened(wasOpened => !wasOpened);
  }
```

---

</SwmSnippet>

# Account creation logic

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" line="80">

---

The <SwmToken path="src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" pos="83:5:5" line-data="  async function createAccount() {">`createAccount`</SwmToken> function handles the account creation process. It sends a POST request with the user-entered data and displays the appropriate modal based on the response.

```javascript
  /**
   * Creates an account using the user entered fields then displays either a success or failure modal
   */
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
          } else if (error.request){
              displayLoadingModal()
              displayFailedNetworkModal()
              console.log(error)
            }
        })
    } catch (e) {
      console.log("Error in creation: " + e)
      displayLoadingModal()
      displayFailedModal()
    }
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" line="117">

---

The <SwmToken path="src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" pos="117:5:5" line-data="  async function submitButtonHandler() {">`submitButtonHandler`</SwmToken> function is called when the user submits the form. It displays the loading modal and then calls <SwmToken path="src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" pos="119:1:1" line-data="    createAccount()">`createAccount`</SwmToken>.

```javascript
  async function submitButtonHandler() {
    displayLoadingModal()
    createAccount()
  }
```

---

</SwmSnippet>

# Form rendering and submission

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" line="122">

---

Finally, we render the form and the modals. The form includes input fields for customer number, account type, overdraft limit, and interest rate. The submit button triggers the <SwmToken path="src/bank-application-frontend/src/content/AccountCreationPage/AccountCreationPage.js" pos="188:13:13" line-data="              &lt;Button className=&quot;displayModal&quot; onClick={submitButtonHandler}&gt;">`submitButtonHandler`</SwmToken>.

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
          <BreadcrumbItem>Create Account</BreadcrumbItem>
        </Breadcrumb>
        <h1 className="landing-page__heading">Create new account</h1>
      </Column>
      <div className="content-parent">
        <div className="left-content-account">
          <Form>
            <Stack gap={7}>
              <div style={{ width: 500 }}>
                <TextInput
                  id="text-input-1"
                  type="text"
                  labelText="Customer number"
                  placeholder=""
                  value={enteredCustomerID}
                  onChange={enteredCustomerIDChangeHandler}
                />
              </div>

              <div style={{ width: 500 }}>
                <Dropdown
                  items2={["MORTGAGE", "ISA", "LOAN", "SAVING", "CURRENT"]}
                  id="default"
                  titleText="Account Type"
                  invalidText="Account Type is not valid"
                  label="Account Type"
                  items={["MORTGAGE", "ISA", "LOAN", "SAVING", "CURRENT"]}
                  onChange={({ selectedItem }) =>
                    setEnteredAccountType(selectedItem)
                  }

                  selectedItem={enteredAccountType}
                />
              </div>

              <div style={{ width: 500 }}>
                <TextInput
                  id="carbon-number"
                  label="Overdraft Limit:"
                  helperText="Please set the overdraft limit"
                  invalidText="Number is not valid"
                  value={enteredOverdraftLimit}
                  onChange={enteredOverdraftLimitChangeHandler}
                  hideSteppers
                />
              </div>
              <div style={{ width: 500 }}>
              <TextInput
                id="interestRate"
                label="Interest rate:"
                invalidText="Number is not valid"
                helperText="Please enter the interest rate"
                value={enteredInterestRate}
                onChange={enteredInterestRateChangeHandler}
              />
              </div>
              <Button className="displayModal" onClick={submitButtonHandler}>
                Submit
              </Button>
            </Stack>
          </Form>
          <Modal
            passiveModal
            size="sm"
            open={isModalOpened}
            onRequestClose={displayModal}
            preventCloseOnClickOutside>
            <h5>Customer account created successfully</h5>
            <br />
            <br />
            <p>Account Number: {successText}</p>
            <p>Sort Code: {sortCode}</p>
            <p>Account Type: {enteredAccountType}</p>
            <p>Overdraft Limit: {enteredOverdraftLimit}</p>
            <ModalFooter>
              <HeaderName
                className="white-background"
                element={Link}
                to="/Admin/account_details"
                prefix="View account details"
              />
            </ModalFooter>
          </Modal>
          <Modal
            passiveModal
            size="sm"
            open={isLoadingModalOpened}
            preventCloseOnClickOutside
            onRequestClose={displayLoadingModal}>
            <h4> Creating account...</h4>
          </Modal>
          <Modal
            passiveModal
            size="sm"
            open={isFailureNetworkModalOpened}
            preventCloseOnClickOutside
            onRequestClose={displayFailedNetworkModal}>
            <h4> Account failed to create due to a network error</h4>
          </Modal>
          <Modal
            passiveModal
            size="sm"
            open={isFailureModalOpened}
            onRequestClose={displayFailedModal}
            preventCloseOnClickOutside>
            <h5>Customer account failed to create</h5>
            <br />
            <br />
            <p>Please check that all inputs are valid.
              Alternatively, it may be that the customer has reached
              the maximum accounts allowed (10), you can check here:
            </p>
            <ModalFooter>
              <HeaderName
                className="white-background"
                element={Link}
                to="/Admin/customer_details"
                prefix="View customer details"
              />
            </ModalFooter>
          </Modal>
        </div>
        <div className="right-content-account">
          <img className="right-content-account"
            src={`${process.env.PUBLIC_URL}/ibm-db2-support-leadspace.png`}
            alt="account"
          />
        </div>
      </div>
      <Column
        lg={16}
        md={8}
        sm={4}
        className="landing-page__r3 bottom-Column"
      />
    </Grid>
  );
};

export default AccountCreationPage;
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
