---
title: Customer Creation Page UI Component
---
# Introduction

This document will walk you through the implementation of the Customer Creation Page UI component in the bank application frontend.

The Customer Creation Page allows users to input customer details and create a new customer profile. We will cover:

1. State management for user inputs and modals.
2. Handling user input changes.
3. Customer creation logic.
4. Modal display logic.
5. Component structure and rendering.

# State management for user inputs and modals

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerCreationPage/CustomerCreationPage.js" line="28">

---

We define the state variables to manage user inputs and modal visibility. This is crucial for handling form data and controlling the display of different modals based on user actions and responses.

```javascript
const CustomerCreationPage = () => {

  /**
   * States for each of the customer details entered by the user + is the popup open
   */
  const [customerFullName, setFullName] = useState("")
  const [line1, setAddressLine1] = useState("")
  const [dateOfBirth, setDOB] = useState("")
  const [successText, setSuccessText] = useState("")
  const [city, setCity] = useState("")
  const [title, setTitle] = useState("")
  const [isSuccessModalOpened, setSuccessModalOpened] = useState(false);
  const [isFailureModalOpened, setFailureModalOpened] = useState(false);
  const [isFailureNetworkModalOpened, setFailureNetworkModalOpened] = useState(false);
  const [isLoadingModalOpened, setIsLoadingModalOpened] = useState(false)
```

---

</SwmSnippet>

# Handling user input changes

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerCreationPage/CustomerCreationPage.js" line="45">

---

We define functions to handle changes in user inputs. These functions update the corresponding state variables when the user interacts with the form fields.

```javascript
  function handleFullNameChange(e){
    setFullName(e.target.value);
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerCreationPage/CustomerCreationPage.js" line="57">

---

&nbsp;

```javascript
  function handleAddressLine1Change(e){
    setAddressLine1(e.target.value);
  }

  function handleDOBChange(e){
    let unformattedDOB = e.target.value
    let formattedDOB = unformattedDOB.substring(6,10) + "-" + unformattedDOB.substring(3,5) + "-" + unformattedDOB.substring(0,2)
    setDOB(formattedDOB);
  }

  function handleCityChange(e){
    setCity(e.target.value)
  }
```

---

</SwmSnippet>

# Customer creation logic

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerCreationPage/CustomerCreationPage.js" line="71">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerCreationPage/CustomerCreationPage.js" pos="76:5:5" line-data="  async function createCustomer(){">`createCustomer`</SwmToken> function checks if all required fields are filled. If not, it displays a failure modal. If the fields are filled, it sends a POST request to create the customer and displays the appropriate modal based on the response.

```javascript
  /**
   * Checks that all fields required have been filled in - if not a failure modal is shown.
   * If the fields have all been filled customerAddress and customerName are formed by concatenating their constituent parts
   * The request is sent and then a success/fail modal is shown depending on the response type
   */
  async function createCustomer(){
    if ((line1) === "" || (city) === "" || (title) === "" || (customerFullName) === ""){
      displayFailureModal()
    }
    else {
    let customerAddress = line1 + ", " + city
    let customerName =  title + " " + customerFullName
    let responseData;
      await axios
      .post(process.env.REACT_APP_CUSTOMER_URL, {
        customerAddress : customerAddress,
        dateOfBirth : dateOfBirth,
        sortCode : "987654",
        customerName : customerName
      }).then((response) => {
        console.log(response)
        responseData = response.data
        setSuccessText(parseInt(responseData.id))
        displayLoadingModal()
        displaySuccessModal();
      }).catch(function (error){
        if (error.response){
          console.log(error)
          displayLoadingModal()
          displayFailureModal()
        }
        else if (error.request) {
          console.log(error)
          displayLoadingModal()
          displayFailureNetworkModal()
        }
      });
    }
  }
```

---

</SwmSnippet>

# Modal display logic

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerCreationPage/CustomerCreationPage.js" line="49">

---

We define functions to toggle the visibility of different modals. These functions are used to provide feedback to the user during the customer creation process.

```javascript
  function displayLoadingModal(){
    setIsLoadingModalOpened(wasOpened => !wasOpened)
  }

  function displayFailureNetworkModal(){
    setFailureNetworkModalOpened(wasOpened => !wasOpened)
  }
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerCreationPage/CustomerCreationPage.js" line="117">

---

&nbsp;

```javascript
  /**
   * Show success modal toggle
   */
  function displaySuccessModal() {
    setSuccessModalOpened(wasOpened => !wasOpened);
  }

  /**
   * Show failure modal toggle
   */
  function displayFailureModal(){
    setFailureModalOpened(wasOpened => !wasOpened)
  }
```

---

</SwmSnippet>

# Component structure and rendering

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerCreationPage/CustomerCreationPage.js" line="131">

---

The component structure includes a form for user inputs and various modals for feedback. The form is divided into sections for different customer details, and the modals provide feedback on the success or failure of the customer creation process.

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
          <BreadcrumbItem>Create Customer</BreadcrumbItem>
        </Breadcrumb>
        <h1 className="landing-page__heading">Create new customer</h1>
      </Column>
      <div className="content-parent">
        <div className="left-content">
          <Column lg={16} md={8} sm={4} className="landing-page__r2">
            <Grid className="tabs-group-content">
              <Column md={4} lg={4} sm={4}>
                {/* left here */}
                <FormGroup
                  legendId="formgroup-legend-id"
                  style={{ maxWidth: '600px' }}
                  className="left-form">
                  <div style={{ marginBottom: '2rem' }}>
                    <Dropdown
                    items2={['Mr', 'Mrs', 'Miss', 'Ms', 'Dr', 'Drs', 'Professor', 'Sir', 'Lady', 'Lord']}
                    titleText="Title"
                    id="titleEntry"
                    labelText="Title"
                    items={['Mr', 'Mrs', 'Miss', 'Ms', 'Dr', 'Drs', 'Professor', 'Sir', 'Lady', 'Lord']}
                    onChange={({ selectedItem }) =>
                      setTitle(selectedItem)
                    }
                    selectedItem={title}
                    />
                    <TextInput
                      id="nameEntry"
                      labelText="Full name"
                      onChange={handleFullNameChange}
                    />
                     <DatePicker datePickerType="simple" dateFormat="Y-m-d">
                      <DatePickerInput
                        placeholder="dd-mm-yyyy"
                        labelText="Date of Birth"
                        id="date-of-birth"
                        onChange = {handleDOBChange}
                      />
                    </DatePicker>
                  </div>
                  <br />
                  <Modal
                    passiveModal
                    size="sm"
                    open={isSuccessModalOpened}
                    onRequestClose={displaySuccessModal}
                    preventCloseOnClickOutside>
                    <h5>Customer created successfully</h5>
                    <br />
                    <br />
                    <p> A customer profile for </p>
                    <div> {customerFullName} </div>
                    <p>has been created, their Customer ID is: </p>
                    <div> {parseInt(successText)}</div>
                  
                    <ModalFooter>
                      <HeaderName
                        className="white-background"
                        element={Link}
                        to="./customer_details"
                        prefix="View Customer Details"
                      />
                    </ModalFooter>
                  </Modal>
                  <Modal
            passiveModal
            size="sm"
            open={isLoadingModalOpened}
            preventCloseOnClickOutside
            onRequestClose={displayLoadingModal}>
            <h4> Creating customer...</h4>
          </Modal>
          <Modal
            passiveModal
            size="sm"
            open={isFailureNetworkModalOpened}
            preventCloseOnClickOutside
            onRequestClose={displayFailureNetworkModal}>
            <h4> Customer failed to create due to a network error</h4>
          </Modal>
                  <Modal
                  passiveModal
                  size="sm"
                  open={isFailureModalOpened}
                  onRequestClose={displayFailureModal}
                  preventCloseOnClickOutside
                  modalHeading="Customer creation unsuccessful">
                  <p> Please check that all fields have been filled </p>
                  </Modal>
                </FormGroup>
              </Column>
              <Column md={4} lg={4} sm={4}>
                {/* right here */}
                <FormGroup
                  legendId="formgroup-legend-id"
                  style={{ maxWidth: '600px' }}
                  className="right-form">
                  <div style={{ marginBottom: '2rem' }}>
                    <TextInput
                      id="addressLine1Entry"
                      labelText="Address line 1"
                      onChange={handleAddressLine1Change}
                    />
                    <TextInput
                      id="addressCityEntry"
                      labelText="City"
                      onChange={handleCityChange}
                    />
                  </div>
                  <div style={{marginTop: '20 px'}}></div>
                   <Button className="displayModal" onClick={buttonPress}>
                    Submit
                  </Button>
                </FormGroup>
              </Column>
            </Grid>
          </Column>
        </div>
        <div className="right-content">
          <img
            className="customer-img"
            src={`${process.env.PUBLIC_URL}/ibm-fireside-chat-trans-bus.png`}
            alt="customer"
          />
        </div>
      </div>
      <Column lg={16} md={8} sm={4} className="landing-page__r3" />
    </Grid>
  );
};

export default CustomerCreationPage;
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
