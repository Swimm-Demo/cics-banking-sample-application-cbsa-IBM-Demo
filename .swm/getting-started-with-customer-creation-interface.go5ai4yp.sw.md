---
title: Getting Started with Customer Creation Interface
---
## User Interface

# Getting Started with Customer Creation Interface

The Customer Creation Page is a component designed to facilitate the addition of new customers to the system. It includes various input fields such as full name, address, date of birth, and city, which are managed using React's state management.

## Submission Process

The page provides a user-friendly interface where users can enter the required customer details. It includes functions to handle changes in these input fields, ensuring that the state is updated accordingly.

## Server Interaction

Upon submission, the <SwmToken path="src/bank-application-frontend/src/content/CustomerCreationPage/CustomerCreationPage.js" pos="76:5:5" line-data="  async function createCustomer(){">`createCustomer`</SwmToken> function is invoked. This function validates that all required fields are filled. If any field is missing, a failure modal is displayed to inform the user.

## Response Handling

If all fields are correctly filled, the function concatenates the address and name components, sends a POST request to the server with the customer details, and handles the server's response.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerCreationPage/CustomerCreationPage.js" line="71">

---

Depending on the server's response, the page will display either a success modal or a failure modal. Additionally, a loading modal is shown while the request is being processed.

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
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
