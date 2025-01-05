---
title: Exploring the Admin Page
---
# Functionalities

# Overview

The Admin page is a vital component that provides various administrative functionalities for managing customers and accounts.

# Usage

It includes options to create, delete, view, and update customer and account details.

<SwmSnippet path="/src/bank-application-frontend/src/content/AdminPage/AdminPage.js" line="28">

---

The Admin page is implemented in the <SwmPath>[src/…/AdminPage/AdminPage.js](src/bank-application-frontend/src/content/AdminPage/AdminPage.js)</SwmPath> file.

```javascript
const AdminPage = () => {
  const [isModalOpened, setModalOpened] = useState(false);
  const [isSuccessModalOpened, setSuccessModalOpened] = useState(false);

  function displayModal() {
    setModalOpened(wasOpened => !wasOpened);
  }

  function displaySuccessModal() {
    setSuccessModalOpened(wasOpened => !wasOpened);
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
