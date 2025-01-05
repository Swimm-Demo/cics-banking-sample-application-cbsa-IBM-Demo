---
title: Bank Frontend Overview
---
## Functionalities

# Bank Frontend Overview

The Bank Frontend is a user interface designed to simulate the operations of a bank from the perspective of a Bank Teller.

## Design and Components

It provides various functionalities such as making payments, creating new customers and accounts, viewing or updating existing customer and account details, and deleting customers and accounts.

## <SwmToken path="src/bank-application-frontend/src/App.js" pos="62:15:15" line-data="            &lt;Route exact path=&quot;./&quot; component={HomePage} /&gt;">`HomePage`</SwmToken>

The frontend is built using Carbon React UI components, ensuring a modern and responsive design. It includes multiple pages such as the <SwmToken path="src/bank-application-frontend/src/App.js" pos="62:15:15" line-data="            &lt;Route exact path=&quot;./&quot; component={HomePage} /&gt;">`HomePage`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/App.js" pos="74:4:4" line-data="              component={CustomerDetailsPage}">`CustomerDetailsPage`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/App.js" pos="70:4:4" line-data="              component={AccountCreationPage}">`AccountCreationPage`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/App.js" pos="13:2:2" line-data="import AdminPage from &#39;./content/AdminPage&#39;;">`AdminPage`</SwmToken>, and others, each serving specific functionalities.

## Routing

The <SwmToken path="src/bank-application-frontend/src/App.js" pos="62:15:15" line-data="            &lt;Route exact path=&quot;./&quot; component={HomePage} /&gt;">`HomePage`</SwmToken> serves as the landing page, welcoming users and providing navigation to other parts of the application.

## Usage Example

Routing within the application is managed using React Router, allowing smooth navigation between different pages and components.

<SwmSnippet path="/src/bank-application-frontend/src/App.js" line="62">

---

An example of the Bank Frontend usage can be seen in the <SwmPath>[src/…/src/App.js](src/bank-application-frontend/src/App.js)</SwmPath> file where different routes are defined for various pages like <SwmToken path="src/bank-application-frontend/src/App.js" pos="66:4:4" line-data="              component={CustomerCreationPage}">`CustomerCreationPage`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/App.js" pos="70:4:4" line-data="              component={AccountCreationPage}">`AccountCreationPage`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/App.js" pos="74:4:4" line-data="              component={CustomerDetailsPage}">`CustomerDetailsPage`</SwmToken>, and others.

```javascript
            <Route exact path="./" component={HomePage} />

            <Route
              path="/Admin/customer_creation"
              component={CustomerCreationPage}
            />
            <Route
              path="/Admin/account_creation"
              component={AccountCreationPage}
            />
            <Route
              path="/Admin/customer_details"
              component={CustomerDetailsPage}
            />
            <Route
              path="/Admin/account_details"
              component={AccountDetailsPage}
            />
            <Route
              path="/Admin/customer_deletion"
              component={CustomerDeletePage}
```

---

</SwmSnippet>

## Building the Project

# Build, Run, and Debug

To build, run, and debug the project, follow these steps:

<SwmSnippet path="/src/bank-application-frontend/package.json" line="10">

---

To build the project, you need to execute the `npm run `<SwmToken path="src/bank-application-frontend/package.json" pos="10:2:2" line-data="    &quot;build&quot;: &quot;react-scripts build&quot;,">`build`</SwmToken> command. This will compile the project and prepare it for production deployment.

```json
    "build": "react-scripts build",
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/package.json" line="17">

---

## Running the Project

To run the project, you need to execute the `npm `<SwmToken path="src/bank-application-frontend/package.json" pos="17:2:2" line-data="    &quot;start&quot;: &quot;react-scripts start&quot;,">`start`</SwmToken> command. This will start the development server, and you can navigate to `http://localhost:4200/` to view the application.

```json
    "start": "react-scripts start",
```

---

</SwmSnippet>

# Service Worker APIs

## Debugging the Project

To debug the project, you can use the <SwmToken path="src/bank-application-frontend/package.json" pos="10:7:9" line-data="    &quot;build&quot;: &quot;react-scripts build&quot;,">`react-scripts`</SwmToken> built-in debugging tools. Running `npm `<SwmToken path="src/bank-application-frontend/package.json" pos="17:2:2" line-data="    &quot;start&quot;: &quot;react-scripts start&quot;,">`start`</SwmToken> will automatically reload the application if you change any of the source files, making it easier to debug and test changes in real-time.

## Register

Service Worker APIs enhance the performance and offline capabilities of the application.

<SwmSnippet path="/src/bank-application-frontend/src/serviceWorker.js" line="29">

---

The <SwmToken path="src/bank-application-frontend/src/serviceWorker.js" pos="29:4:4" line-data="export function register(config) {">`register`</SwmToken> function is used to register a service worker, which helps in loading the app faster on subsequent visits and provides offline capabilities. It checks if the environment is production and if the service worker is supported by the browser. If the app is running on localhost, it verifies if a service worker already exists; otherwise, it registers a new service worker.

```javascript
export function register(config) {
  if (process.env.NODE_ENV === 'production' && 'serviceWorker' in navigator) {
    // The URL constructor is available in all browsers that support SW.
    const publicUrl = new URL(process.env.PUBLIC_URL, window.location.href);
    if (publicUrl.origin !== window.location.origin) {
      // Our service worker won't work if PUBLIC_URL is on a different origin
      // from what our page is served on. This might happen if a CDN is used to
      // serve assets; see https://github.com/facebook/create-react-app/issues/2374
      return;
    }

    window.addEventListener('load', () => {
      const swUrl = `${process.env.PUBLIC_URL}/service-worker.js`;

      if (isLocalhost) {
        // This is running on localhost. Let's check if a service worker still exists or not.
        checkValidServiceWorker(swUrl, config);

        // Add some additional logging to localhost, pointing developers to the
        // service worker/PWA documentation.
        navigator.serviceWorker.ready.then(() => {
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/serviceWorker.js" line="135">

---

## Unregister

The <SwmToken path="src/bank-application-frontend/src/serviceWorker.js" pos="135:4:4" line-data="export function unregister() {">`unregister`</SwmToken> function is used to unregister an existing service worker. This is useful when you want to remove the service worker and its associated cached data from the user's browser.

```javascript
export function unregister() {
  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.ready.then(registration => {
      registration.unregister();
    });
  }
}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
