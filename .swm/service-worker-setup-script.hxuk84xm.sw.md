---
title: Service Worker Setup Script
---
# Introduction

This document will walk you through the service worker setup script in the <SwmPath>[src/…/src/serviceWorker.js](src/bank-application-frontend/src/serviceWorker.js)</SwmPath> file.

The service worker is used to enhance the performance of the application by enabling offline capabilities and faster load times on subsequent visits.

We will cover:

1. The purpose of the service worker.
2. How the service worker is registered.
3. Handling different environments (localhost vs production).
4. Validating and registering the service worker.
5. Unregistering the service worker.

# Purpose of the service worker

<SwmSnippet path="/src/bank-application-frontend/src/serviceWorker.js" line="10">

---

The service worker allows the app to load faster on subsequent visits and provides offline capabilities. However, it requires all tabs to be closed to see updates, as cached resources are updated in the background.

```javascript
// This lets the app load faster on subsequent visits in production, and gives
// it offline capabilities. However, it also means that developers (and users)
// will only see deployed updates on subsequent visits to a page, after all the
// existing tabs open on the page have been closed, since previously cached
// resources are updated in the background.

// To learn more about the benefits of this model and instructions on how to
// opt-in, read https://bit.ly/CRA-PWA
```

---

</SwmSnippet>

# Registering the service worker

<SwmSnippet path="/src/bank-application-frontend/src/serviceWorker.js" line="29">

---

The <SwmToken path="src/bank-application-frontend/src/serviceWorker.js" pos="29:4:4" line-data="export function register(config) {">`register`</SwmToken> function is the main entry point for registering the service worker. It checks if the environment is production and if the browser supports service workers.

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
```

---

</SwmSnippet>

# Handling different environments

<SwmSnippet path="/src/bank-application-frontend/src/serviceWorker.js" line="19">

---

We determine if the app is running on localhost to handle service worker registration differently for development and production environments.

```javascript
const isLocalhost = Boolean(
  window.location.hostname === 'localhost' ||
    // [::1] is the IPv6 localhost address.
    window.location.hostname === '[::1]' ||
    // 127.0.0.1/8 is considered localhost for IPv4.
    window.location.hostname.match(
      /^127(?:\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$/
    )
);
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/serviceWorker.js" line="40">

---

If running on localhost, we check if a service worker already exists and log additional information for developers.

```javascript
    window.addEventListener('load', () => {
      const swUrl = `${process.env.PUBLIC_URL}/service-worker.js`;

      if (isLocalhost) {
        // This is running on localhost. Let's check if a service worker still exists or not.
        checkValidServiceWorker(swUrl, config);

        // Add some additional logging to localhost, pointing developers to the
        // service worker/PWA documentation.
        navigator.serviceWorker.ready.then(() => {
          console.log(
            'This web app is being served cache-first by a service ' +
              'worker. To learn more, visit https://bit.ly/CRA-PWA'
          );
        });
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/serviceWorker.js" line="55">

---

If not running on localhost, we proceed to register the service worker directly.

```javascript
      } else {
        // Is not localhost. Just register service worker
        registerValidSW(swUrl, config);
      }
    });
  }
}
```

---

</SwmSnippet>

# Validating and registering the service worker

<SwmSnippet path="/src/bank-application-frontend/src/serviceWorker.js" line="63">

---

The <SwmToken path="src/bank-application-frontend/src/serviceWorker.js" pos="63:2:2" line-data="function registerValidSW(swUrl, config) {">`registerValidSW`</SwmToken> function registers the service worker and handles updates. It logs messages to inform the user about the caching status and executes callbacks if provided.

```javascript
function registerValidSW(swUrl, config) {
  navigator.serviceWorker
    .register(swUrl)
    .then(registration => {
      registration.onupdatefound = () => {
        const installingWorker = registration.installing;
        if (installingWorker == null) {
          return;
        }
        installingWorker.onstatechange = () => {
          if (installingWorker.state === 'installed') {
            if (navigator.serviceWorker.controller) {
              // At this point, the updated precached content has been fetched,
              // but the previous service worker will still serve the older
              // content until all client tabs are closed.
              console.log(
                'New content is available and will be used when all ' +
                  'tabs for this page are closed. See https://bit.ly/CRA-PWA.'
              );

              // Execute callback
              if (config && config.onUpdate) {
                config.onUpdate(registration);
              }
            } else {
              // At this point, everything has been precached.
              // It's the perfect time to display a
              // "Content is cached for offline use." message.
              console.log('Content is cached for offline use.');

              // Execute callback
              if (config && config.onSuccess) {
                config.onSuccess(registration);
              }
            }
          }
        };
      };
    })
    .catch(error => {
      console.error('Error during service worker registration:', error);
    });
}
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/serviceWorker.js" line="107">

---

The <SwmToken path="src/bank-application-frontend/src/serviceWorker.js" pos="107:2:2" line-data="function checkValidServiceWorker(swUrl, config) {">`checkValidServiceWorker`</SwmToken> function ensures the service worker exists and is a valid JavaScript file. If not, it reloads the page.

```javascript
function checkValidServiceWorker(swUrl, config) {
  // Check if the service worker can be found. If it can't reload the page.
  fetch(swUrl)
    .then(response => {
      // Ensure service worker exists, and that we really are getting a JS file.
      const contentType = response.headers.get('content-type');
      if (
        response.status === 404 ||
        (contentType != null && contentType.indexOf('javascript') === -1)
      ) {
        // No service worker found. Probably a different app. Reload the page.
        navigator.serviceWorker.ready.then(registration => {
          registration.unregister().then(() => {
            window.location.reload();
          });
        });
      } else {
        // Service worker found. Proceed as normal.
        registerValidSW(swUrl, config);
      }
    })
    .catch(() => {
      console.log(
        'No internet connection found. App is running in offline mode.'
      );
    });
}
```

---

</SwmSnippet>

# Unregistering the service worker

<SwmSnippet path="/src/bank-application-frontend/src/serviceWorker.js" line="135">

---

The <SwmToken path="src/bank-application-frontend/src/serviceWorker.js" pos="135:4:4" line-data="export function unregister() {">`unregister`</SwmToken> function allows the service worker to be unregistered if needed.

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
