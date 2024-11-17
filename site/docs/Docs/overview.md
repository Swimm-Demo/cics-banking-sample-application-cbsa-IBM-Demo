---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: v2trixdx
---
# Overview

The CICS Bank Sample Application (CBSA) simulates the operation of a bank from the perspective of a Bank Teller. It serves multiple purposes including being a teaching aid, a conversation piece for application development lifecycle discussions, a testing tool for CICS interactions, and a foundation for application modernization conversations.

## Main Components

### Backend

```mermaid
graph TD;
 hequp("Retrieving Customer Accounts (INQACCCU)") --> rqv4x("Get Customer Record (INQCUST)")
 7euv9("Creating Customer (BNK1CCS)") --> 9p4ns("Processing Customer (CRECUST)")
 gytfk("Transferring Funds (BNK1TFN)") --> zv00e("Transferring Funds (XFRFUN)")
 6qco1("Creating Account (BNK1CAC)") --> tmrqp("Processing Account Creation (CREACC)")
 pn9qw("Displaying Customer (BNK1DCS)") --> yesic("Deleting Customer (DELCUS)")
 zat05("Credit/Debit (BNK1CRA)") --> 1blkq("Cash Transactions (DBCRFUN)")
 pn9qw("Displaying Customer (BNK1DCS)") --> rqv4x("Get Customer Record (INQCUST)")
 tmrqp("Processing Account Creation (CREACC)") --> rqv4x("Get Customer Record (INQCUST)")
 yesic("Deleting Customer (DELCUS)") --> rqv4x("Get Customer Record (INQCUST)")
 cpt79("Updating Account (BNK1UAC)") --> rvwb8("Retrieving Account (INQACC)")
 wuni5("Managing Account (BNK1DAC)") --> rvwb8("Retrieving Account (INQACC)")
 pn9qw("Displaying Customer (BNK1DCS)") --> 6zzjj("Updating Customer (UPDCUST)")
 wuni5("Managing Account (BNK1DAC)") --> ldapa("Deleting Account (DELACC)")
 yesic("Deleting Customer (DELCUS)") --> ldapa("Deleting Account (DELACC)")
 kdn5d("Listing Accounts (BNK1CCA)") --> hequp("Retrieving Customer Accounts (INQACCCU)")
 tmrqp("Processing Account Creation (CREACC)") --> hequp("Retrieving Customer Accounts (INQACCCU)")
 yesic("Deleting Customer (DELCUS)") --> hequp("Retrieving Customer Accounts (INQACCCU)")
```

#### Deleting Customer (DELCUS)

Deleting Customer (DELCUS) is a COBOL program designed to handle the deletion of a customer and their associated accounts. The process involves retrieving all accounts linked to the customer, deleting each account individually, and recording these deletions. Once all accounts are deleted, the customer record itself is deleted, and this deletion is also recorded. The program ensures data consistency by aborting the operation if any failure occurs during the deletion process, except when an account has already been deleted.

- <SwmLink doc-title="Deleting Customer (DELCUS)">[Deleting Customer (DELCUS)](/docs/Docs/deleting-customer-delcus.md)</SwmLink>

#### Creating Account (BNK1CAC)

Creating Account (BNK1CAC) involves verifying input data and linking to the `CREACC` program to add the new account to the account datastore.

- <SwmLink doc-title="Creating Account (BNK1CAC)">[Creating Account (BNK1CAC)](/docs/Docs/creating-account-bnk1cac.md)</SwmLink>

#### Managing Account (BNK1DAC)

Managing Account (BNK1DAC) is a COBOL program that handles displaying account information and processing account deletion requests. It interacts with CICS for transaction processing and uses BMS for screen handling. The program validates input data, retrieves account details, and performs account deletion by linking to the DELACC subprogram. It also manages user interactions through various function keys and ensures proper error handling and messaging throughout the process.

- <SwmLink doc-title="Managing Account (BNK1DAC)">[Managing Account (BNK1DAC)](/docs/Docs/managing-account-bnk1dac.md)</SwmLink>

#### Transferring Funds (BNK1TFN)

Transferring Funds (BNK1TFN) refers to the functionality that allows the transfer of funds between accounts within the same bank. This process involves validating the input data, such as account numbers and transfer amounts, ensuring that the accounts are different and valid, and then performing the transfer operation. The program handles various user interactions, including sending maps, processing data, and handling errors or invalid inputs. It ensures that the transfer is executed correctly and updates the account balances accordingly.

- <SwmLink doc-title="Transferring Funds (BNK1TFN)">[Transferring Funds (BNK1TFN)](/docs/Docs/transferring-funds-bnk1tfn.md)</SwmLink>

#### Get Customer Record (INQCUST)

INQCUST is a COBOL program that takes a customer number as input and returns a communication area containing all the customer information for that record. If the customer is found, the data is returned; otherwise, a record with low values is returned. In case of any issues, an appropriate abend is issued.

- <SwmLink doc-title="Get Customer Record (INQCUST)">[Get Customer Record (INQCUST)](/docs/Docs/get-customer-record-inqcust.md)</SwmLink>

#### Credit/Debit (BNK1CRA)

Credit/Debit (BNK1CRA) refers to a COBOL program within the CICS Bank Sample Application that handles credit and debit transactions. It processes user inputs, validates transaction amounts, and updates account balances accordingly. The program interacts with CICS for transaction management and uses BMS for screen handling. It includes various sections for data validation, error handling, and communication with other programs to ensure accurate and secure transaction processing.

- <SwmLink doc-title="Credit/Debit (BNK1CRA)">[Credit/Debit (BNK1CRA)](/docs/Docs/creditdebit-bnk1cra.md)</SwmLink>

#### Cash Transactions (DBCRFUN)

Cash Transactions (DBCRFUN) handle the process of depositing or withdrawing cash over the counter. The program takes an account number and amount, accesses the DB2 datastore to retrieve the associated account record, applies the transaction, and returns the updated balance. If the update is successful, a record is written to the PROCTRAN datastore. If the transaction fails, a failure code is returned for the calling routine to handle.

- <SwmLink doc-title="Cash Transactions (DBCRFUN)">[Cash Transactions (DBCRFUN)](/docs/Docs/cash-transactions-dbcrfun.md)</SwmLink>

#### Processing Account Creation (CREACC)

Processing Account Creation (CREACC) involves taking account information from the Bank Management System (BMS) application, such as customer number, name, address, and date of birth. The process includes enqueuing a named counter for the account, incrementing the counter to generate a new account number, and updating the account datastore on Db2. If the update is successful, a record is written to the PROCTRAN datastore. If any part of the process fails, the named counter is decremented to its original position, and the counter is dequeued. The final step is to return the new account number if all operations are successful.

- <SwmLink doc-title="Processing Account Creation (CREACC)">[Processing Account Creation (CREACC)](/docs/Docs/processing-account-creation-creacc.md)</SwmLink>

#### Retrieving Account (INQACC)

Retrieving Account (INQACC) involves accessing the DB2 datastore to fetch account details using an account number and account type. The program handles errors by abending if issues occur during the retrieval process.

- <SwmLink doc-title="Retrieving Account (INQACC)">[Retrieving Account (INQACC)](/docs/Docs/retrieving-account-inqacc.md)</SwmLink>

#### Transferring Funds (XFRFUN)

Transferring Funds (XFRFUN) refers to the process of moving money from one account to another. This involves taking the sort code and account number of both the source and destination accounts, along with the transfer amount. The program updates the account balances and records the transaction in the PROCTRAN datastore. If any part of the process fails, the transaction is rolled back to maintain data integrity.

- <SwmLink doc-title="Transferring Funds (XFRFUN)">[Transferring Funds (XFRFUN)](/docs/Docs/transferring-funds-xfrfun.md)</SwmLink>

#### Listing Accounts (BNK1CCA)

Listing Accounts (BNK1CCA) refers to the functionality that lists all accounts associated with a specified customer number. This COBOL program, identified as BNK1CCA, retrieves and displays account information for a customer by interacting with the CICS system. It validates the customer number, retrieves the relevant account data, and handles various user inputs to display the account details or appropriate messages.

- <SwmLink doc-title="Listing Accounts (BNK1CCA)">[Listing Accounts (BNK1CCA)](/docs/Docs/listing-accounts-bnk1cca.md)</SwmLink>

#### Updating Customer (UPDCUST)

Updating Customer (UPDCUST) refers to the process of modifying customer details in the system. The program receives customer information as input, validates the fields, and updates the relevant records in the VSAM datastore. Only specific fields are allowed to be changed, and the presentation layer ensures data validation. If the update fails, a failure flag is returned to the calling program.

- <SwmLink doc-title="Updating Customer (UPDCUST)">[Updating Customer (UPDCUST)](/docs/Docs/updating-customer-updcust.md)</SwmLink>

#### Deleting Account (DELACC)

Deleting Account (DELACC) refers to the process of removing an account from the datastore. The program takes an incoming account number, retrieves the associated account record by matching the customer number and account type, and then deletes it. If no matching record is found, it returns an error flag for the visualization layer to process. Any issues during the process will cause the program to abend. It is assumed that the incoming customer number is valid.

- <SwmLink doc-title="Deleting Account (DELACC)">[Deleting Account (DELACC)](/docs/Docs/deleting-account-delacc.md)</SwmLink>

#### Creating Customer (BNK1CCS)

Creating Customer (BNK1CCS) refers to the process of adding a new customer to the banking application. This involves capturing customer details through a user interface, validating the input data, and then storing the customer information in the database. The program handles various user interactions, such as pressing function keys or entering data, and ensures that the data is correctly formatted and validated before creating the customer record. It also manages terminal settings and error handling to ensure a smooth user experience.

- <SwmLink doc-title="Creating Customer (BNK1CCS)">[Creating Customer (BNK1CCS)](/docs/Docs/creating-customer-bnk1ccs.md)</SwmLink>

#### Main Menu (BNKMENU)

The Main Menu (BNKMENU) is the initial program that displays the main menu to the user, validates input, and initiates corresponding transactions based on user selections.

- <SwmLink doc-title="Main Menu (BNKMENU)">[Main Menu (BNKMENU)](/docs/Docs/main-menu-bnkmenu.md)</SwmLink>

#### Displaying Customer (BNK1DCS)

Displaying Customer (BNK1DCS) is a COBOL program within the banking application that handles the display, update, and deletion of customer records. It interacts with the user through a BMS map, allowing the user to view customer details, delete a customer record by pressing PF5, or update a customer record by pressing PF10. The program includes various validation checks and processes to ensure data integrity and proper handling of user inputs.

- <SwmLink doc-title="Displaying Customer (BNK1DCS)">[Displaying Customer (BNK1DCS)](/docs/Docs/displaying-customer-bnk1dcs.md)</SwmLink>

#### Initializing Data (BANKDATA)

Initializing Data (BANKDATA) involves running a batch program that generates and populates the necessary data for the banking application. This includes creating customer records in a VSAM file and account records in a DB2 table, using specified parameters to control the range and randomness of the generated data.

- <SwmLink doc-title="Initializing Data (BANKDATA)">[Initializing Data (BANKDATA)](/docs/Docs/initializing-data-bankdata.md)</SwmLink>

#### Updating Account (BNK1UAC)

Updating Account (BNK1UAC) refers to the COBOL program responsible for handling account updates within the CICS Bank Sample Application. This program validates incoming data, processes updates to account information, and interacts with other components to ensure data integrity and proper transaction handling. It includes sections for receiving and validating data, updating account details, and handling errors or abnormal terminations. The program ensures that account information is accurately updated and that any issues are properly logged and managed.

- <SwmLink doc-title="Updating Account (BNK1UAC)">[Updating Account (BNK1UAC)](/docs/Docs/updating-account-bnk1uac.md)</SwmLink>

#### Processing Customer (CRECUST)

Processing Customer (CRECUST) involves taking customer information from a BMS application, determining the appropriate datastore (VSAM or DB2), performing asynchronous credit checks with multiple agencies, and updating the customer and transaction records. If any step fails, the process ensures data integrity by rolling back changes.

- <SwmLink doc-title="Processing Customer (CRECUST)">[Processing Customer (CRECUST)](/docs/Docs/processing-customer-crecust.md)</SwmLink>

#### Retrieving Customer Accounts (INQACCCU)

Retrieving Customer Accounts (INQACCCU) involves taking an incoming customer number, accessing the datastore, and retrieving the associated account records that match the customer number.

- <SwmLink doc-title="Retrieving Customer Accounts (INQACCCU)">[Retrieving Customer Accounts (INQACCCU)](/docs/Docs/retrieving-customer-accounts-inqacccu.md)</SwmLink>

#### Programs

- <SwmLink doc-title="Handling Abnormal Terminations (ABNDPROC)">[Handling Abnormal Terminations (ABNDPROC)](/docs/Docs/handling-abnormal-terminations-abndproc.md)</SwmLink>
- <SwmLink doc-title="Updating Account Information (UPDACC)">[Updating Account Information (UPDACC)](/docs/Docs/updating-account-information-updacc.md)</SwmLink>
- <SwmLink doc-title="Managing Account Control Operations (ACCTCTRL)">[Managing Account Control Operations (ACCTCTRL)](/docs/Docs/managing-account-control-operations-acctctrl.md)</SwmLink>
- <SwmLink doc-title="Managing Customer Data (CUSTCTRL)">[Managing Customer Data (CUSTCTRL)](/docs/Docs/managing-customer-data-custctrl.md)</SwmLink>
- <SwmLink doc-title="Generating and Storing Credit Scores (CRDTAGY2)">[Generating and Storing Credit Scores (CRDTAGY2)](/docs/Docs/generating-and-storing-credit-scores-crdtagy2.md)</SwmLink>
- <SwmLink doc-title="Generating Credit Score (CRDTAGY3)">[Generating Credit Score (CRDTAGY3)](/docs/Docs/generating-credit-score-crdtagy3.md)</SwmLink>
- <SwmLink doc-title="Setting Sort Code (GETSCODE)">[Setting Sort Code (GETSCODE)](/docs/Docs/setting-sort-code-getscode.md)</SwmLink>
- <SwmLink doc-title="Credit Score Generation and Update (CRDTAGY4)">[Credit Score Generation and Update (CRDTAGY4)](/docs/Docs/credit-score-generation-and-update-crdtagy4.md)</SwmLink>
- <SwmLink doc-title="Handling Delay and Error Management (CRDTAGY5)">[Handling Delay and Error Management (CRDTAGY5)](/docs/Docs/handling-delay-and-error-management-crdtagy5.md)</SwmLink>
- <SwmLink doc-title="Initializing Company Name (GETCOMPY)">[Initializing Company Name (GETCOMPY)](/docs/Docs/initializing-company-name-getcompy.md)</SwmLink>
- <SwmLink doc-title="Generating and Storing Credit Scores (CRDTAGY1)">[Generating and Storing Credit Scores (CRDTAGY1)](/docs/Docs/generating-and-storing-credit-scores-crdtagy1.md)</SwmLink>

### Bank Frontend

- **Customer details page**
  - <SwmLink doc-title="Customer Details Page in Bank Frontend">[Customer Details Page in Bank Frontend](/docs/Docs/customer-details-page-in-bank-frontend.md)</SwmLink>
  - **Customer details page**
    - <SwmLink doc-title="Getting Started with Customer Details Page">[Getting Started with Customer Details Page](/docs/Docs/getting-started-with-customer-details-page.md)</SwmLink>
- **Customer delete page**
  - <SwmLink doc-title="Exploring Customer Deletion Page">[Exploring Customer Deletion Page](/docs/Docs/exploring-customer-deletion-page.md)</SwmLink>
- **Account delete page**
  - <SwmLink doc-title="Exploring the Account Deletion Page">[Exploring the Account Deletion Page](/docs/Docs/exploring-the-account-deletion-page.md)</SwmLink>
- **Account details page**
  - <SwmLink doc-title="Getting Started with Account Details Page">[Getting Started with Account Details Page](/docs/Docs/getting-started-with-account-details-page.md)</SwmLink>

### Payment Interface

The Payment Interface is a Spring Boot application that allows trusted companies to process payments and issue refunds directly from customer accounts at CBSA Bank via RESTful API calls to a zOS Connect server, which routes the requests to a CICS region.

- <SwmLink doc-title="Payment Interface Overview">[Payment Interface Overview](/docs/Docs/payment-interface-overview.md)</SwmLink>
- **Jsonclasses**
  - <SwmLink doc-title="Overview of Jsonclasses in Payment Interface">[Overview of Jsonclasses in Payment Interface](/docs/Docs/overview-of-jsonclasses-in-payment-interface.md)</SwmLink>
- **Build tools**
  - <SwmLink doc-title="Building the Payment Interface with Maven">[Building the Payment Interface with Maven](/docs/Docs/building-the-payment-interface-with-maven.md)</SwmLink>

### Web UI

Web UI refers to the web interface component of the CBSA, built using WebSphere Liberty Profile, and includes all necessary web content and assets.

- <SwmLink doc-title="Web UI Overview">[Web UI Overview](/docs/Docs/web-ui-overview.md)</SwmLink>
- **Datainterfaces**
  - **Proctran**
    - <SwmLink doc-title="Getting Started with Processed Transaction Data Interface">[Getting Started with Processed Transaction Data Interface](/docs/Docs/getting-started-with-processed-transaction-data-interface.md)</SwmLink>
  - **Customer control**
    - <SwmLink doc-title="Exploring Customer Management">[Exploring Customer Management](/docs/Docs/exploring-customer-management.md)</SwmLink>
  - **Customer**
    - <SwmLink doc-title="Customer Data Management Overview">[Customer Data Management Overview](/docs/Docs/customer-data-management-overview.md)</SwmLink>
  - **Crecust**
    - <SwmLink doc-title="Overview of Customer Data Interface">[Overview of Customer Data Interface](/docs/Docs/overview-of-customer-data-interface.md)</SwmLink>
- **Webui**
  - **Account list**
    - <SwmLink doc-title="Getting Started with Account List in WebUI">[Getting Started with Account List in WebUI](/docs/Docs/getting-started-with-account-list-in-webui.md)</SwmLink>
  - **Account**
    - <SwmLink doc-title="Overview of Account Class">[Overview of Account Class](/docs/Docs/overview-of-account-class.md)</SwmLink>
  - **Customer**
    - <SwmLink doc-title="Customer Management in Webui">[Customer Management in Webui](/docs/Docs/customer-management-in-webui.md)</SwmLink>
  - **Customer list**
    - <SwmLink doc-title="Customer List Feature Overview">[Customer List Feature Overview](/docs/Docs/customer-list-feature-overview.md)</SwmLink>
- **Api**
  - **Customer resource**
    - <SwmLink doc-title="Customer Resource in API">[Customer Resource in API](/docs/Docs/customer-resource-in-api.md)</SwmLink>
    - **Flows**
      - <SwmLink doc-title="Filtering and Counting Customers">[Filtering and Counting Customers](/docs/Docs/filtering-and-counting-customers.md)</SwmLink>
      - <SwmLink doc-title="Customer Deletion Process">[Customer Deletion Process](/docs/Docs/customer-deletion-process.md)</SwmLink>
      - <SwmLink doc-title="Updating Customer Information Flow">[Updating Customer Information Flow](/docs/Docs/updating-customer-information-flow.md)</SwmLink>
  - **Processed transaction resource**
    - <SwmLink doc-title="Basic Concepts of Processed Transaction Resource">[Basic Concepts of Processed Transaction Resource](/docs/Docs/basic-concepts-of-processed-transaction-resource.md)</SwmLink>
  - **Accounts resource**
    - <SwmLink doc-title="Exploring Accounts Resource in API">[Exploring Accounts Resource in API](/docs/Docs/exploring-accounts-resource-in-api.md)</SwmLink>
    - **Classes**
      - <SwmLink doc-title="The AccountsResource class">[The AccountsResource class](/docs/Docs/the-accountsresource-class.md)</SwmLink>
  - **Flows**
    - <SwmLink doc-title="Account Deletion Process">[Account Deletion Process](/docs/Docs/account-deletion-process.md)</SwmLink>
    - <SwmLink doc-title="Handling Debit and Credit Transactions">[Handling Debit and Credit Transactions](/docs/Docs/handling-debit-and-credit-transactions.md)</SwmLink>
    - <SwmLink doc-title="Retrieving Customer Account Information">[Retrieving Customer Account Information](/docs/Docs/retrieving-customer-account-information.md)</SwmLink>
    - <SwmLink doc-title="Creating a New Bank Account">[Creating a New Bank Account](/docs/Docs/creating-a-new-bank-account.md)</SwmLink>
    - <SwmLink doc-title="Updating Account Details Flow">[Updating Account Details Flow](/docs/Docs/updating-account-details-flow.md)</SwmLink>
    - <SwmLink doc-title="Determining the Number of Accounts">[Determining the Number of Accounts](/docs/Docs/determining-the-number-of-accounts.md)</SwmLink>
    - <SwmLink doc-title="Adding a Customer to the Database">[Adding a Customer to the Database](/docs/Docs/adding-a-customer-to-the-database.md)</SwmLink>
    - <SwmLink doc-title="Transferring Funds Between Accounts">[Transferring Funds Between Accounts](/docs/Docs/transferring-funds-between-accounts.md)</SwmLink>
    - <SwmLink doc-title="Transaction Data Retrieval and Processing Flow">[Transaction Data Retrieval and Processing Flow](/docs/Docs/transaction-data-retrieval-and-processing-flow.md)</SwmLink>
    - <SwmLink doc-title="Deleting a Bank Account Flow">[Deleting a Bank Account Flow](/docs/Docs/deleting-a-bank-account-flow.md)</SwmLink>
    - <SwmLink doc-title="Creating a New Account Flow">[Creating a New Account Flow](/docs/Docs/creating-a-new-account-flow.md)</SwmLink>
    - <SwmLink doc-title="Creating a New Customer Flow">[Creating a New Customer Flow](/docs/Docs/creating-a-new-customer-flow.md)</SwmLink>
    - <SwmLink doc-title="Deleting a Customer Record Flow">[Deleting a Customer Record Flow](/docs/Docs/deleting-a-customer-record-flow.md)</SwmLink>
    - <SwmLink doc-title="Handling Local Transfer Requests">[Handling Local Transfer Requests](/docs/Docs/handling-local-transfer-requests.md)</SwmLink>
- **Web**
  - **Account**
    - <SwmLink doc-title="Exploring the Account Class">[Exploring the Account Class](/docs/Docs/exploring-the-account-class.md)</SwmLink>
  - **Processed transaction**
    - <SwmLink doc-title="Introduction to Processed Transactions">[Introduction to Processed Transactions](/docs/Docs/introduction-to-processed-transactions.md)</SwmLink>
- **Build tools**
  - <SwmLink doc-title="Building the WebUI with Maven">[Building the WebUI with Maven](/docs/Docs/building-the-webui-with-maven.md)</SwmLink>
- **Flows**
  - <SwmLink doc-title="Handling Debit and Credit Operations">[Handling Debit and Credit Operations](/docs/Docs/handling-debit-and-credit-operations.md)</SwmLink>
  - <SwmLink doc-title="Retrieving Account Information Flow">[Retrieving Account Information Flow](/docs/Docs/retrieving-account-information-flow.md)</SwmLink>
  - <SwmLink doc-title="Customer Data Retrieval Flow">[Customer Data Retrieval Flow](/docs/Docs/customer-data-retrieval-flow.md)</SwmLink>

### Customer Services Interface

- **Controllers**
  - <SwmLink doc-title="Exploring Customer Services Controllers">[Exploring Customer Services Controllers](/docs/Docs/exploring-customer-services-controllers.md)</SwmLink>
- **Jsonclasses**
  - **Updatecustomer**
    - <SwmLink doc-title="Basic Concepts of UpdateCustomerJson">[Basic Concepts of UpdateCustomerJson](/docs/Docs/basic-concepts-of-updatecustomerjson.md)</SwmLink>
  - **Listaccounts**
    - <SwmLink doc-title="Introduction to ListAccJson">[Introduction to ListAccJson](/docs/Docs/introduction-to-listaccjson.md)</SwmLink>
  - **Createaccount**
    - <SwmLink doc-title="Introduction to CreateAccountJson">[Introduction to CreateAccountJson](/docs/Docs/introduction-to-createaccountjson.md)</SwmLink>
  - **Customerenquiry**
    - <SwmLink doc-title="CustomerEnquiryJson Overview">[CustomerEnquiryJson Overview](/docs/Docs/customerenquiryjson-overview.md)</SwmLink>
  - **Deleteaccount**
    - <SwmLink doc-title="Exploring DeleteAccountJson">[Exploring DeleteAccountJson](/docs/Docs/exploring-deleteaccountjson.md)</SwmLink>
  - **Createcustomer**
    - <SwmLink doc-title="Introduction to CreateCustomerJson">[Introduction to CreateCustomerJson](/docs/Docs/introduction-to-createcustomerjson.md)</SwmLink>
  - **Updateaccount**
    - <SwmLink doc-title="Overview of UpdateAccountJson">[Overview of UpdateAccountJson](/docs/Docs/overview-of-updateaccountjson.md)</SwmLink>
  - **Accountenquiry**
    - <SwmLink doc-title="Introduction to Account Enquiry">[Introduction to Account Enquiry](/docs/Docs/introduction-to-account-enquiry.md)</SwmLink>
- **Build tools**
  - <SwmLink doc-title="Building the Z-OS Connect Customer Services Interface with Maven">[Building the Z-OS Connect Customer Services Interface with Maven](/docs/Docs/building-the-z-os-connect-customer-services-interface-with-maven.md)</SwmLink>

&nbsp;

### Cross-component Flows

- <SwmLink doc-title="Creating a customer">[Creating a customer](/docs/Docs/creating-a-customer.md)</SwmLink>
- <SwmLink doc-title="Updating account flow">[Updating account flow](/docs/Docs/updating-account-flow.md)</SwmLink>

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
