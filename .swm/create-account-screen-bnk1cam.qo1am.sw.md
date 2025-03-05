---
title: Create Account Screen (BNK1CAM)
---
The Create Account screen (BNK1CAM) enables users to create new accounts for existing customers. It collects essential information such as customer number, account type, interest rate, overdraft limit, account number, sort code, account opening date, last statement date, and next statement date.

## Screen Preview

```
BNK1CA                  CICS Bank Sample Application- Create Account. 

Please provide the requested information and press Enter.


Customer number  : __________
Account Type     : ________
Interest Rate    : 0000.00
Overdraft Limit  : 0       


Account number   :         
Sort code        :      
Account Opened   : __/__/____
Last Stmt Date   : __/__/____
Next Stmt Date   : __/__/____
Available Balance:              
Actual Balance   :              



F3=Exit   F12=Cancel
```

## Fields

### Customer Number

The Customer Number field is a numeric input field with a length of 10 characters. It is a required field, indicated by the fixed text 'Customer number :' preceding it. The field is underlined and highlighted in green to indicate it's an input field. The field is validated to accept only numeric values.

### Account Type

The Account Type field is a character input field with a length of 8 characters. It is a required field, indicated by the fixed text 'Account Type :' preceding it. The field is underlined and highlighted in green to indicate it's an input field. The field is validated to accept only alphanumeric characters.

### Interest Rate

The Interest Rate field is a numeric input field with a length of 7 characters. It is a required field, indicated by the fixed text 'Interest Rate :' preceding it. The field is underlined and highlighted in green to indicate it's an input field. The field is validated to accept only numeric values with a maximum of 4 decimal places. The initial value is '0000.00'.

### Overdraft Limit

The Overdraft Limit field is a numeric input field with a length of 8 characters. It is a required field, indicated by the fixed text 'Overdraft Limit :' preceding it. The field is underlined and highlighted in green to indicate it's an input field. The field is validated to accept only numeric values. The initial value is '0'.

### Account Number

The Account Number field is a character input field with a length of 8 characters. It is a required field, indicated by the fixed text 'Account number :' preceding it. The field is not underlined or highlighted, indicating it's not an input field. The field is validated to accept only numeric values.

### Sort Code

The Sort Code field is a character input field with a length of 6 characters. It is a required field, indicated by the fixed text 'Sort code :' preceding it. The field is not underlined or highlighted, indicating it's not an input field. The field is validated to accept only numeric values.

### Account Opened Date

The Account Opened Date field is a date input field with a length of 6 characters. It is a required field, indicated by the fixed text 'Account Opened :' preceding it. The field is not underlined or highlighted, indicating it's not an input field. The field is validated to accept only date values in the format DD/MM/YYYY.

### Last Statement Date

The Last Statement Date field is a date input field with a length of 6 characters. It is a required field, indicated by the fixed text 'Last Stmt Date :' preceding it. The field is not underlined or highlighted, indicating it's not an input field. The field is validated to accept only date values in the format DD/MM/YYYY.

### Next Statement Date

The Next Statement Date field is a date input field with a length of 6 characters. It is a required field, indicated by the fixed text 'Next Stmt Date :' preceding it. The field is not underlined or highlighted, indicating it's not an input field. The field is validated to accept only date values in the format DD/MM/YYYY.

### Available Balance

The Available Balance field is a numeric display field with a length of 14 characters. It is a read-only field, indicated by the fixed text 'Available Balance:' preceding it. The field is not underlined or highlighted, indicating it's not an input field. The field displays the available balance for the account.

### Actual Balance

The Actual Balance field is a numeric display field with a length of 14 characters. It is a read-only field, indicated by the fixed text 'Actual Balance :' preceding it. The field is not underlined or highlighted, indicating it's not an input field. The field displays the actual balance for the account.

### 

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
