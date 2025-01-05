---
title: HBankDataAccess Class Definition
---
# Introduction

This document will walk you through the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="31:4:4" line-data="public class HBankDataAccess">`HBankDataAccess`</SwmToken> class definition.

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="31:4:4" line-data="public class HBankDataAccess">`HBankDataAccess`</SwmToken> class is designed to manage <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="71:13:13" line-data="		// Open a connection to the DB2 database">`DB2`</SwmToken> database connections efficiently. It uses a <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="11:6:6" line-data="import java.util.HashMap;">`HashMap`</SwmToken> to store and reuse connections, ensuring that each task has a unique connection. This approach minimizes the overhead of repeatedly opening and closing connections.

We will cover:

1. Class initialization and logging setup
2. Connection management
3. Connection lifecycle methods

# Class initialization and logging setup

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="26">

---

The class is defined to hold a <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="11:6:6" line-data="import java.util.HashMap;">`HashMap`</SwmToken> for <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="71:13:13" line-data="		// Open a connection to the DB2 database">`DB2`</SwmToken> connections.

```java
/**
 * This class is used to hold a HashTable which in turn holds Db2 Connections
 * 
 */

public class HBankDataAccess
{
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="35">

---

We initialize the connection and the connection count.

```java
	protected Connection conn = null;

	static int connectionCount = 0;
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="44">

---

A static logger is set up for logging purposes.

```java
	private static Logger logger = Logger
			.getLogger("com.ibm.cics.cip.bankliberty.api.json");
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="48">

---

The constructor initializes logging and creates the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="11:6:6" line-data="import java.util.HashMap;">`HashMap`</SwmToken> if it doesn't exist.

```java
	public HBankDataAccess()
	{
		sortOutLogging();
		// If the hashmap 'cornedBeef' does not exist then create a new
		// hashtable
		if (cornedBeef == null)
		{
			logger.log(Level.FINE,
					() -> "HBankDataAccess creating new hashtable");
			HBankDataAccess.createHashMap();
		}
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="209">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="209:5:5" line-data="	protected void sortOutLogging()">`sortOutLogging`</SwmToken> method configures the logging settings.

```java
	protected void sortOutLogging()
	{
		try
		{
			LogManager.getLogManager().readConfiguration();
		}
		catch (SecurityException | IOException e)
		{
			logger.severe(e.toString());
		}
	}
```

---

</SwmSnippet>

# Connection management

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="69">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="69:5:5" line-data="	protected void openConnection()">`openConnection`</SwmToken> method is responsible for opening a connection to the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="71:13:13" line-data="		// Open a connection to the DB2 database">`DB2`</SwmToken> database. It checks if a connection already exists for the current task and reuses it if possible.

```java
	protected void openConnection()
	{
		// Open a connection to the DB2 database
		logger.entering(this.getClass().getName(), "openConnection()");
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="74">

---

If no connection exists, it attempts to create a new one.

```java
		Integer taskNumberInteger = Task.getTask().getTaskNumber();
		String db2ConnString = DB2CONN.concat(taskNumberInteger.toString());
		logger.log(Level.FINE,
				() -> "Attempting to get DB2CONN for task number "
						+ taskNumberInteger.toString());
		this.conn = (Connection) cornedBeef.get(db2ConnString);
		if (this.conn == null)
		{
			HBankDataAccess.incrementConnCount();
			logger.log(Level.FINE,
					() -> "Attempting to create DB2CONN for task number "
							+ taskNumberInteger.toString());
			// Attempt to open a connection
			openConnectionInternal();
			logger.log(Level.FINE,
					() -> "Creation succcessful for DB2CONN for task number "
							+ taskNumberInteger.toString());
		}
		else
		{
			logger.log(Level.FINE, () -> "Reusing DB2CONN for task number "
					+ taskNumberInteger.toString());
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="97">

---

If the connection is closed, it tries to reopen it.

```java
			try
			{
				// If the connection is closed, try to reopen the connection
				if (this.conn.isClosed())
				{
					logger.warning(
							"DB2 connection was closed, attempting to reopen");
					openConnectionInternal();
				}
			}
			catch (SQLException e)
			{
				logger.severe(e.getLocalizedMessage());
			}
		}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="162">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="163:3:3" line-data="	void openConnectionInternal()">`openConnectionInternal`</SwmToken> method handles the actual process of obtaining a connection from the data source.

```java
	@SuppressWarnings("unchecked")
	void openConnectionInternal()
	{
		logger.entering(this.getClass().getName(), "openConnectionInternal");
		String jndiString = "jdbc/defaultCICSDataSource";
		Context ctx;
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="169">

---

It looks up the data source and attempts to get a connection.

```java
		try
		{
			ctx = new InitialContext();
			DataSource ds = (DataSource) ctx.lookup(jndiString);
			logger.log(Level.FINE, () -> "jndi string is " + jndiString);
			// If there is no current connection
			if (this.conn == null)
			{
				logger.log(Level.FINE,
						() -> "About to attempt to get DB2 connection");
				// Try and get a connection
				this.conn = ds.getConnection();
				this.conn.setTransactionIsolation(
						Connection.TRANSACTION_READ_UNCOMMITTED);
				Integer taskNumberInterger = Task.getTask().getTaskNumber();
				String db2ConnString = DB2CONN
						.concat(taskNumberInterger.toString());
				cornedBeef.put(db2ConnString, this.conn);
				HBankDataAccess.incrementConnCount();

			}
			else
			{
				// If the connection is closed, open a new connection
				if (this.conn.isClosed())
				{
					logger.log(Level.FINE,
							() -> "DB2 connection was closed, getting a new one");
					this.conn = ds.getConnection();
				}
			}
		}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="201">

---

If an error occurs during the lookup or connection process, it logs the error and aborts the task.

```java
		catch (NamingException | SQLException e)
		{
			logger.severe(e.getMessage());
			Task.getTask().abend("HDB2");
		}
	}
```

---

</SwmSnippet>

# Connection lifecycle methods

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="117">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="117:5:5" line-data="	public void terminate()">`terminate`</SwmToken> method is used to close the connection when it is no longer needed.

```java
	public void terminate()
	{
		// Close the connection
		closeConnection();
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="124">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="124:5:5" line-data="	public void closeConnection()">`closeConnection`</SwmToken> method handles the process of closing the connection and removing it from the <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="11:6:6" line-data="import java.util.HashMap;">`HashMap`</SwmToken>.

```java
	public void closeConnection()
	{
		// Close the connection to the DB2 database
		logger.entering(this.getClass().getName(), "closeConnection()");
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="129">

---

It decrements the connection count and commits the task before closing the connection.

```java
		HBankDataAccess.decrementConnCount();

		Integer taskNumberInterger = Task.getTask().getTaskNumber();
		String db2ConnString = DB2CONN.concat(taskNumberInterger.toString());
		this.conn = (Connection) cornedBeef.get(db2ConnString);
		// If there is an open connection
		if (this.conn != null)
		{
			logger.log(Level.FINE, () -> "We have a DB2 connection to close");
			try
			{
				logger.log(Level.FINE, () -> "Syncpointing");
				Task.getTask().commit();
				// Close the connection
				this.conn.close();
				cornedBeef.remove(db2ConnString);
			}
			catch (SQLException e)
			{
				logger.severe(
						"SQLException in com.ibm.cics.cip.bankliberty.web.db2.Customer "
								+ e.getErrorCode() + "," + e.getSQLState() + ","
								+ e.getMessage());
			}
			catch (InvalidRequestException | RolledBackException e)
			{
				logger.severe(e.getLocalizedMessage());
			}
		}
		logger.exiting(this.getClass().getName(), "closeConnection()");
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="222">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="222:7:7" line-data="	private static void incrementConnCount()">`incrementConnCount`</SwmToken> and <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="129:3:3" line-data="		HBankDataAccess.decrementConnCount();">`decrementConnCount`</SwmToken> methods manage the connection count.

```java
	private static void incrementConnCount()
	{
		HBankDataAccess.connectionCount++;
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" line="228">

---

&nbsp;

```java
	private static void decrementConnCount()
	{
		HBankDataAccess.connectionCount--;
	}
}
```

---

</SwmSnippet>

This structure ensures efficient and reliable management of <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/HBankDataAccess.java" pos="71:13:13" line-data="		// Open a connection to the DB2 database">`DB2`</SwmToken> connections, reducing the overhead associated with opening and closing connections for each task.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
