---
title: Building the Z-OS Connect Payment Interface with Maven
---
# Intro

This document explains how Maven is used in the <SwmPath>[src/Z-OS-Connect-Payment-Interface/](src/Z-OS-Connect-Payment-Interface/)</SwmPath> directory. It will cover the configuration steps in the <SwmPath>[src/Z-OS-Connect-Payment-Interface/pom.xml](src/Z-OS-Connect-Payment-Interface/pom.xml)</SwmPath> file and other related Maven files.

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/pom.xml" line="1">

---

# Project Definition

The <SwmPath>[src/Z-OS-Connect-Payment-Interface/pom.xml](src/Z-OS-Connect-Payment-Interface/pom.xml)</SwmPath> file begins with the project definition, specifying the model version and parent project. The parent project is <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="8:4:10" line-data="		&lt;artifactId&gt;spring-boot-starter-parent&lt;/artifactId&gt;">`spring-boot-starter-parent`</SwmToken> with version <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="9:4:8" line-data="		&lt;version&gt;3.2.5&lt;/version&gt;">`3.2.5`</SwmToken>.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright IBM Corp. 2023 -->
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd">
	<modelVersion>4.0.0</modelVersion>
	<parent>
		<groupId>org.springframework.boot</groupId>
		<artifactId>spring-boot-starter-parent</artifactId>
		<version>3.2.5</version>
		<relativePath /> <!-- lookup parent from repository -->
	</parent>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/pom.xml" line="14">

---

# Project Metadata

The project metadata includes the group ID, artifact ID, version, packaging type, name, and description. This metadata uniquely identifies the project and provides information about its purpose.

```xml
	<groupId>com.ibm.cics.cip.bank.springboot</groupId>
	<artifactId>paymentinterface</artifactId>
	<version>1.1</version>
	<packaging>war</packaging>
	<name>paymentinterface</name>
	<description>Springboot project utilising Z/OS Connect</description>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/pom.xml" line="20">

---

# Properties

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="20:2:2" line-data="	&lt;properties&gt;">`properties`</SwmToken> section specifies the Java version to be used, which is set to <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="21:6:6" line-data="		&lt;java.version&gt;17&lt;/java.version&gt;">`17`</SwmToken>.

```xml
	<properties>
		<java.version>17</java.version>
	</properties>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/pom.xml" line="23">

---

# Dependencies

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="23:2:2" line-data="	&lt;dependencies&gt;">`dependencies`</SwmToken> section lists all the dependencies required by the project. These include various libraries such as <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="27:4:4" line-data="			&lt;artifactId&gt;jcommander&lt;/artifactId&gt;">`jcommander`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="34:4:6" line-data="			&lt;artifactId&gt;jackson-core&lt;/artifactId&gt;">`jackson-core`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="40:4:6" line-data="			&lt;artifactId&gt;jackson-databind&lt;/artifactId&gt;">`jackson-databind`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="46:4:8" line-data="			&lt;artifactId&gt;reactor-netty-http&lt;/artifactId&gt;">`reactor-netty-http`</SwmToken>, and several Spring Boot starters.

```xml
	<dependencies>

		<dependency>
			<groupId>com.beust</groupId>
			<artifactId>jcommander</artifactId>
			<version>1.82</version>
			<scope>compile</scope>
		</dependency>

		<dependency>
			<groupId>com.fasterxml.jackson.core</groupId>
			<artifactId>jackson-core</artifactId>
			<scope>compile</scope>
		</dependency>

		<dependency>
			<groupId>com.fasterxml.jackson.core</groupId>
			<artifactId>jackson-databind</artifactId>
		</dependency>


```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/pom.xml" line="178">

---

# Build Plugins

The <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="178:2:2" line-data="	&lt;build&gt;">`build`</SwmToken> section defines the plugins used during the build process. The <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="182:4:8" line-data="				&lt;artifactId&gt;maven-war-plugin&lt;/artifactId&gt;">`maven-war-plugin`</SwmToken> is used to package the project as a WAR file, and the <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="194:4:10" line-data="				&lt;artifactId&gt;spring-boot-maven-plugin&lt;/artifactId&gt;">`spring-boot-maven-plugin`</SwmToken> is used to repackage the project as an executable JAR.

```xml
	<build>
		<plugins>
			<plugin>
				<groupId>org.apache.maven.plugins</groupId>
				<artifactId>maven-war-plugin</artifactId>
				<executions>
					<execution>
						<goals>
							<goal>war</goal>
						</goals>
						<phase>package</phase>
					</execution>
				</executions>
			</plugin>
			<plugin>
				<groupId>org.springframework.boot</groupId>
				<artifactId>spring-boot-maven-plugin</artifactId>
				<executions>
					<execution>
						<id>repackage</id>
						<goals>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/mvnw" line="1">

---

# Maven Wrapper

The `mvnw` and <SwmPath>[src/Z-OS-Connect-Payment-Interface/mvnw.cmd](src/Z-OS-Connect-Payment-Interface/mvnw.cmd)</SwmPath> files are used to invoke Maven without requiring it to be installed on the system. These scripts ensure that the correct version of Maven is used for the build.

```
#!/bin/sh
#
# ----------------------------------------------------------------------------
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
# ----------------------------------------------------------------------------

```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/mvnw.cmd" line="1">

---

# Maven Wrapper Batch Script

The <SwmPath>[src/Z-OS-Connect-Payment-Interface/mvnw.cmd](src/Z-OS-Connect-Payment-Interface/mvnw.cmd)</SwmPath> file is the Windows batch script for the Maven wrapper. It performs similar functions to the `mvnw` script but is tailored for Windows environments.

```batchfile
@REM ----------------------------------------------------------------------------
@REM Licensed to the Apache Software Foundation (ASF) under one
@REM or more contributor license agreements.  See the NOTICE file
@REM distributed with this work for additional information
@REM regarding copyright ownership.  The ASF licenses this file
@REM to you under the Apache License, Version 2.0 (the
@REM "License"); you may not use this file except in compliance
@REM with the License.  You may obtain a copy of the License at
@REM
@REM    https://www.apache.org/licenses/LICENSE-2.0
@REM
@REM Unless required by applicable law or agreed to in writing,
@REM software distributed under the License is distributed on an
@REM "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
@REM KIND, either express or implied.  See the License for the
@REM specific language governing permissions and limitations
@REM under the License.
@REM ----------------------------------------------------------------------------

@REM ----------------------------------------------------------------------------
@REM Maven Start Up Batch script
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
