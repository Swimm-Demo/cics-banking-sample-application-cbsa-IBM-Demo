---
title: Building the Payment Interface with Maven
---
# Intro

This document explains how Maven is used in the <SwmPath>[src/Z-OS-Connect-Payment-Interface/](src/Z-OS-Connect-Payment-Interface/)</SwmPath> directory. It will go through the configuration steps in the <SwmPath>[src/Z-OS-Connect-Payment-Interface/pom.xml](src/Z-OS-Connect-Payment-Interface/pom.xml)</SwmPath> file.

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/pom.xml" line="1">

---

# Project Metadata

The <SwmPath>[src/Z-OS-Connect-Payment-Interface/pom.xml](src/Z-OS-Connect-Payment-Interface/pom.xml)</SwmPath> file begins with the project metadata, including the model version, parent project, group ID, artifact ID, version, packaging type, name, description, and Java version. This sets up the basic information for the Maven project.

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


	<groupId>com.ibm.cics.cip.bank.springboot</groupId>
	<artifactId>paymentinterface</artifactId>
	<version>1.1</version>
	<packaging>war</packaging>
	<name>paymentinterface</name>
	<description>Springboot project utilising Z/OS Connect</description>
	<properties>
		<java.version>17</java.version>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Payment-Interface/pom.xml" line="23">

---

# Dependencies

The dependencies section lists all the libraries required for the project. Each dependency includes the group ID, artifact ID, version, and scope. This ensures that all necessary libraries are included during the build process.

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

The build section specifies the plugins used during the build process. The <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="182:4:8" line-data="				&lt;artifactId&gt;maven-war-plugin&lt;/artifactId&gt;">`maven-war-plugin`</SwmToken> is used to package the project as a WAR file, and the <SwmToken path="src/Z-OS-Connect-Payment-Interface/pom.xml" pos="194:4:10" line-data="				&lt;artifactId&gt;spring-boot-maven-plugin&lt;/artifactId&gt;">`spring-boot-maven-plugin`</SwmToken> is used to repackage the project as an executable JAR or WAR file.

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

# Maven Wrapper Script

The `mvnw` script is a shell script that allows the project to be built with a specific version of Maven, ensuring consistency across different environments. It checks for the presence of Maven and Java, sets up environment variables, and executes Maven commands.

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

The <SwmPath>[src/Z-OS-Connect-Payment-Interface/mvnw.cmd](src/Z-OS-Connect-Payment-Interface/mvnw.cmd)</SwmPath> script is a batch script for Windows that performs the same functions as the `mvnw` shell script. It ensures that the project can be built consistently on Windows environments by setting up the necessary environment variables and executing Maven commands.

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
