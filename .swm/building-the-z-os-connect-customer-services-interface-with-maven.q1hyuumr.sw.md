---
title: Building the Z-OS Connect Customer Services Interface with Maven
---
# Intro

This document explains how Maven is used in the <SwmPath>[src/Z-OS-Connect-Customer-Services-Interface/](src/Z-OS-Connect-Customer-Services-Interface/)</SwmPath> directory. It will cover the configuration and usage of Maven in this specific context.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="1">

---

# Project Definition

The <SwmPath>[src/Z-OS-Connect-Customer-Services-Interface/pom.xml](src/Z-OS-Connect-Customer-Services-Interface/pom.xml)</SwmPath> file starts with the XML declaration and the project definition, specifying the model version and the XML namespace for Maven POM.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright IBM Corp. 2023 -->
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd">
	<modelVersion>4.0.0</modelVersion>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="6">

---

# Parent Project

The parent project is defined as <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="8:4:10" line-data="		&lt;artifactId&gt;spring-boot-starter-parent&lt;/artifactId&gt;">`spring-boot-starter-parent`</SwmToken> with version <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="9:4:8" line-data="		&lt;version&gt;3.2.5&lt;/version&gt;">`3.2.5`</SwmToken>. This allows the project to inherit dependencies and plugins from the Spring Boot parent POM.

```xml
	<parent>
		<groupId>org.springframework.boot</groupId>
		<artifactId>spring-boot-starter-parent</artifactId>
		<version>3.2.5</version>
		<relativePath /> <!-- lookup parent from repository -->
	</parent>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="14">

---

# Project Coordinates

The project coordinates are defined with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="14:2:2" line-data="	&lt;groupId&gt;com.ibm.cics.cip.bank.springboot&lt;/groupId&gt;">`groupId`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="15:2:2" line-data="	&lt;artifactId&gt;customerservices&lt;/artifactId&gt;">`artifactId`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="16:2:2" line-data="	&lt;version&gt;1.0&lt;/version&gt;">`version`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="17:2:2" line-data="	&lt;packaging&gt;war&lt;/packaging&gt;">`packaging`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="18:2:2" line-data="	&lt;name&gt;customerservices&lt;/name&gt;">`name`</SwmToken>, and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="19:2:2" line-data="	&lt;description&gt;Springboot project utilising Z/OS Connect&lt;/description&gt;">`description`</SwmToken>. This uniquely identifies the project and provides metadata.

```xml
	<groupId>com.ibm.cics.cip.bank.springboot</groupId>
	<artifactId>customerservices</artifactId>
	<version>1.0</version>
	<packaging>war</packaging>
	<name>customerservices</name>
	<description>Springboot project utilising Z/OS Connect</description>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="20">

---

# Properties

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="20:2:2" line-data="	&lt;properties&gt;">`properties`</SwmToken> section defines the Java version to be used, which is set to <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="21:6:6" line-data="		&lt;java.version&gt;17&lt;/java.version&gt;">`17`</SwmToken>.

```xml
	<properties>
		<java.version>17</java.version>
	</properties>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="23">

---

# Dependencies

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="23:2:2" line-data="	&lt;dependencies&gt;">`dependencies`</SwmToken> section lists all the dependencies required by the project. This includes various libraries such as <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="27:4:4" line-data="			&lt;artifactId&gt;jcommander&lt;/artifactId&gt;">`jcommander`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="34:4:6" line-data="			&lt;artifactId&gt;jackson-core&lt;/artifactId&gt;">`jackson-core`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="45:4:8" line-data="			&lt;artifactId&gt;reactor-netty-http&lt;/artifactId&gt;">`reactor-netty-http`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="84:4:10" line-data="			&lt;artifactId&gt;spring-boot-starter-validation&lt;/artifactId&gt;">`spring-boot-starter-validation`</SwmToken>, and more. Each dependency is defined with its <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="26:2:2" line-data="			&lt;groupId&gt;com.beust&lt;/groupId&gt;">`groupId`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="27:2:2" line-data="			&lt;artifactId&gt;jcommander&lt;/artifactId&gt;">`artifactId`</SwmToken>, <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="28:2:2" line-data="			&lt;version&gt;1.82&lt;/version&gt;">`version`</SwmToken>, and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="29:2:2" line-data="			&lt;scope&gt;compile&lt;/scope&gt;">`scope`</SwmToken>.

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

		<dependency>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="150">

---

# Build Plugins

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="150:2:2" line-data="	&lt;build&gt;">`build`</SwmToken> section defines the plugins used during the build process. This includes the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="154:4:10" line-data="				&lt;artifactId&gt;spring-boot-maven-plugin&lt;/artifactId&gt;">`spring-boot-maven-plugin`</SwmToken> for building Spring Boot applications and the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="158:4:10" line-data="				&lt;artifactId&gt;cics-bundle-maven-plugin&lt;/artifactId&gt;">`cics-bundle-maven-plugin`</SwmToken> for creating CICS bundles. The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="158:4:10" line-data="				&lt;artifactId&gt;cics-bundle-maven-plugin&lt;/artifactId&gt;">`cics-bundle-maven-plugin`</SwmToken> is configured to execute the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="163:4:6" line-data="							&lt;goal&gt;bundle-war&lt;/goal&gt;">`bundle-war`</SwmToken> goal with a specific <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/pom.xml" pos="166:2:2" line-data="							&lt;jvmserver&gt;CBSAWLP&lt;/jvmserver&gt;">`jvmserver`</SwmToken>.

```xml
	<build>
		<plugins>
			<plugin>
				<groupId>org.springframework.boot</groupId>
				<artifactId>spring-boot-maven-plugin</artifactId>
			</plugin>
			<plugin>
				<groupId>com.ibm.cics</groupId>
				<artifactId>cics-bundle-maven-plugin</artifactId>
				<version>1.0.2</version>
				<executions>
					<execution>
						<goals>
							<goal>bundle-war</goal>
						</goals>
						<configuration>
							<jvmserver>CBSAWLP</jvmserver>
						</configuration>
					</execution>
				</executions>
			</plugin>
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/mvnw" line="1">

---

# Maven Wrapper Script

The `mvnw` script is a shell script for running Maven with the Maven Wrapper. It ensures that the correct version of Maven is used and handles various environment configurations. It checks for the presence of <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/mvnw" pos="27:2:2" line-data="#   JAVA_HOME - location of a JDK home dir">`JAVA_HOME`</SwmToken> and <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/mvnw" pos="31:2:2" line-data="#   M2_HOME - location of maven2&#39;s installed home dir">`M2_HOME`</SwmToken>, sets up the classpath, and executes the Maven Wrapper main class.

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

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd" line="1">

---

# Maven Wrapper Batch Script

The <SwmPath>[src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd](src/Z-OS-Connect-Customer-Services-Interface/mvnw.cmd)</SwmPath> script is a batch script for running Maven on Windows with the Maven Wrapper. It performs similar functions to the `mvnw` script, including checking for <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/mvnw" pos="27:2:2" line-data="#   JAVA_HOME - location of a JDK home dir">`JAVA_HOME`</SwmToken>, setting up the project base directory, and executing the Maven Wrapper main class.

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
