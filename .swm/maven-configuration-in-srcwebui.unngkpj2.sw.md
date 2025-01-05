---
title: Maven Configuration in src/webui
---
# Intro

This document explains how Maven is used in the <SwmPath>[src/webui/](src/webui/)</SwmPath> directory. It will cover the configuration steps in the <SwmPath>[src/webui/pom.xml](src/webui/pom.xml)</SwmPath> file.

<SwmSnippet path="/src/webui/pom.xml" line="3">

---

## Project Metadata

The <SwmPath>[src/webui/pom.xml](src/webui/pom.xml)</SwmPath> file begins with the project metadata, including the group ID, artifact ID, version, name, and URL of the project.

```xml
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
	<modelVersion>4.0.0</modelVersion>

	<groupId>com.ibm.cics.cip.bank.libertyapi.webui</groupId>
	<artifactId>webui</artifactId>
	<version>1.0</version>

	<name>webui</name>
	<url>https://github.com/cicsdev/cics-banking-sample-application-cbsa</url>
	<packaging>war</packaging>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="15">

---

## Project Properties

Next, the project properties are defined, specifying the source encoding and the Java compiler source and target versions.

```xml
	<properties>
		<project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
		<maven.compiler.source>8</maven.compiler.source>
		<maven.compiler.target>8</maven.compiler.target>
	</properties>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="22">

---

## Dependency Management

The <SwmToken path="src/webui/pom.xml" pos="22:2:2" line-data="	&lt;dependencyManagement&gt;">`dependencyManagement`</SwmToken> section is used to import the Bill of Materials (BOM) for IBM CICS dependencies, ensuring consistent versions across the project.

```xml
	<dependencyManagement>
		<dependencies>
			<dependency>
				<groupId>com.ibm.cics</groupId>
				<artifactId>com.ibm.cics.ts.bom</artifactId>
				<version>5.6-20200609123739</version>
				<type>pom</type>
				<scope>import</scope>
			</dependency>
		</dependencies>
	</dependencyManagement>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="34">

---

## Project Dependencies

The <SwmToken path="src/webui/pom.xml" pos="34:2:2" line-data="	&lt;dependencies&gt;">`dependencies`</SwmToken> section lists all the dependencies required by the project, including Jakarta EE, JAX-RS, validation API, servlet API, and others.

```xml
	<dependencies>
        <dependency>
            <groupId>jakarta.platform</groupId>
            <artifactId>jakarta.jakartaee-api</artifactId>
            <version>10.0.0</version>
            <scope>provided</scope>
        </dependency>
		<dependency>
			<groupId>javax.ws.rs</groupId>
			<artifactId>javax.ws.rs-api</artifactId>
			<version>2.1.1</version>
			<scope>provided</scope>
		</dependency>
		<dependency>
			<groupId>javax.validation</groupId>
			<artifactId>validation-api</artifactId>
			<version>2.0.1.Final</version>
			<scope>provided</scope>
		</dependency>

		<dependency>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="99">

---

## Build Configuration

The <SwmToken path="src/webui/pom.xml" pos="99:2:2" line-data="	&lt;build&gt;">`build`</SwmToken> section specifies the source and output directories for the project.

```xml
	<build>
		<sourceDirectory>${basedir}/src/main/java</sourceDirectory>
		<outputDirectory>${basedir}/target/classes</outputDirectory>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="102">

---

## Plugin Management

The <SwmToken path="src/webui/pom.xml" pos="102:2:2" line-data="		&lt;pluginManagement&gt;">`pluginManagement`</SwmToken> section locks down plugin versions to avoid using Maven defaults. It includes plugins for cleaning, compiling, testing, packaging, installing, deploying, and generating site reports.

```xml
		<pluginManagement>
			<!-- lock down plugins versions to avoid using Maven defaults (may be 
				moved to parent pom) -->
			<plugins>
				<!-- clean lifecycle, see https://maven.apache.org/ref/current/maven-core/lifecycles.html#clean_Lifecycle -->
				<plugin>
					<artifactId>maven-clean-plugin</artifactId>
					<version>3.1.0</version>
				</plugin>
				<!-- default lifecycle, jar packaging: see https://maven.apache.org/ref/current/maven-core/default-bindings.html#Plugin_bindings_for_jar_packaging -->
				<plugin>
					<artifactId>maven-resources-plugin</artifactId>
					<version>3.0.2</version>
				</plugin>
				<plugin>
					<artifactId>maven-compiler-plugin</artifactId>
					<version>3.8.0</version>
					<configuration>
						<compilerArgs>
							<arg>-Xlint:deprecation</arg>
							<arg>-Xlint:unchecked</arg>
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/pom.xml" line="151">

---

## WAR Plugin Configuration

The <SwmToken path="src/webui/pom.xml" pos="153:4:8" line-data="					&lt;artifactId&gt;maven-war-plugin&lt;/artifactId&gt;">`maven-war-plugin`</SwmToken> is configured to specify the location of the <SwmPath>[src/…/WEB-INF/web.xml](src/webui/WebContent/WEB-INF/web.xml)</SwmPath> file and the web resources directory.

```xml
				<plugin>
					<groupId>org.apache.maven.plugins</groupId>
					<artifactId>maven-war-plugin</artifactId>
					<version>3.4.0</version>
					<configuration>
						<webXml>WebContent\WEB-INF\web.xml</webXml>
						<webResources>
							<resource>
								<directory>WebContent</directory>
							</resource>
						</webResources>
					</configuration>
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"><sup>Powered by [Swimm](/)</sup></SwmMeta>
