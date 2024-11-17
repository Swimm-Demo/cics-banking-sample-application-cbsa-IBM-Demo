---
repo_id: >-
  Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==
doc_id: 03z8ldh7
---
# Building the WebUI with Maven

## Intro

This document explains how Maven is used in the `src/webui/` directory of the CICS Bank Sample Application (CBSA). It will cover the configuration steps in the `src/webui/pom.xml` file.

<SwmSnippet path="/src/webui/pom.xml" line="1">

---

## Project Metadata

The `src/webui/pom.xml` file begins with the project metadata, including the group ID, artifact ID, version, name, URL, and packaging type. This information uniquely identifies the project and its version.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright IBM Corp. 2023 -->
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

The properties section defines the project's build source encoding and the Java compiler source and target versions. This ensures that the project is built using the specified Java version and encoding.

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

The dependency management section imports a BOM (Bill of Materials) for CICS dependencies. This ensures that all dependencies are aligned with the specified version of the CICS BOM.

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

## Dependencies

The dependencies section lists all the libraries required by the project. These include Jakarta EE API, JAX-RS API, Validation API, Servlet API, CICS server, WebSphere JSON API, JZOS, YUI Compressor, Rhino, and SLF4J. Some dependencies are marked as 'provided' because they are expected to be available in the runtime environment.

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

The build configuration section specifies the source and output directories for the project's compiled classes. This ensures that the source files are compiled to the correct location.

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

The plugin management section locks down the versions of various Maven plugins used in the build process. This includes plugins for cleaning, resource processing, compilation, testing, packaging, installation, deployment, site generation, and project information reporting. The <SwmToken path="src/webui/pom.xml" pos="153:4:8" line-data="					&lt;artifactId&gt;maven-war-plugin&lt;/artifactId&gt;">`maven-war-plugin`</SwmToken> is configured to package the project as a WAR file, specifying the location of the `src/webui/WebContent/WEB-INF/web.xml` file and additional web resources.

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

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa-IBM-Demo"></SwmMeta>
