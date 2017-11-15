# article-import 
[![Build Status](https://travis-ci.org/NDLANO/article-import.svg?branch=master)](https://travis-ci.org/NDLANO/article-import)

## Usage
Creates, updates and returns an `Article`. Implements Elasticsearch for search within the article database.
Imports, converts, submits and publishes an article from ndla.no to draft-api/article-api.

To interact with the api, you need valid security credentials; see [Access Tokens usage](https://github.com/NDLANO/auth/blob/master/README.md).
To write data to the api, you need write role access.

It also has as internal import routines for importing data from the old system to this database. There are a number of cleaning and
reporting services pertaining to the import which are only available for internal admin services. 

## Developer documentation

**Compile**: sbt compile

**Run tests:** sbt test

**Create Docker Image:** sbt docker

### IntegrationTest Tag and sbt run problems
Tests that need a running elasticsearch outside of component, e.g. in your local docker are marked with selfdefined java
annotation test tag  ```IntegrationTag``` in ```/ndla/article-api/src/test/java/no/ndla/tag/IntegrationTest.java```. 
As of now we have no running elasticserach or tunnel to one on Travis and need to ignore these tests there or the build will fail.  
Therefore we have the
 ```testOptions in Test += Tests.Argument("-l", "no.ndla.tag.IntegrationTest")``` in ```build.sbt```  

    sbt "test-only -- -n no.ndla.tag.IntegrationTest"

