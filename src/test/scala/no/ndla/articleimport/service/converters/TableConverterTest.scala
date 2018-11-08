/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.{TestData, UnitSuite}
import no.ndla.articleimport.model.domain.ImportStatus

import scala.util.Success

class TableConverterTest extends UnitSuite {
  val nodeId = "1234"

  test("Dont ruin if theads already exist") {
    val tableWithoutThead =
      s"""<table>
         |<thead>
         |<tr>
         |<th>Header content 1</th>
         |<th>Header content 2</th>
         |</tr>
         |</thead>
         |<tbody>
         |<tr>
         |<td>Body content 1</td>
         |<td>Body content 2</td>
         |</tr>
         |</tbody>
         |</table>""".stripMargin.replace("\n", "")
    val expectedTable =
      s"""<table>
         |<thead>
         |<tr>
         |<th>Header content 1</th>
         |<th>Header content 2</th>
         |</tr>
         |</thead>
         |<tbody>
         |<tr>
         |<td>Body content 1</td>
         |<td>Body content 2</td>
         |</tr>
         |</tbody>
         |</table>""".stripMargin.replace("\n", "")

    val initialContent =
      TestData.sampleContent.copy(content = tableWithoutThead)
    val Success((result, _)) =
      TableConverter.convert(initialContent, ImportStatus.empty)
    result.content should equal(expectedTable)
  }

  test("Create theads even if two bodies") {
    val tableWithoutThead =
      s"""<table>
         |<tbody>
         |<tr>
         |<th>Header content 1</th>
         |<th>Header content 2</th>
         |</tr>
         |</tbody>
         |<tbody>
         |<tr>
         |<td>Body content 1</td>
         |<td>Body content 2</td>
         |</tr>
         |</tbody>
         |</table>""".stripMargin.replace("\n", "")
    val expectedTable =
      s"""<table>
         |<thead>
         |<tr>
         |<th>Header content 1</th>
         |<th>Header content 2</th>
         |</tr>
         |</thead>
         |<tbody>
         |<tr>
         |<td>Body content 1</td>
         |<td>Body content 2</td>
         |</tr>
         |</tbody>
         |</table>""".stripMargin.replace("\n", "")

    val initialContent =
      TestData.sampleContent.copy(content = tableWithoutThead)
    val Success((result, _)) =
      TableConverter.convert(initialContent, ImportStatus.empty)
    result.content should equal(expectedTable)
  }

  test("Header rows are wrapped in thead block") {
    val tableWithoutThead =
      s"""<table>
          |<tr>
          |<th>Header content 1</th>
          |<th>Header content 2</th>
          |</tr>
          |<tr>
          |<td>Body content 1</td>
          |<td>Body content 2</td>
          |</tr>
          |</table>""".stripMargin.replace("\n", "")
    val expectedTable =
      s"""<table>
         |<thead>
         |<tr>
         |<th>Header content 1</th>
         |<th>Header content 2</th>
         |</tr>
         |</thead>
         |<tbody>
         |<tr>
         |<td>Body content 1</td>
         |<td>Body content 2</td>
         |</tr>
         |</tbody>
         |</table>""".stripMargin.replace("\n", "")

    val initialContent =
      TestData.sampleContent.copy(content = tableWithoutThead)
    val Success((result, _)) =
      TableConverter.convert(initialContent, ImportStatus.empty)
    result.content should equal(expectedTable)
  }

  test("Header rows are wrapped in thead block when already inside tbody") {
    val tableWithoutTheadWithTbody =
      s"""<table>
         |<tbody>
         |<tr>
         |<th>Header content 1</th>
         |<th>Header content 2</th>
         |</tr>
         |<tr>
         |<td>Body content 1</td>
         |<td>Body content 2</td>
         |</tr>
         |</tbody>
         |</table>""".stripMargin.replace("\n", "")
    val expectedTable =
      s"""<table>
         |<thead>
         |<tr>
         |<th>Header content 1</th>
         |<th>Header content 2</th>
         |</tr>
         |</thead>
         |<tbody>
         |<tr>
         |<td>Body content 1</td>
         |<td>Body content 2</td>
         |</tr>
         |</tbody>
         |</table>""".stripMargin.replace("\n", "")

    val initialContent =
      TestData.sampleContent.copy(content = tableWithoutTheadWithTbody)
    val Success((result, _)) =
      TableConverter.convert(initialContent, ImportStatus.empty)
    result.content should equal(expectedTable)
  }

  test("paragraphs are unwrapped if cell contains only one") {
    val table2x3 =
      s"""<table>
          |<tbody>
          |<tr>
          |<td><p>column</p></td>
          |<td><p>column</p><p>hey</p></td>
          |</tr>
          |</tbody>
          |</table>""".stripMargin.replace("\n", "")

    val table2x3ExpectedResult =
      s"""<table>
          |<tbody>
          |<tr>
          |<td>column</td>
          |<td><p>column</p><p>hey</p></td>
          |</tr>
          |</tbody>
          |</table>""".stripMargin.replace("\n", "")

    val initialContent = TestData.sampleContent.copy(content = table2x3)
    val Success((result, _)) =
      TableConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(table2x3ExpectedResult)
  }

  test("The first table row (tr) is converted to table header th") {
    val table2x3 =
      s"""<table>
          |<tbody>
          |<tr>
          |<td><strong>col 1</strong></td>
          |<td><strong>col 2</strong></td>
          |</tr>
          |<tr>
          |<td>row 2</td>
          |<td>row 2</td>
          |</tr>
          |</tbody>
          |</table>""".stripMargin.replace("\n", "")

    val table2x3ExpectedResult =
      s"""<table>
          |<thead>
          |<tr>
          |<th>col 1</th>
          |<th>col 2</th>
          |</tr>
          |</thead>
          |<tbody>
          |<tr>
          |<td>row 2</td>
          |<td>row 2</td>
          |</tr>
          |</tbody>
          |</table>""".stripMargin.replace("\n", "")

    val initialContent = TestData.sampleContent.copy(content = table2x3)
    val Success((result, _)) =
      TableConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(table2x3ExpectedResult)
  }

  test("Strong tags in table header are unwrapped") {
    val table2x3 =
      s"""<table>
          |<tbody>
          |<tr>
          |<th><strong>heading 1</strong></th>
          |<th><strong>heading 2</strong></th>
          |</tr>
          |<tr>
          |<td>col 1</td>
          |<td>col 2</td>
          |</tr>
          |</tbody>
          |</table>""".stripMargin.replace("\n", "")

    val table2x3ExpectedResult =
      s"""<table>
          |<thead>
          |<tr>
          |<th>heading 1</th>
          |<th>heading 2</th>
          |</tr>
          |</thead>
          |<tbody>
          |<tr>
          |<td>col 1</td>
          |<td>col 2</td>
          |</tr>
          |</tbody>
          |</table>""".stripMargin.replace("\n", "")

    val initialContent = TestData.sampleContent.copy(content = table2x3)
    val Success((result, _)) =
      TableConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(table2x3ExpectedResult)
  }

  test("table headers in leftmost column should be supported") {
    val table2x3 =
      s"""
         |<table>
         |<tbody>
         |<tr>
         |<th><strong>Antall øyne</strong></th>
         |<td>1</td>
         |<td>2</td>
         |</tr>
         |<tr>
         |<th>Sannsynlighet</th>
         |<td>1/2</td>
         |<td>1/2</td>
         |</tr>
         |</tbody>
         |</table>
       """.stripMargin.replace("\n", "")

    val expectedResult =
      s"""
         |<table>
         |<tbody>
         |<tr>
         |<th>Antall øyne</th>
         |<td>1</td>
         |<td>2</td>
         |</tr>
         |<tr>
         |<th>Sannsynlighet</th>
         |<td>1/2</td>
         |<td>1/2</td>
         |</tr>
         |</tbody>
         |</table>
       """.stripMargin.replace("\n", "")

    val initialContent = TestData.sampleContent.copy(content = table2x3)
    val Success((result, _)) =
      TableConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)

  }

}
