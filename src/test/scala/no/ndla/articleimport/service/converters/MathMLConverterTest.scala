/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.integration.LanguageIngress
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}

import scala.util.Success

class MathMLConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val defaultImportStatus = ImportStatus.empty

  val defaultLanguageIngress = LanguageIngress("Jeg er en ingress", None)

  val defaultLanguageIngressWithHtml =
    LanguageIngress("<p>Jeg er en ingress</p>", None)

  test("an xmlns attribute should be added to MathML math tags") {
    val originalContent = """<math><mi>P</mi></math><math></math>"""
    val expectedContent =
      """<math xmlns="http://www.w3.org/1998/Math/MathML"><mi>P</mi></math><math xmlns="http://www.w3.org/1998/Math/MathML"></math>"""
    val content = TestData.sampleContent.copy(content = originalContent)
    val Success((result, _)) =
      MathMLConverter.convert(content, defaultImportStatus)

    result.content should equal(expectedContent)
  }

  test("that p-tag parents of math with center aligment gets converted to data-align") {
    val originalContent =
      """<p style="text-align:center"><span><math xmlns="http://www.w3.org/1998/Math/MathML"></math></span></p>"""
    val expectedContent =
      """<p style="text-align:center" data-align="center"><span><math xmlns="http://www.w3.org/1998/Math/MathML"></math></span></p>"""
    val content = TestData.sampleContent.copy(content = originalContent)
    val Success((result, _)) =
      MathMLConverter.convert(content, defaultImportStatus)

    result.content should equal(expectedContent)
  }

}
