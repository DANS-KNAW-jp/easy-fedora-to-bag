/**
 * Copyright (C) 2020 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.fedora2vault

import java.io.FileInputStream
import java.lang.{ StringBuilder => JavaStringBuilder }
import java.util.UUID

import better.files.File
import com.yourmediashelf.fedora.client.FedoraClientException
import javax.naming.NamingEnumeration
import javax.naming.directory.{ BasicAttributes, SearchControls, SearchResult }
import javax.naming.ldap.InitialLdapContext
import nl.knaw.dans.easy.fedora2vault.TransformationType.SIMPLE
import nl.knaw.dans.easy.fedora2vault.fixture.{ AudienceSupport, FileSystemSupport, TestSupportFixture }
import org.apache.commons.csv.CSVPrinter
import org.scalamock.scalatest.MockFactory
import resource.managed

import scala.util.{ Failure, Success, Try }
import scala.xml.XML

class AppSpec extends TestSupportFixture with MockFactory with FileSystemSupport with AudienceSupport {
  implicit val logFile: File = testDir / "log.txt"

  override def beforeEach(): Unit = {
    super.beforeEach()
    if (testDir.exists) testDir.delete()
    testDir.createDirectories()
  }

  private class MockedLdapContext extends InitialLdapContext(new java.util.Hashtable[String, String](), null)

  private class MockedApp() extends EasyFedora2vaultApp(null) {
    override lazy val fedoraProvider: FedoraProvider = mock[FedoraProvider]
    override lazy val ldapContext: InitialLdapContext = mock[MockedLdapContext]
  }

  private class OverriddenApp extends MockedApp {
    /** overrides the method called by the method under test */
    override def singleTransform(outputDir: File)(datasetId: DatasetId): Try[CsvRecord] = {
      if (datasetId.startsWith("fatal"))
        Failure(new FedoraClientException(300, "mocked exception"))
      else if (!datasetId.startsWith("success")) {
        outputDir.createFile().writeText(datasetId)
        Failure(new Exception(datasetId))
      } else {
        outputDir.createFile().writeText(datasetId)
        Success(CsvRecord(datasetId, "", "", SIMPLE, UUID.randomUUID(), "OK"))
      }
    }
  }

  "simpleTransforms" should "report success" in {
    val input = (testDir / "input").write(
      """success:1
        |success:2
        |""".stripMargin
    )
    val outputDir = (testDir / "output").createDirectories()
    new OverriddenApp().simpleTransForms(outputDir, input) .toSeq should matchPattern {
      case Seq(
      Success(CsvRecord("success:1", "", "", SIMPLE, _, "OK")),
      Success(CsvRecord("success:2", "", "", SIMPLE, _, "OK")),
      ) =>
    }
    outputDir.list.toSeq should have length 2
  }

  it should "report failure" in {
    val input = (testDir / "input").write(
      """success:1
        |failure:2
        |success:3
        |fatal:4
        |success:5
        |""".stripMargin
    )
    val outputDir = (testDir / "output").createDirectories()
    new OverriddenApp().simpleTransForms(outputDir, input).toSeq should matchPattern {
      case Seq(
      Success(CsvRecord(_, _, _, _, _, "OK")),
      Success(CsvRecord(_, _, _, _, _, "FAILED: java.lang.Exception: failure:2")),
      Success(CsvRecord(_, _, _, _, _, "OK")),
      Failure(_),
      Success(CsvRecord(_, _, _, _, _, "OK")), // TODO not wanted
      ) =>
    }
    outputDir.list.toSeq should have length 3
  }

  "simpleTransform" should "process DepositApi" in {
    val app = new MockedApp()
    implicit val fedoraProvider: FedoraProvider = app.fedoraProvider
    expectedAudiences(Map(
      "easy-discipline:77" -> "D13200",
    ))
    expectedSubordinates(app.fedoraProvider)
    expectedFoXmls(app.fedoraProvider, sampleFoXML / "DepositApi.xml")
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "additional-license").write("lalala"),
      (testDir / "dataset-license").write("blablabla"),
      (testDir / "manifest-sha1.txt").write("rabarbera"),
    )

    app.singleTransform(testDir / "bags" / UUID.randomUUID.toString)("easy-dataset:17") should matchPattern {
      case Success(CsvRecord("easy-dataset:17", "10.17026/test-Iiib-z9p-4ywa", "user001", SIMPLE, _, "OK")) =>
    }
    val metadata = (testDir / "bags").children.next() / "metadata"
    (metadata / "depositor-info/depositor-agreement.pdf").contentAsString shouldBe "blablabla"
    (metadata / "license.pdf").contentAsString shouldBe "lalala"
    metadata.list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml", "license.pdf")
    (metadata / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml", "depositor-agreement.pdf", "message-from-depositor.txt")
  }

  it should "process streaming" in {
    val app = new MockedApp()
    implicit val fedoraProvider: FedoraProvider = app.fedoraProvider
    expectedAudiences(Map(
      "easy-discipline:6" -> "D35400",
    ))
    expectAUser(app.ldapContext)
    expectedFoXmls(app.fedoraProvider, sampleFoXML / "streaming.xml")
    expectedSubordinates(app.fedoraProvider, "easy-file:35")
    expectedFoXmls(app.fedoraProvider, sampleFoXML / "easy-file-35.xml")
    expectedManagedStreams(app.fedoraProvider,
      (testDir / "something.txt").writeText("don't care")
    )

    app.singleTransform(testDir / "bags" / UUID.randomUUID.toString)("easy-dataset:13") should matchPattern {
      case Success(CsvRecord("easy-dataset:13", null, "user001", SIMPLE, _, "OK")) =>
    }
    val metadata = (testDir / "bags").children.next() / "metadata"
    metadata.list.toSeq.map(_.name)
      .sortBy(identity) shouldBe Seq("amd.xml", "dataset.xml", "depositor-info", "emd.xml", "files.xml")
    (metadata / "depositor-info").list.toSeq.map(_.name).sortBy(identity) shouldBe
      Seq("agreements.xml")
    (metadata / "files.xml").contentAsString.split("\n").map(_.trim).mkString("\n") shouldBe
      """<?xml version='1.0' encoding='UTF-8'?>
        |<files
        |xsi:schemaLocation="http://easy.dans.knaw.nl/schemas/bag/metadata/files/ https://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/" xmlns:dcterms="http://purl.org/dc/terms/">
        |<file filepath="data/original/P1130783.JPG">
        |<dcterms:title>P1130783.JPG</dcterms:title>
        |<dcterms:format>image/jpeg</dcterms:format>
        |<dcterms:created>2020-03-17T10:24:17.660Z</dcterms:created>
        |<accessibleToRights>ANONYMOUS</accessibleToRights>
        |<visibleToRights>RESTRICTED_REQUEST</visibleToRights>
        |</file>
        |</files>""".stripMargin
  }

  private def csvPrinter(sb: JavaStringBuilder): CSVPrinter = {
    CsvRecord.csvFormat.print(sb)
  }

  private def expectedSubordinates(fedoraProvider: => FedoraProvider, expectedIds: String*): Unit = {
    (fedoraProvider.getSubordinates(_: String)) expects * once() returning Success(expectedIds)
  }

  private def expectedManagedStreams(fedoraProvider: => FedoraProvider, expectedObjects: File*): Unit = {
    expectedObjects.foreach(file =>
      (fedoraProvider.disseminateDatastream(_: String, _: String)) expects(*, *) once() returning
        managed(new FileInputStream(file.toJava))
    )
  }

  private def expectedFoXmls(fedoraProvider: => FedoraProvider, expectedObjects: File*): Unit = {
    expectedObjects.foreach(file =>
      (fedoraProvider.loadFoXml(_: String)) expects * once() returning
        Try(XML.loadFile(file.toJava))
    )
  }

  private def expectAUser(ldapContext: => InitialLdapContext) = {
    val result = mock[NamingEnumeration[SearchResult]]
    result.hasMoreElements _ expects() returning true
    val attributes = new BasicAttributes {
      put("displayname", "U.Ser")
      put("mail", "does.not.exist@dans.knaw.nl")
    }
    result.nextElement _ expects() returning new SearchResult("", null, attributes)
    (ldapContext.search(_: String, _: String, _: SearchControls)) expects(*, *, *) returning result
  }
}
