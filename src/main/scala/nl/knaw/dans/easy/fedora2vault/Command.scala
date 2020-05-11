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

import java.io.FileWriter
import java.util.UUID

import better.files.{ Dispose, File }
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.language.reflectiveCalls
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

object Command extends App with DebugEnhancedLogging {
  type FeedBackMessage = String

  val configuration = Configuration(File(System.getProperty("app.home")))
  val commandLine: CommandLineOptions = new CommandLineOptions(args, configuration) {
    verify()
  }
  val app = new EasyFedora2vaultApp(configuration)

  runSubcommand(app)
    .doIfSuccess(msg => println(s"OK: $msg"))
    .doIfFailure { case e => logger.error(e.getMessage, e) }
    .doIfFailure { case NonFatal(e) => println(s"FAILED: ${ e.getMessage }") }

  private def runSubcommand(app: EasyFedora2vaultApp): Try[FeedBackMessage] = {
    val outputDir = commandLine.outputDir()
    val logWriter: FileWriter = commandLine.logFile().newFileWriter(append = true)
    new Dispose(CsvRecord.csvFormat.print(logWriter)).apply { implicit printer =>
      val xs = commandLine.datasetId
        .map(app.simpleTransform(outputDir / UUID.randomUUID().toString))
        .getOrElse(app.simpleTransForms(outputDir, commandLine.inputFile()))
      xs.foreach(_.foreach(_.print))
      xs.left.map(t => Failure(t))
        .getOrElse(Success(s"""All datasets in ${ commandLine.inputFile() }
                              | saved as bags in $outputDir""".stripMargin))
    }
  }
}
