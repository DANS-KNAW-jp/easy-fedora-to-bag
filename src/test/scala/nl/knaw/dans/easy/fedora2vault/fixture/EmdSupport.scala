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
package nl.knaw.dans.easy.fedora2vault.fixture

import com.typesafe.scalalogging.Logger
import nl.knaw.dans.easy.fedora2vault._
import nl.knaw.dans.pf.language.emd.EasyMetadataImpl
import nl.knaw.dans.pf.language.emd.binding.EmdUnmarshaller
import org.scalatest.Assertions._

import scala.util.Try
import scala.xml.{ Elem, NodeSeq }
import nl.knaw.dans.lib.error._
trait EmdSupport {
  private val emdUnmarshaller = new EmdUnmarshaller(classOf[EasyMetadataImpl])

  def parseEmdContent(xml: NodeSeq): EasyMetadataImpl = {
    val emd = <emd:easymetadata xmlns:emd="http://easy.dans.knaw.nl/easy/easymetadata/"
                          xmlns:eas="http://easy.dans.knaw.nl/easy/easymetadata/eas/"
                          xmlns:dct="http://purl.org/dc/terms/"
                          xmlns:dc="http://purl.org/dc/elements/1.1/"
                          emd:version="0.1"
        >{ xml }</emd:easymetadata>.serialize
    Try(emdUnmarshaller.unmarshal(emd))
      .getOrRecover(e => fail("could not load test EMD", e))
  }

  def emd2ddm(emd: EasyMetadataImpl)(implicit logger: Logger): Elem = DDM(emd, Seq.empty)
    .getOrRecover(e => fail("could not create DDM from test EMD", e))
}
