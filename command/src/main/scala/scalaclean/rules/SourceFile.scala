package scalaclean.rules

import java.nio.charset.Charset
import java.nio.file.Files

import scalaclean.model.SourceModel
import scalaclean.util.DocHelper

class SourceFile(val file: SourceModel) {
  lazy val content = new String(Files.readAllBytes(file.filename), Charset.forName(file.encoding))
  lazy val syntacticDocument = DocHelper.readSyntacticDoc(file, content)
  lazy val tokenArray = syntacticDocument.tokens.tokens
}
