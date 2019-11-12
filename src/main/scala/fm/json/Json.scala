/*
 * Copyright 2019 Frugal Mechanic (http://frugalmechanic.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fm.json

import com.fasterxml.jackson.core.io.{CharacterEscapes, SerializedString}
import com.fasterxml.jackson.core.json.JsonReadFeature
import com.fasterxml.jackson.core.util.{DefaultIndenter, DefaultPrettyPrinter}
import com.fasterxml.jackson.core.{JsonFactory, JsonFactoryBuilder, JsonGenerator, JsonParser, JsonToken, SerializableString}
import fm.common.Implicits._
import java.io.StringWriter

object Json {
  val jsonFactory: JsonFactory = {
    val builder: JsonFactoryBuilder = new JsonFactoryBuilder()

    builder.enable(JsonReadFeature.ALLOW_BACKSLASH_ESCAPING_ANY_CHARACTER)
    builder.enable(JsonReadFeature.ALLOW_JAVA_COMMENTS)
    builder.enable(JsonReadFeature.ALLOW_LEADING_ZEROS_FOR_NUMBERS)
    //builder.enable(JsonReadFeature.ALLOW_MISSING_VALUES)      // Not currently enabled
    //builder.enable(JsonReadFeature.ALLOW_NON_NUMERIC_NUMBERS) // Not currently enabled
    builder.enable(JsonReadFeature.ALLOW_SINGLE_QUOTES)
    builder.enable(JsonReadFeature.ALLOW_TRAILING_COMMA)
    builder.enable(JsonReadFeature.ALLOW_UNESCAPED_CONTROL_CHARS)
    builder.enable(JsonReadFeature.ALLOW_UNQUOTED_FIELD_NAMES)
    builder.enable(JsonReadFeature.ALLOW_YAML_COMMENTS)

    builder.build()
  }

  jsonFactory.setCharacterEscapes(new CustomCharacterEscapes())

  val jsonPrettyPrinter: DefaultPrettyPrinter = {
    new DefaultPrettyPrinter()
      .withObjectIndenter(DefaultIndenter.SYSTEM_LINEFEED_INSTANCE)
      .withArrayIndenter(DefaultIndenter.SYSTEM_LINEFEED_INSTANCE)
      .withSpacesInObjectEntries()
  }

  /**
   * The JSON Spec seems to say '/' should be escaped however Jackson does not do this by default so let's add this
   * so we can more easily compare fm.serializer.json to fm.serializer.jackson
   */
  private class CustomCharacterEscapes extends CharacterEscapes {
    private val asciiEscapes: Array[Int] = CharacterEscapes.standardAsciiEscapesForJSON()
    asciiEscapes('/') = CharacterEscapes.ESCAPE_CUSTOM

    private val customEscape: SerializedString = new SerializedString("\\/")

    def getEscapeCodesForAscii(): Array[Int] = asciiEscapes
    def getEscapeSequence(i: Int): SerializableString = if (i === ('/': Int)) customEscape else null
  }

  def toCompactJsonString(json: String): String = toCompactJsonString(jsonFactory.createParser(json))

  def toCompactJsonString(parser: JsonParser): String = {
    val sw: StringWriter = new StringWriter()
    val generator: JsonGenerator = jsonFactory.createGenerator(sw)
    pipe(parser, generator)
    generator.close()
    sw.toString
  }

  /**
   * Pipe a JsonParser to a JsonGenerator
   * @param parser
   * @param generator
   */
  def pipe(parser: JsonParser, generator: JsonGenerator): Unit = {
    var token: JsonToken = if (!parser.hasCurrentToken) parser.nextToken() else parser.currentToken()

    while (null != token) {
      token match {
        case JsonToken.VALUE_NULL => generator.writeNull()
        case JsonToken.VALUE_TRUE => generator.writeBoolean(true)
        case JsonToken.VALUE_FALSE => generator.writeBoolean(false)
        case JsonToken.VALUE_STRING => if (parser.hasTextCharacters) generator.writeString(parser.getTextCharacters, parser.getTextOffset, parser.getTextLength) else generator.writeString(parser.getText)
        case JsonToken.VALUE_NUMBER_INT | JsonToken.VALUE_NUMBER_FLOAT =>
          parser.getNumberType match {
            case JsonParser.NumberType.BIG_DECIMAL | JsonParser.NumberType.DOUBLE | JsonParser.NumberType.FLOAT => generator.writeNumber(parser.getDecimalValue)
            case JsonParser.NumberType.BIG_INTEGER => generator.writeNumber(parser.getBigIntegerValue)
            case JsonParser.NumberType.INT => generator.writeNumber(parser.getIntValue)
            case JsonParser.NumberType.LONG => generator.writeNumber(parser.getLongValue)
          }
        case JsonToken.START_ARRAY => generator.writeStartArray()
        case JsonToken.START_OBJECT => generator.writeStartObject()
        case JsonToken.END_ARRAY => generator.writeEndArray()
        case JsonToken.END_OBJECT => generator.writeEndObject()
        case JsonToken.FIELD_NAME => generator.writeFieldName(parser.getCurrentName)
        case JsonToken.VALUE_EMBEDDED_OBJECT | JsonToken.NOT_AVAILABLE => throw new IllegalStateException(s"Unexpected JsonToken: $token")
      }

      token = parser.nextToken()
    }
  }
}
