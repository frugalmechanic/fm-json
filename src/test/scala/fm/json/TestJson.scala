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

import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
import java.io.StringWriter
import java.math.{BigDecimal, BigInteger}
import org.scalatest.{AppendedClues, FunSuite, Matchers}

final class TestJson extends FunSuite with Matchers with AppendedClues {
  private val jsonObjectString: String =
    """
      |{
      |  "null" : null,
      |  "true" : true,
      |  "false" : false,
      |  "string" : "string",
      |  "intZero" : 0,
      |  "intMin" : -2147483648,
      |  "intMax" : 2147483647,
      |  "longMin" : -9223372036854775808,
      |  "longMax" : 9223372036854775807,
      |  "bigInteger" : 92233720368547758079223372036854775807,
      |  "floatMinPositive" : 1.4E-45,
      |  "floatMin" : -3.4028235E+38,
      |  "floatMax" : 3.4028235E+38,
      |  "doubleMinPositive" : 4.9E-324,
      |  "doubleMin" : -1.7976931348623157E+308,
      |  "doubleMax" : 1.7976931348623157E+308,
      |  "bigDecimal" : 92233720368547758079223372036854775807.92233720368547758079223372036854775807,
      |  "object" : {
      |    "nestedString" : "testing 123",
      |    "nestedArray" : [
      |      {
      |        "foo" : "bar",
      |        "field" : 123.456,
      |        "array" : [
      |          "one",
      |          2,
      |          3.3333333
      |        ],
      |        "null" : null,
      |        "false" : false,
      |        "intZero" : 0,
      |        "emptyObject" : { },
      |        "emptyArray" : [ ]
      |      },
      |      "two",
      |      3.3
      |    ]
      |  },
      |  "intArray" : [
      |    1,
      |    2,
      |    3
      |  ],
      |  "stringArray" : [
      |    "one",
      |    "two",
      |    "three"
      |  ],
      |  "mixedArray" : [
      |    1,
      |    "two",
      |    3.3
      |  ],
      |  "unicode" : "Hello \r\t\n \\ \/ \" \b\f""".stripMargin.trim + " oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  " + """World!"
      |}
      |""".stripMargin.trim

  private val jsonObjectNode: JsonObject = {
    import Implicits._

    JsonObject(
      "null" -> JsonNull,
      "true" -> true,
      "false" -> false,
      "string" -> "string",
      "intZero" -> 0,
      "intMin" -> Int.MinValue,
      "intMax" -> Int.MaxValue,
      "longMin" -> Long.MinValue,
      "longMax" -> Long.MaxValue,
      "bigInteger" -> new BigInteger("92233720368547758079223372036854775807"),
      "floatMinPositive" -> new BigDecimal("1.4E-45"),
      "floatMin" -> new BigDecimal("-3.4028235E+38"),
      "floatMax" -> new BigDecimal("3.4028235E+38"),
      "doubleMinPositive" -> new BigDecimal("4.9E-324"),
      "doubleMin" -> new BigDecimal("-1.7976931348623157E+308"),
      "doubleMax" -> new BigDecimal("1.7976931348623157E+308"),
      "bigDecimal" -> new BigDecimal("92233720368547758079223372036854775807.92233720368547758079223372036854775807"),
      "object" -> JsonObject(
        "nestedString" -> "testing 123",
        "nestedArray" -> JsonArray(
          JsonObject(
            "foo" -> "bar",
            "field" -> new BigDecimal("123.456"),
            "array" -> JsonArray("one", 2, new BigDecimal("3.3333333")),
            "null" -> JsonNull,
            "false" -> false,
            "intZero" -> 0,
            "emptyObject" -> JsonObject.empty,
            "emptyArray" -> JsonArray.empty
          ),
          "two",
          new BigDecimal("3.3")
        )
      ),
      "intArray" -> JsonArray(1, 2, 3),
      "stringArray" -> JsonArray("one", "two", "three"),
      "mixedArray" -> JsonArray(1, "two", new BigDecimal("3.3")),
      "unicode" -> "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!"
    )
  }

  private val jsonObjectStringMinimal: String =
    """
      |{
      |  "true" : true,
      |  "string" : "string",
      |  "intMin" : -2147483648,
      |  "intMax" : 2147483647,
      |  "longMin" : -9223372036854775808,
      |  "longMax" : 9223372036854775807,
      |  "bigInteger" : 92233720368547758079223372036854775807,
      |  "floatMinPositive" : 1.4E-45,
      |  "floatMin" : -3.4028235E+38,
      |  "floatMax" : 3.4028235E+38,
      |  "doubleMinPositive" : 4.9E-324,
      |  "doubleMin" : -1.7976931348623157E+308,
      |  "doubleMax" : 1.7976931348623157E+308,
      |  "bigDecimal" : 92233720368547758079223372036854775807.92233720368547758079223372036854775807,
      |  "object" : {
      |    "nestedString" : "testing 123",
      |    "nestedArray" : [
      |      {
      |        "foo" : "bar",
      |        "field" : 123.456,
      |        "array" : [
      |          "one",
      |          2,
      |          3.3333333
      |        ]
      |      },
      |      "two",
      |      3.3
      |    ]
      |  },
      |  "intArray" : [
      |    1,
      |    2,
      |    3
      |  ],
      |  "stringArray" : [
      |    "one",
      |    "two",
      |    "three"
      |  ],
      |  "mixedArray" : [
      |    1,
      |    "two",
      |    3.3
      |  ],
      |  "unicode" : "Hello \r\t\n \\ \/ \" \b\f""".stripMargin.trim + " oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  " + """World!"
      |}
      |""".stripMargin.trim

  private val jsonObjectNodeMinimal: JsonObject = {
    import Implicits._

    JsonObject(
      "true" -> true,
      "string" -> "string",
      "intMin" -> Int.MinValue,
      "intMax" -> Int.MaxValue,
      "longMin" -> Long.MinValue,
      "longMax" -> Long.MaxValue,
      "bigInteger" -> new BigInteger("92233720368547758079223372036854775807"),
      "floatMinPositive" -> new BigDecimal("1.4E-45"),
      "floatMin" -> new BigDecimal("-3.4028235E+38"),
      "floatMax" -> new BigDecimal("3.4028235E+38"),
      "doubleMinPositive" -> new BigDecimal("4.9E-324"),
      "doubleMin" -> new BigDecimal("-1.7976931348623157E+308"),
      "doubleMax" -> new BigDecimal("1.7976931348623157E+308"),
      "bigDecimal" -> new BigDecimal("92233720368547758079223372036854775807.92233720368547758079223372036854775807"),
      "object" -> JsonObject(
        "nestedString" -> "testing 123",
        "nestedArray" -> JsonArray(
          JsonObject(
            "foo" -> "bar",
            "field" -> new BigDecimal("123.456"),
            "array" -> JsonArray("one", 2, new BigDecimal("3.3333333"))
          ),
          "two",
          new BigDecimal("3.3")
        )
      ),
      "intArray" -> JsonArray(1, 2, 3),
      "stringArray" -> JsonArray("one", "two", "three"),
      "mixedArray" -> JsonArray(1, "two", new BigDecimal("3.3")),
      "unicode" -> "Hello \r\t\n \\ / \" \b\f oneByte: \u0024 twoByte: \u00A2 threeByte: \u20AC fourByteSupplementary: \uD83D\uDCA5  World!"
    )
  }

  test("null") {
    checkParse("null", JsonNull, JsonNull)
  }

  test("true") {
    checkParse("true", JsonTrue, JsonTrue)

    JsonTrue.parse("true") shouldBe true
    JsonTrue.parse("\"true\"") shouldBe true
    JsonTrue.parse("1") shouldBe true

    JsonTrue.tryParse("false") shouldBe None
    JsonTrue.tryParse("\"foo\"") shouldBe None
    JsonTrue.tryParse("123") shouldBe None
    JsonTrue.tryParse("0") shouldBe None
  }

  test("false") {
    checkParse("false", JsonFalse, JsonFalse)

    JsonFalse.parse("false") shouldBe false
    JsonFalse.parse("\"false\"") shouldBe false
    JsonFalse.parse("0") shouldBe false

    JsonFalse.tryParse("true") shouldBe None
    JsonFalse.tryParse("\"foo\"") shouldBe None
    JsonFalse.tryParse("123") shouldBe None
    JsonFalse.tryParse("1") shouldBe None
  }

  test("boolean") {
    JsonBoolean.parse("false") shouldBe false
    JsonBoolean.parse("\"false\"") shouldBe false
    JsonBoolean.parse("0") shouldBe false

    JsonBoolean.parse("true") shouldBe true
    JsonBoolean.parse("\"true\"") shouldBe true
    JsonBoolean.parse("1") shouldBe true

    JsonBoolean.tryParse("\"foo\"") shouldBe None
    JsonBoolean.tryParse("123") shouldBe None
  }

  test("string") {
    checkParse("\"string\"", JsonString("string"), JsonString)

    JsonString.parse("\"123\"") shouldBe "123"
    JsonString.parse("123") shouldBe "123"
    JsonString.parse("true") shouldBe "true"
    JsonString.parse("null") shouldBe ""
  }

  test("int") {
    checkParse("123", JsonInt(123), JsonInt)
    checkParse("-2147483648", JsonInt(Int.MinValue), JsonInt)
    checkParse("2147483647", JsonInt(Int.MaxValue), JsonInt)

    JsonInt.parse("2147483647.000") shouldBe 2147483647

    JsonInt.tryParse("9223372036854775807") shouldBe None
    JsonInt.tryParse("9223372036854775807.000") shouldBe None
  }

  test("long") {
    checkParse("-9223372036854775808", JsonLong(Long.MinValue), JsonLong)
    checkParse("9223372036854775807", JsonLong(Long.MaxValue), JsonLong)

    JsonLong.parse("9223372036854775807.000") shouldBe 9223372036854775807L

    JsonLong.tryParse("92233720368547758079223372036854775807") shouldBe None
  }

  test("bigInteger") {
    checkParse("92233720368547758079223372036854775807", JsonBigInteger(new BigInteger("92233720368547758079223372036854775807")), JsonBigInteger)

    JsonBigInteger.parse("123") shouldBe BigInteger.valueOf(123)
    JsonBigInteger.parse("123.0") shouldBe BigInteger.valueOf(123)
  }

  test("float -- parsed as BigDecimal") {
    checkParse("1.23", JsonBigDecimal(BigDecimal.valueOf(1.23d)), JsonBigDecimal)
    checkParse("1.4E-45", JsonBigDecimal(new BigDecimal(String.valueOf(Float.MinPositiveValue))), JsonBigDecimal)
    checkParse("-3.4028235E+38", JsonBigDecimal(new BigDecimal(String.valueOf(Float.MinValue))), JsonBigDecimal)
    checkParse("3.4028235E+38", JsonBigDecimal(new BigDecimal(String.valueOf(Float.MaxValue))), JsonBigDecimal)

    JsonFloat.parse("1") shouldBe 1f
    JsonFloat.parse("1.23") shouldBe 1.23f
    JsonFloat.parse("\"1.23\"") shouldBe 1.23f
  }

  test("double -- parsed as BigDecimal") {
    checkParse("4.9E-324", JsonBigDecimal(BigDecimal.valueOf(Double.MinPositiveValue)), JsonBigDecimal)
    checkParse("-1.7976931348623157E+308", JsonBigDecimal(BigDecimal.valueOf(Double.MinValue)), JsonBigDecimal)
    checkParse("1.7976931348623157E+308", JsonBigDecimal(BigDecimal.valueOf(Double.MaxValue)), JsonBigDecimal)

    JsonDouble.parse("1") shouldBe 1d
    JsonDouble.parse("1.23") shouldBe 1.23d
    JsonDouble.parse("\"1.23\"") shouldBe 1.23d
  }

  test("bigDecimal") {
    checkParse("92233720368547758079223372036854775807.92233720368547758079223372036854775807", JsonBigDecimal(new BigDecimal("92233720368547758079223372036854775807.92233720368547758079223372036854775807")), JsonBigDecimal)
  }

  test("object - JsonOptions.default") {
    checkParse(jsonObjectString, jsonObjectNode, JsonObject, JsonOptions.default)
  }

  test("object - JsonOptions.minimal") {
    JsonNode.parse(jsonObjectString, JsonOptions.minimal) shouldBe jsonObjectNodeMinimal
    jsonObjectNode.toJson(JsonOptions.minimal.copy(prettyFormat = true)) shouldBe jsonObjectStringMinimal
  }

  test("object - concatenation") {
    val oneA: (String, JsonInt) = ("one", JsonInt(1))
    val twoA: (String, JsonInt) = ("two", JsonInt(2))
    val threeA: (String, JsonInt) = ("three", JsonInt(3))

    val oneB: (String, JsonInt) = ("one", JsonInt(11))
    val twoB: (String, JsonInt) = ("two", JsonInt(22))
    val threeB: (String, JsonInt) = ("three", JsonInt(33))

    JsonObject(oneA, twoA, threeA) ++ JsonObject() shouldBe JsonObject(oneA, twoA, threeA)
    JsonObject(oneA, twoA, threeA) ++ JsonObject(twoB) shouldBe JsonObject(oneA, twoB, threeA)
    JsonObject(oneA, twoA, threeA) ++ JsonObject(threeB, oneB, twoB) shouldBe JsonObject(oneB, twoB, threeB)

    JsonObject(oneA, twoA, threeA) :+ twoB shouldBe JsonObject(oneA, twoB, threeA)
  }

  test("array") {
    val json: String = """[1, "two", 3.3]"""
    val one: JsonInt = JsonInt(1)
    val two: JsonString = JsonString("two")
    val three: JsonBigDecimal = JsonBigDecimal(new BigDecimal("3.3"))

    val expectedElements: IndexedSeq[JsonNode] = IndexedSeq(one, two, three)
    val expectedNode: JsonArray = JsonArray(expectedElements)

    JsonArray.parse(json) shouldBe expectedElements

    JsonArray(one, two) ++ Vector(three) shouldBe expectedNode
    JsonArray(one, two) ++ JsonArray(three) shouldBe expectedNode
    JsonArray(one, two) :+ three shouldBe expectedNode
  }

  test("toCompactJsonString") {
    val json: String =
      """
        |{
        |  "foo" : "bar",
        |  "one" : 2,
        |  "arr" : [
        |    1,
        |    2,
        |    3
        |  ]
        |}
        |""".stripMargin

    Json.toCompactJsonString(json) shouldBe """{"foo":"bar","one":2,"arr":[1,2,3]}"""
  }

  test("JsonNumber.bestFitFor - BigInteger") {
    JsonNumber.bestFitFor(new BigInteger("0")) shouldBe JsonInt(0)
    JsonNumber.bestFitFor(new BigInteger(Int.MinValue.toString)) shouldBe JsonInt(Int.MinValue)
    JsonNumber.bestFitFor(new BigInteger(Int.MaxValue.toString)) shouldBe JsonInt(Int.MaxValue)

    JsonNumber.bestFitFor(new BigInteger(Long.MinValue.toString)) shouldBe JsonLong(Long.MinValue)
    JsonNumber.bestFitFor(new BigInteger(Long.MaxValue.toString)) shouldBe JsonLong(Long.MaxValue)

    JsonNumber.bestFitFor(new BigInteger("92233720368547758079223372036854775807")) shouldBe JsonBigInteger(new BigInteger("92233720368547758079223372036854775807"))
  }

  test("JsonNumber.bestFitFor - BigDecimal") {
    // These are the same as the BigInteger tests:
    JsonNumber.bestFitFor(new BigDecimal("0")) shouldBe JsonInt(0)
    JsonNumber.bestFitFor(new BigDecimal(Int.MinValue.toString)) shouldBe JsonInt(Int.MinValue)
    JsonNumber.bestFitFor(new BigDecimal(Int.MaxValue.toString)) shouldBe JsonInt(Int.MaxValue)

    JsonNumber.bestFitFor(new BigDecimal(Long.MinValue.toString)) shouldBe JsonLong(Long.MinValue)
    JsonNumber.bestFitFor(new BigDecimal(Long.MaxValue.toString)) shouldBe JsonLong(Long.MaxValue)

    JsonNumber.bestFitFor(new BigDecimal("92233720368547758079223372036854775807")) shouldBe JsonBigInteger(new BigInteger("92233720368547758079223372036854775807"))

    // These are additional tests only for BigDecimal:

    JsonNumber.bestFitFor(new BigDecimal("0.0")) shouldBe JsonInt(0)
    JsonNumber.bestFitFor(new BigDecimal("1.23")) shouldBe JsonBigDecimal(new BigDecimal("1.23"))
    JsonNumber.bestFitFor(new BigDecimal("92233720368547758079223372036854775807.92233720368547758079223372036854775807")) shouldBe JsonBigDecimal(new BigDecimal("92233720368547758079223372036854775807.92233720368547758079223372036854775807"))
  }

  test("JsonBoolean.unapply") {
    (JsonNull: JsonNode) match {
      case JsonBoolean(value) => assert(false)
      case _ =>
    }

    JsonTrue match {
      case JsonBoolean(value) => value shouldBe true
      case _ => assert(false)
    }

    JsonFalse match {
      case JsonBoolean(value) => value shouldBe false
      case _ => assert(false)
    }
  }

  private def checkParse(
    s: String,
    expected: JsonNode,
    parseFactory: JsonNodeParseFactory[_,_],
    options: JsonOptions = JsonOptions.default
  ): Unit = {
    val node: JsonNode = JsonNode.parse(s)

    node shouldBe expected
    node.toPrettyJson() shouldBe s
    JsonNode.parse(JsonNodeParser(node), options) shouldBe expected
    JsonNode.parse(expected.toPrettyJson()) shouldBe expected
    JsonNode.parse(expected.toCompactJson()) shouldBe expected

    parseFactory.parseNode(s) shouldBe node

    if (node.isJsonValue) Json.toCompactJsonString(s) shouldBe s
    Json.toCompactJsonString(s) shouldBe node.toCompactJson()
    node.toCompactJson() shouldBe node.toString()

    checkPipe(JsonNodeParser(expected), s)
    checkPipe(Json.jsonFactory.createParser(s), s)

    checkGenerator(node)
    checkGenerator(expected)

    checkReRead(s, expected, parseFactory)

    checkMinimal(s)
  } withClue(s"checkParse($s, $expected)")

  private def checkPipe(parser: JsonParser, expected: String): Unit = {
    val sw: StringWriter = new StringWriter()
    val generator: JsonGenerator = Json.jsonFactory.createGenerator(sw)
    generator.setPrettyPrinter(Json.jsonPrettyPrinter)
    Json.pipe(parser, generator)
    generator.close()
    sw.toString shouldBe expected
  } withClue s"checkPipe($expected, $expected)"

  private def checkGenerator(node: JsonNode): Unit = {
    val generator: JsonNodeGenerator = new JsonNodeGenerator()
    node.write(generator, JsonOptions.default)
    generator.result shouldBe node
  } withClue s"checkGenerator($node)"

  private def checkReRead(s: String, expected: JsonNode, parseFactory: JsonNodeParseFactory[_,_]): Unit = {
    val parser: JsonParser = Json.jsonFactory.createParser(s)

    parseFactory.parseNode(parser, JsonOptions.default) shouldBe expected
    parseFactory.tryParse(parser) shouldBe None
  } withClue(s"checkReRead($s, $expected, $parseFactory)")

  private def checkMinimal(s: String): Unit = {
    val node: JsonNode = JsonNode.parse(s, JsonOptions.default)
    val minimalNode: JsonNode = JsonNode.parse(s, JsonOptions.minimal)

    // Sanity check the JsonNodeParser logic works properly
    minimalNode shouldBe JsonNode.parse(JsonNodeParser(node), JsonOptions.minimal)

    node.toJson(JsonOptions.minimal) shouldBe minimalNode.toJson(JsonOptions.default)

  } withClue(s"checkMinimal($s)")


  test("") {
    JsonObject(
      "key" -> JsonValue("asd"),
      "key" -> JsonValue(123),
      "key" -> JsonValue(false),
      "key" -> JsonValue(false)
    )
  }
}
