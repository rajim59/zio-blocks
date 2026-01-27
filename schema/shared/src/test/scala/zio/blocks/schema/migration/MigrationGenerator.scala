package zio.blocks.schema.migration

import zio.test._
import zio.blocks.schema._
import zio.blocks.schema.migration.MigrationAction._
import zio.blocks.schema.migration.SchemaExpr

object MigrationGenerator {

  val genName: Gen[Any, String] =
    Gen.int(1, 10).flatMap(n => Gen.stringN(n)(Gen.alphaNumericChar))

  val anyPrimitive: Gen[Any, DynamicValue] = Gen.oneOf(
    Gen
      .int(0, 20)
      .flatMap(n => Gen.stringN(n)(Gen.alphaNumericChar))
      .map(s => DynamicValue.Primitive(PrimitiveValue.String(s))),
    Gen.int.map(i => DynamicValue.Primitive(PrimitiveValue.Int(i))),
    Gen.boolean.map(b => DynamicValue.Primitive(PrimitiveValue.Boolean(b)))
  )

  val anyRecord: Gen[Any, DynamicValue] =
    Gen
      .int(1, 10)
      .flatMap(n => Gen.vectorOfN(n)(genName.zip(anyPrimitive)))
      .map(fields => DynamicValue.Record(fields.toVector))

  // Helper to create optic safely
  private def fieldOptic(name: String): DynamicOptic = 
    DynamicOptic(Vector(DynamicOptic.Node.Field(name)))

  val anyAction: Gen[Any, MigrationAction] = Gen.oneOf(
    genName.zip(genName).map { case (f, t) =>
      Rename(fieldOptic(f), t)
    },
    genName.zip(anyPrimitive).map { case (n, v) =>
      AddField(fieldOptic(n), SchemaExpr.Constant(v))
    }
  )

  /* 
   * REMOVED: anyDynamicMigration
   * REASON: DynamicMigration structure differs between Scala 2 (List) and Scala 3 (Tuple).
   * Generating random migrations at runtime breaks the compile-time type safety guarantees 
   * required by the new architecture. Tests should build migrations deterministically 
   * or test individual actions.
   */
}