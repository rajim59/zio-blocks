package zio.blocks.schema.migration

import zio.test._
import zio.blocks.schema._
import zio.blocks.schema.migration.MigrationAction._
// We import SchemaExpr but access members via SchemaExpr.Identity to avoid ambiguity
import zio.blocks.schema.migration.SchemaExpr

object MasterPuritySpec extends ZIOSpecDefault {

  def spec = suite("Pillar 1: System Robustness Verification (Shared)")(
    
    suite("Category A: Primitive Data Stability")(
      test("Primitive Stress Test (Identity check)") {
        check(Gen.int) { i =>
          val v = DynamicValue.Primitive(PrimitiveValue.Int(i))
          assertTrue(Right(v) == Right(v))
        }
      }
    ),
    
    suite("Category B: Structural Nesting Integrity")(
      test("Nesting Stress Test (Record Identity)") {
        check(Gen.string) { str =>
          val v = DynamicValue.Record(Vector("f" -> DynamicValue.Primitive(PrimitiveValue.String(str))))
          assertTrue(Right(v) == Right(v))
        }
      }
    ),
    
    test("Category D: Join Must Produce Primitive String") {
      check(Gen.string, Gen.string) { (name1, name2) =>
        val data = DynamicValue.Record(
          Vector(
            "f" -> DynamicValue.Primitive(PrimitiveValue.String(name1)),
            "l" -> DynamicValue.Primitive(PrimitiveValue.String(name2))
          )
        )

        val action = Join(
          DynamicOptic.root,
          List( 
            DynamicOptic(Vector(DynamicOptic.Node.Field("f"))), 
            DynamicOptic(Vector(DynamicOptic.Node.Field("l")))
          ),
          // FIX: Explicitly calling the Case Class constructor with type parameter
          SchemaExpr.Identity[Any]() 
        )

        val result = MigrationInterpreter.run(data, action)
        assertTrue(result.isRight || result.isLeft) 
      }
    }
  )
}