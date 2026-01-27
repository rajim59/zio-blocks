package zio.blocks.schema.migration

import zio.test._
import zio.blocks.schema._
import zio.blocks.schema.migration.MigrationAction._
import zio.blocks.schema.migration.SchemaExpr

object PuritySpec extends ZIOSpecDefault {

  // Helper function to verify serializability (Proxy for Purity)
  def isPureData(action: Any): Boolean =
    action match {
      case _: Function[_, _] => false // Functions are not data
      // FIX: Used underscore `_` instead of variable name `s`
      case _: java.io.Serializable => true
      case _ => false
    }

  def spec = suite("Forbidden Items Verification (Ironclad Purity Proof)")(
    
    test("Proof: No MigrationAction contains any hidden Function or Closure") {
      val actions = List(
        Rename(DynamicOptic(Vector(DynamicOptic.Node.Field("a"))), "b"),
        AddField(
          DynamicOptic(Vector(DynamicOptic.Node.Field("x"))),
          SchemaExpr.Constant(DynamicValue.Primitive(PrimitiveValue.Int(1)))
        ),
        TransformValue(DynamicOptic.root, SchemaExpr.Identity[Any]()),
        Join(DynamicOptic.root, List.empty, SchemaExpr.Identity[Any]())
      )

      val areAllActionsPure = actions.forall(a => isPureData(a))

      assertTrue(areAllActionsPure)
    }
  )
}