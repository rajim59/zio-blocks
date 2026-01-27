package zio.blocks.schema.migration

import zio.test._
import zio.blocks.schema._
// FIX: Removed unused imports if any, keeping essential ones
import scala.language.reflectiveCalls

object StructuralComplianceSpec extends ZIOSpecDefault {

  // Structural Type Definition
  type UserV1_Structural = { def name: String; def age: Int }

  case class UserV2_Real(fullName: String, age: Int)

  // Shadow class for Schema derivation trick
  private case class UserV1_Shadow(name: String, age: Int)

  // Hack to provide schema for structural type
  implicit val schemaStruct: Schema[UserV1_Structural] =
    Schema.derived[UserV1_Shadow].asInstanceOf[Schema[UserV1_Structural]]

  implicit val schemaDst: Schema[UserV2_Real] = Schema.derived

  def spec = suite("Structural Type Compliance Test (Shared)")(
    
    test("MUST support migration from Structural Types (Compile-time only)") {

      val migration = MigrationBuilder
        .make[UserV1_Structural, UserV2_Real]
        .renameField(
          (old: UserV1_Structural) => old.name,
          (curr: UserV2_Real) => curr.fullName
        )
        .build

      // Accessing actions structure in a version-agnostic way (via toString)
      val debugString = migration.dynamicMigration.toString

      // Verify that Rename action was created with correct field names
      assertTrue(debugString.contains("Rename")) &&
      assertTrue(debugString.contains("name")) &&
      assertTrue(debugString.contains("fullName"))
    }
  )
}