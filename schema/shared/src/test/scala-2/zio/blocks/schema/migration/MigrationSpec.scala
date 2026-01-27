package zio.blocks.schema.migration

import zio.test._
import zio.blocks.schema._

object MigrationSpec extends ZIOSpecDefault {

  // --- Test Data Models ---
  case class UserV1(name: String)
  case class UserV2(fullName: String)
  
  // Implicits
  implicit val userV1Schema: Schema[UserV1] = Schema.derived
  implicit val userV2Schema: Schema[UserV2] = Schema.derived

  def spec = suite("MigrationSpec")(
    
    // Suite 1: Functional Tests (Safe for JVM, JS, and Native)
    suite("Builder DSL")(
      test("compiles valid migration steps") {
        // Using explicit types for Scala 2 compiler stability
        val migration = MigrationBuilder
          .make[UserV1, UserV2]
          .renameField((u: UserV1) => u.name, (u: UserV2) => u.fullName)
          .build

        assertTrue(migration.dynamicMigration.actions.size == 1)
      },

      test("macro derives correct path") {
        val optic = ToDynamicOptic.derive((u: UserV1) => u.name)
        assertTrue(optic.apply().toString.contains("name"))
      }
    )
    
    /**
     * NOTE: The "Safety Laws" suite was removed.
     * Reflection (scala.reflect) is a JVM-only feature. 
     * To support cross-platform (JS/Native), we must avoid runtime reflection.
     */
  )
}