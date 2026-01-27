package zio.blocks.schema.migration

import zio.test._
import zio.blocks.schema._

object StructuralAndMacroSpec extends ZIOSpecDefault {

  // --- Domain Models ---
  case class UserV1(name: String)
  case class UserV2(fullName: String)
  
  // Scala 3 Structural / Intersection Types
  trait PaymentMethodBase
  type OldCreditCard = PaymentMethodBase & {
    type Tag = "CreditCard"
    def number: String
  }
  
  trait PathWrapper      { def when[T]: T          }
  trait PaymentProcessor { def method: PathWrapper }

  // --- Implicits ---
  implicit val userV1Schema: Schema[UserV1] = Schema.derived
  implicit val userV2Schema: Schema[UserV2] = Schema.derived
  // Fake schema casting for traits
  implicit val pmSchema: Schema[PaymentProcessor] = userV1Schema.asInstanceOf[Schema[PaymentProcessor]]

  def spec = suite("Structural & Macro Compliance (Scala 3)")(
    
    // Test Group 1: Builder DSL & Type Safety
    suite("Builder DSL")(
      test("compiles valid migration steps") {
        val migration = MigrationBuilder
          .make[UserV1, UserV2]
          .renameField((u: UserV1) => u.name, (u: UserV2) => u.fullName)
          .build

        // Scala 3 Tuple Size Check
        assertTrue(migration.dynamicMigration.actions.size == 1)
      },

      test("macro derives correct path for intersection types") {
        val optic = ToDynamicOptic.derive((p: PaymentProcessor) => p.method.when[OldCreditCard])
        
        // Verifying the path string contains expected segments
        val pathStr = optic.apply().toString
        assertTrue(pathStr.contains(".method") && pathStr.contains("OldCreditCard"))
      }
    ),
    
    // Test Group 2: Compile-Time Verification
    suite("Compile-Time Verification")(
      test("identity migration compiles without steps") {
        val m = MigrationBuilder.make[UserV1, UserV1].build
        assertTrue(m.dynamicMigration.actions.size == 0)
      }
    )
  )
}