package zio.blocks.schema.migration

import zio.test._
import zio.blocks.schema._
import zio.blocks.schema.migration.MigrationAction._
import zio.blocks.schema.migration.SchemaExpr
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream, ObjectStreamClass}
import scala.annotation.unused

/**
 * Requirement Verification: Pure Data & Serialization
 */
object PureDataSpec extends ZIOSpecDefault {

  case class UserV1(name: String, age: Int)
  case class UserV2(fullName: String, age: Int, active: Boolean)

  @unused implicit val schemaV1: Schema[UserV1] = null.asInstanceOf[Schema[UserV1]]
  @unused implicit val schemaV2: Schema[UserV2] = null.asInstanceOf[Schema[UserV2]]

  // @nowarn("msg=unused")
  implicit val conversion: scala.languageFeature.implicitConversions = scala.language.implicitConversions

  def spec = suite("Requirement: Pure Data, Serialization & Introspection")(
    test("1. No Closures & Serialization: Migration must be binary serializable") {
      val migration = MigrationBuilder
        .make[UserV1, UserV2]
        .renameField((u: UserV1) => u.name, (u: UserV2) => u.fullName)
        .addField((u: UserV2) => u.active, SchemaExpr.Constant(DynamicValue.Primitive(PrimitiveValue.Boolean(true))))
        .build

      val pureData = migration.dynamicMigration

      // Step A: Serialize
      val resultTry = scala.util.Try {
        val stream = new ByteArrayOutputStream()
        val oos    = new ObjectOutputStream(stream)
        oos.writeObject(pureData)
        oos.close()
        stream.toByteArray
      }

      // Step B: Deserialize (With ClassLoader Fix)
      val deserializedTry = resultTry.flatMap { bytes =>
        scala.util.Try {
          // [FIX] Custom ObjectInputStream to handle SBT/ZIO Test ClassLoaders
          val ois = new ObjectInputStream(new ByteArrayInputStream(bytes)) {
            override def resolveClass(desc: ObjectStreamClass): Class[_] =
              try {
                Class.forName(desc.getName, false, Thread.currentThread().getContextClassLoader)
              } catch {
                case _: ClassNotFoundException => super.resolveClass(desc)
              }
          }
          val obj = ois.readObject().asInstanceOf[DynamicMigration]
          ois.close()
          obj
        }
      }

      assertTrue(resultTry.isSuccess) &&
      assertTrue(deserializedTry.isSuccess) &&
      assertTrue(deserializedTry.get == pureData)
    },

    test("2. Introspection: Can inspect data to generate SQL DDL (Offline)") {
      val migration = MigrationBuilder
        .make[UserV1, UserV2]
        .renameField((u: UserV1) => u.name, (u: UserV2) => u.fullName)
        .addField((u: UserV2) => u.active, SchemaExpr.Constant(DynamicValue.Primitive(PrimitiveValue.Boolean(true))))
        .build

      def generateSQL(actions: Vector[MigrationAction]): List[String] =
        actions.toList.collect {
          case Rename(at, to) =>
            val table = "users"
            val col   = at.nodes.last.asInstanceOf[DynamicOptic.Node.Field].name
            s"ALTER TABLE $table RENAME COLUMN $col TO $to;"

          case AddField(at, _) =>
            val table = "users"
            val col   = at.nodes.last.asInstanceOf[DynamicOptic.Node.Field].name
            s"ALTER TABLE $table ADD COLUMN $col BOOLEAN DEFAULT TRUE;"
        }

      val sqlStatements = generateSQL(migration.dynamicMigration.actions)

      assertTrue(sqlStatements.contains("ALTER TABLE users RENAME COLUMN name TO fullName;")) &&
      assertTrue(sqlStatements.contains("ALTER TABLE users ADD COLUMN active BOOLEAN DEFAULT TRUE;"))
    },

    test("3. Invertibility: Can derive 'Downgrader' from 'Upgrader' data") {
      val upgrader = MigrationBuilder
        .make[UserV1, UserV2]
        .renameField((u: UserV1) => u.name, (u: UserV2) => u.fullName)
        .buildPartial

      def deriveDowngrade(actions: Vector[MigrationAction]): Vector[MigrationAction] =
        actions.reverse.map {
          case Rename(at, to) =>
            val originalName = at.nodes.last.asInstanceOf[DynamicOptic.Node.Field].name
            val newPath      = DynamicOptic(Vector(DynamicOptic.Node.Field(to)))
            Rename(newPath, originalName)
          case _ => throw new RuntimeException("Not implemented for test")
        }

      val downgraderActions = deriveDowngrade(upgrader.dynamicMigration.actions)

      assertTrue(downgraderActions.head match {
        case Rename(from, to) =>
          from.nodes.last == DynamicOptic.Node.Field("fullName") && to == "name"
        case _ => false
      })
    }
  )
}
