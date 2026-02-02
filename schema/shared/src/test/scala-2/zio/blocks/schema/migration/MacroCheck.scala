package zio.blocks.schema.migration

import zio.blocks.schema.Schema

object MacroCheck {
  // ১. স্যাম্পল মডেল
  case class User(name: String, age: Int)
  implicit val userSchema: Schema[User] = Schema.derived

  // ২. মাইগ্রেশন বিল্ডার টেস্ট (সঠিক পাথ)
  val migration = MigrationBuilder.make[User, User]
    .renameField(_.name, _.name) // সঠিক ফিল্ড, তাই কম্পাইল হওয়া উচিত
}